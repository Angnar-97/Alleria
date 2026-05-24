# ============================================================================
# R/data_fetch.R · scraping en vivo de criptodivisas
# ----------------------------------------------------------------------------
# Dos fuentes complementarias, gratuitas y sin API key:
#   1. CoinGecko (httr2)   ->  snapshot multimercado + serie diaria 1h/24h.
#   2. Yahoo Finance (quantmod) -> OHLCV intradía robusto (fallback).
# Las funciones *_raw se memoisean en global.R con TTL de 5 minutos.
# ============================================================================

# ---- CoinGecko: utilidades base ---------------------------------------------
.coingecko_base <- "https://api.coingecko.com/api/v3"

.coingecko_get <- function(path, query = list()) {
  req <- httr2::request(.coingecko_base) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("alleria/2.0 (Angnar)") |>
    httr2::req_retry(
      max_tries = 4,
      backoff   = function(i) min(2 ^ i, 30),
      is_transient = function(resp) {
        s <- httr2::resp_status(resp)
        s == 429 || s >= 500
      }
    ) |>
    httr2::req_timeout(25)

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_is_error(resp)) return(NULL)

  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  if (!nzchar(body)) return(NULL)
  tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE),
           error = function(e) NULL)
}

# ---- Snapshot de mercado -----------------------------------------------------
#' Devuelve un tibble con los precios actuales y métricas de mercado.
#' Para cada moneda del catálogo: precio, change_24h, change_7d, market_cap,
#' volumen 24 h y sparkline de los últimos 7 días.
fetch_market_snapshot_raw <- function(ids = crypto_catalog$id) {
  data <- .coingecko_get("coins/markets", list(
    vs_currency = "usd",
    ids         = paste(ids, collapse = ","),
    order       = "market_cap_desc",
    per_page    = 250,
    page        = 1,
    sparkline   = "true",
    price_change_percentage = "1h,24h,7d,30d"
  ))

  empty <- is.null(data) || length(data) == 0L
  if (!empty && is.data.frame(data) && NROW(data) == 0L) empty <- TRUE
  if (empty) {
    return(.market_fallback(ids))
  }

  # `sparkline_in_7d` puede llegar como data.frame (con simplifyVector) o como
  # lista de listas. Normalizamos a una list-column con vectores numéricos.
  spark <- if (is.data.frame(data$sparkline_in_7d)) {
    lapply(data$sparkline_in_7d$price, function(v) as.numeric(unlist(v)))
  } else if (is.list(data$sparkline_in_7d)) {
    lapply(data$sparkline_in_7d, function(s) as.numeric(unlist(s$price)))
  } else {
    rep(list(numeric(0)), nrow(data))
  }

  tibble::as_tibble(data) |>
    dplyr::transmute(
      id,
      symbol         = toupper(symbol),
      name,
      image,
      price          = current_price,
      market_cap,
      total_volume,
      change_1h      = price_change_percentage_1h_in_currency,
      change_24h     = price_change_percentage_24h_in_currency,
      change_7d      = price_change_percentage_7d_in_currency,
      change_30d     = price_change_percentage_30d_in_currency,
      ath, ath_change_percentage,
      circulating_supply, total_supply,
      sparkline      = spark,
      last_updated   = as.POSIXct(last_updated, tz = "UTC",
                                  format = "%Y-%m-%dT%H:%M:%OSZ")
    )
}

# Si CoinGecko falla (rate-limit o sin red), construimos un snapshot mínimo a
# partir de los CSV locales para mantener la app usable. Los CSV de
# CryptoDataDownload incluyen una primera línea de comentario
# (`https://www.cryptodatadownload.com`) que hay que saltar.
.detect_csv_skip <- function(f, max_peek = 5L) {
  con <- file(f, "r"); on.exit(close(con))
  for (i in seq_len(max_peek)) {
    l <- readLines(con, n = 1, warn = FALSE)
    if (length(l) == 0) return(0L)
    is_comment <- isTRUE(startsWith(l, "https://")) ||
                  isTRUE(startsWith(l, "#"))
    is_data    <- isTRUE(nzchar(l)) && !is_comment
    if (is_data) return(i - 1L)
  }
  0L
}

.market_fallback <- function(ids) {
  files <- file.path("data",
                     paste0(crypto_catalog$symbol[match(ids, crypto_catalog$id)],
                            ".csv"))
  out <- purrr::map2_dfr(ids, files, function(id, f) {
    if (!file.exists(f)) return(NULL)
    df <- tryCatch(
      utils::read.csv(f, skip = .detect_csv_skip(f), header = TRUE,
                      stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || nrow(df) == 0) return(NULL)
    names(df) <- tolower(names(df))
    # Renombramos posibles variantes ("close" presente; algunos archivos
    # llaman al volumen "volume usd" / "volume btc"…)
    if (!"close" %in% names(df))  return(NULL)
    if (!"volume" %in% names(df)) {
      vcand <- names(df)[startsWith(names(df), "volume")]
      if (length(vcand)) df$volume <- df[[vcand[1]]] else df$volume <- NA_real_
    }
    if (!"date" %in% names(df))   df$date <- seq_len(nrow(df))
    last_row <- utils::tail(df, 1)
    tibble::tibble(
      id           = id,
      symbol       = crypto_catalog$symbol[match(id, crypto_catalog$id)],
      name         = crypto_catalog$name[match(id, crypto_catalog$id)],
      image        = NA_character_,
      price        = as.numeric(last_row$close[1]),
      market_cap   = NA_real_,
      total_volume = suppressWarnings(as.numeric(last_row$volume[1])),
      change_1h    = NA_real_,
      change_24h   = NA_real_,
      change_7d    = NA_real_,
      change_30d   = NA_real_,
      ath          = NA_real_,
      ath_change_percentage = NA_real_,
      circulating_supply    = NA_real_,
      total_supply          = NA_real_,
      sparkline    = list(as.numeric(utils::tail(df$close, 168))),
      last_updated = suppressWarnings(as.POSIXct(last_row$date[1], tz = "UTC"))
    )
  })
  out
}

# ---- Histórico CoinGecko -----------------------------------------------------
#' Serie histórica (precio, market_cap, volumen) de una cripto. CoinGecko
#' determina automáticamente la granularidad en función de `days` (intra-hora
#' para ≤1, horaria para ≤90, diaria para >90). El parámetro `interval=daily`
#' está restringido a planes de pago, por eso no se usa.
#' @param id    CoinGecko id (e.g. "bitcoin")
#' @param days  número de días de histórico (1, 7, 14, 30, 90, 180, 365, "max")
fetch_history_coingecko_raw <- function(id, days = 90) {
  data <- .coingecko_get(
    paste("coins", id, "market_chart", sep = "/"),
    list(vs_currency = "usd", days = days)
  )
  if (is.null(data) || is.null(data$prices)) return(NULL)

  prices  <- as.data.frame(data$prices,        stringsAsFactors = FALSE)
  mcaps   <- as.data.frame(data$market_caps,   stringsAsFactors = FALSE)
  volumes <- as.data.frame(data$total_volumes, stringsAsFactors = FALSE)
  names(prices)  <- c("ts", "price")
  names(mcaps)   <- c("ts", "market_cap")
  names(volumes) <- c("ts", "volume")

  joined <- prices |>
    dplyr::left_join(mcaps,   by = "ts") |>
    dplyr::left_join(volumes, by = "ts") |>
    dplyr::mutate(
      date   = as.Date(as.POSIXct(ts / 1000, tz = "UTC",
                                  origin = "1970-01-01")),
      symbol = toupper(crypto_catalog$symbol[match(id, crypto_catalog$id)])
    )

  # Para ventanas <= 90 días CoinGecko devuelve datos horarios. Agrupamos a
  # diaria tomando el último valor del día (close-of-day) para uniformidad
  # entre ventanas y para que los modelos clásicos asuman frequency = 7.
  joined |>
    dplyr::group_by(date, symbol) |>
    dplyr::summarise(
      price      = dplyr::last(price),
      market_cap = dplyr::last(market_cap),
      volume     = dplyr::last(volume),
      .groups    = "drop"
    ) |>
    dplyr::arrange(date) |>
    tibble::as_tibble()
}

# ---- Histórico Yahoo (OHLCV) ------------------------------------------------
#' OHLCV diario desde Yahoo via quantmod. Devuelve un tibble con
#' Date, Open, High, Low, Close, Volume, Adjusted, symbol.
fetch_history_yahoo_raw <- function(yahoo_ticker, from = Sys.Date() - 365,
                                    to = Sys.Date()) {
  if (length(yahoo_ticker) == 0L) return(NULL)
  if (!isTRUE(nzchar(yahoo_ticker[1]))) return(NULL)
  out <- tryCatch(
    suppressWarnings(suppressMessages(
      quantmod::getSymbols(yahoo_ticker, src = "yahoo",
                           from = from, to = to, auto.assign = FALSE)
    )),
    error = function(e) NULL
  )
  if (is.null(out) || NROW(out) == 0) return(NULL)

  df <- data.frame(date = zoo::index(out), zoo::coredata(out))
  if (ncol(df) < 7) return(NULL)
  names(df) <- c("date", "open", "high", "low", "close", "volume", "adjusted")
  df$symbol <- crypto_catalog$symbol[match(yahoo_ticker, crypto_catalog$yahoo)]
  tibble::as_tibble(df)
}

# ---- Helper: histórico múltiple ---------------------------------------------
#' Concatena el histórico CoinGecko de varias monedas en formato largo.
#' Filtramos NULLs explícitamente para que un símbolo fallido (rate-limit,
#' off-line momentáneo) no tumbe el lote entero — `map_dfr` con un NULL
#' en la lista falla en algunas versiones de purrr.
fetch_history_multi <- function(symbols, days = 90) {
  ids <- crypto_catalog$id[match(symbols, crypto_catalog$symbol)]
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0) return(tibble::tibble(
    date = as.Date(character()), symbol = character(),
    price = numeric(), market_cap = numeric(), volume = numeric()
  ))
  results <- purrr::map(ids, fetch_history_coingecko, days = days)
  results <- purrr::compact(results)
  if (length(results) == 0) {
    return(tibble::tibble(
      date = as.Date(character()), symbol = character(),
      price = numeric(), market_cap = numeric(), volume = numeric()
    ))
  }
  dplyr::bind_rows(results)
}

#' Concatena OHLCV Yahoo de varias monedas para el módulo de velas.
fetch_ohlcv_multi <- function(symbols, days = 365) {
  tickers <- crypto_catalog$yahoo[match(symbols, crypto_catalog$symbol)]
  tickers <- tickers[!is.na(tickers) & nzchar(tickers)]
  from <- Sys.Date() - days
  results <- purrr::map(tickers, fetch_history_yahoo, from = from)
  results <- purrr::compact(results)
  if (length(results) == 0) return(NULL)
  dplyr::bind_rows(results)
}
