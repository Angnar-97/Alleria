# ============================================================================
# alleria · global.R
# ----------------------------------------------------------------------------
# Aplicación Shiny independiente para análisis e inferencia sobre criptodivisas.
# Stack 100% R: scraping en vivo (CoinGecko + Yahoo via quantmod), modelos
# clásicos de series temporales (ARIMA / ETS / Prophet / Random Forest) y
# visualización editorial con paleta "Crypto Aurora".
#
# Renombrada de doge_whisperer en honor a Alleria Windrunner — la cazadora
# alta elfa de WoW (precisión, rastreo, tiros al horizonte ~ pronóstico).
#
# Autor: Alejandro Navas González (Angnar) · angnar@telaris.es
# Proyecto independiente del portfolio personal de Angnar — no afiliado a
# ninguna empresa. Logo: árbol celta (celta.png), coherente con
# rnaseqr / firesr / ecorangr / phytora / andera.
# ============================================================================

# ---- Encoding ---------------------------------------------------------------
options(encoding = "UTF-8")
Sys.setlocale("LC_TIME", "C")

# ---- Guardia de paquetes ----------------------------------------------------
.required_pkgs <- c(
  "shiny", "bslib", "bsicons", "htmltools", "shinyWidgets",
  "shinycssloaders", "dplyr", "tidyr", "tibble", "purrr", "stringr",
  "lubridate", "readr", "scales", "memoise", "cachem", "httr2", "jsonlite",
  "DT", "ggplot2", "plotly", "forecast", "quantmod", "TTR"
)
.missing_pkgs <- .required_pkgs[
  !vapply(.required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]
if (length(.missing_pkgs) > 0) {
  stop(
    "alleria: faltan paquetes requeridos -> ",
    paste(.missing_pkgs, collapse = ", "),
    "\nInstala con: install.packages(c(",
    paste0('"', .missing_pkgs, '"', collapse = ", "), "))",
    call. = FALSE
  )
}

# ---- Paquetes ---------------------------------------------------------------
# ORDEN IMPORTA: jsonlite exporta `validate()` (valida si un string es JSON
# bien formado) y enmascara `shiny::validate()`. Si shiny se carga ANTES de
# jsonlite, ese masking provoca el error críptico
# `is.character(txt) is not TRUE` al llegar `NULL` desde `need()`. Cargamos
# primero las utilidades de datos y al final shiny, para que `validate` y
# `need` queden resueltas al espacio de shiny en la búsqueda por defecto.
suppressPackageStartupMessages({
  # Datos & utilidades
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(scales)
  library(memoise)
  library(cachem)
  library(httr2)
  library(jsonlite)

  # Tablas
  library(DT)

  # Visualización
  library(ggplot2)
  library(plotly)

  # Series temporales / modelos
  library(forecast)        # ARIMA, ETS
  library(quantmod)        # OHLCV Yahoo + indicadores técnicos
  library(TTR)             # SMA, EMA, RSI, MACD

  # UI / dashboard — al final para que shiny::validate gane la búsqueda
  library(htmltools)
  library(shinyWidgets)
  library(shinycssloaders)
  library(bslib)
  library(bsicons)
  library(shiny)
})

# Belt-and-suspenders: aliases explícitos a las funciones críticas de shiny
# por si algún paquete futuro vuelve a enmascarar. Estas referencias son
# las que usa el server; son inmunes al orden de carga.
validate <- shiny::validate
need     <- shiny::need
req      <- shiny::req

# Modelos opcionales (carga perezosa con requireNamespace en R/models.R)
#   - prophet  -> tendencia + estacionalidad
#   - ranger   -> random forest

# ---- Diseño global de gráficas ----------------------------------------------
# Paleta Crypto Aurora ---------------------------------------------------------
crypto_aurora <- list(
  bg          = "#0E1116",
  surface     = "#161B22",
  surface_alt = "#1F2731",
  hairline    = "#2A323D",
  ink         = "#E6EDF3",
  ink_soft    = "#C9D1D9",
  muted       = "#8B949E",

  # Acentos
  gold        = "#F2A900",   # primario · Doge / Bitcoin
  gold_soft   = "#FFD86B",
  mint        = "#3FB950",   # alcista
  mint_soft   = "#56D364",
  crimson     = "#F85149",   # bajista
  electric    = "#58A6FF",   # info / azul
  violet      = "#A371F7"    # acento secundario
)

# Escala discreta consistente para criptodivisas (10 colores)
crypto_palette <- c(
  BTC  = "#F2A900",
  ETH  = "#A371F7",
  BNB  = "#FFD86B",
  SOL  = "#58A6FF",
  ADA  = "#3FB950",
  XRP  = "#79C0FF",
  DOT  = "#FF7B72",
  LTC  = "#8B949E",
  LINK = "#56D364",
  UNI  = "#F85149"
)

# Tema ggplot editorial dark
theme_aurora <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = "Inter") +
    theme(
      plot.background    = element_rect(fill = crypto_aurora$bg, colour = NA),
      panel.background   = element_rect(fill = crypto_aurora$bg, colour = NA),
      panel.grid.major   = element_line(colour = "#222831", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = crypto_aurora$muted),
      axis.title         = element_text(colour = crypto_aurora$ink_soft),
      plot.title         = element_text(family = "Crimson Pro",
                                        face = "bold", size = base_size * 1.4,
                                        colour = crypto_aurora$ink),
      plot.subtitle      = element_text(colour = crypto_aurora$muted,
                                        size = base_size * 0.95),
      plot.caption       = element_text(colour = crypto_aurora$muted,
                                        size = base_size * 0.8, hjust = 1),
      strip.background   = element_rect(fill = crypto_aurora$surface_alt,
                                        colour = NA),
      strip.text         = element_text(colour = crypto_aurora$ink_soft,
                                        face = "bold"),
      legend.background  = element_rect(fill = crypto_aurora$bg, colour = NA),
      legend.key         = element_rect(fill = crypto_aurora$bg, colour = NA),
      legend.text        = element_text(colour = crypto_aurora$ink_soft),
      legend.title       = element_text(colour = crypto_aurora$ink_soft)
    )
}

theme_set(theme_aurora())

# ---- Tema bslib (Bootstrap 5 dark) ------------------------------------------
alleria_theme <- bslib::bs_theme(
  version      = 5,
  preset       = NULL,
  bootswatch   = NULL,
  primary      = crypto_aurora$gold,
  secondary    = crypto_aurora$violet,
  success      = crypto_aurora$mint,
  info         = crypto_aurora$electric,
  warning      = crypto_aurora$gold_soft,
  danger       = crypto_aurora$crimson,
  bg           = crypto_aurora$bg,
  fg           = crypto_aurora$ink,
  base_font    = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Crimson Pro"),
  code_font    = bslib::font_google("JetBrains Mono"),

  "body-bg"          = crypto_aurora$bg,
  "body-color"       = crypto_aurora$ink,
  "border-radius"    = "0.55rem",
  "border-radius-sm" = "0.4rem",
  "border-radius-lg" = "0.75rem",

  "card-bg"           = crypto_aurora$surface,
  "card-cap-bg"       = crypto_aurora$surface_alt,
  "card-border-color" = crypto_aurora$hairline,

  "navbar-bg"                = "#0A0D11",
  "navbar-dark-color"        = "rgba(230,237,243,0.72)",
  "navbar-dark-hover-color"  = crypto_aurora$gold,
  "navbar-dark-active-color" = crypto_aurora$gold,
  "navbar-brand-font-size"   = "1.3rem",

  "input-bg"                = crypto_aurora$surface_alt,
  "input-color"             = crypto_aurora$ink,
  "input-border-color"      = crypto_aurora$hairline,
  "input-focus-border-color"= crypto_aurora$gold,
  "btn-font-weight"         = "500"
)

# ---- Cargar utilidades -------------------------------------------------------
source("R/data_fetch.R", local = FALSE, encoding = "UTF-8")
source("R/models.R",     local = FALSE, encoding = "UTF-8")
source("R/plots.R",      local = FALSE, encoding = "UTF-8")

# ---- Catálogo de criptodivisas ----------------------------------------------
# Mantenemos las 10 originales del proyecto pero con metadatos enriquecidos.
crypto_catalog <- tibble::tribble(
  ~id,            ~symbol, ~name,        ~yahoo,
  "bitcoin",      "BTC",   "Bitcoin",    "BTC-USD",
  "ethereum",     "ETH",   "Ethereum",   "ETH-USD",
  "binancecoin",  "BNB",   "Binance",    "BNB-USD",
  "solana",       "SOL",   "Solana",     "SOL-USD",
  "cardano",      "ADA",   "Cardano",    "ADA-USD",
  "ripple",       "XRP",   "Ripple",     "XRP-USD",
  "polkadot",     "DOT",   "Polkadot",   "DOT-USD",
  "litecoin",     "LTC",   "Litecoin",   "LTC-USD",
  "chainlink",    "LINK",  "Chainlink",  "LINK-USD",
  "uniswap",      "UNI",   "Uniswap",    "UNI-USD"
)

choices_crypto <- setNames(crypto_catalog$symbol, crypto_catalog$name)

# ---- Cache compartida en disco (datos de mercado) ---------------------------
cache_dir <- file.path(tempdir(), "alleria_cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
mem_cache <- cachem::cache_disk(cache_dir, max_age = 60 * 5)  # 5 min TTL

# Memoise de las funciones de scraping (definidas en R/data_fetch.R)
fetch_market_snapshot   <- memoise::memoise(fetch_market_snapshot_raw,
                                            cache = mem_cache)
fetch_history_coingecko <- memoise::memoise(fetch_history_coingecko_raw,
                                            cache = mem_cache)
fetch_history_yahoo     <- memoise::memoise(fetch_history_yahoo_raw,
                                            cache = mem_cache)

# ---- Helpers de formato ------------------------------------------------------
fmt_usd <- function(x, accuracy = 0.01) {
  scales::dollar(x, accuracy = accuracy, prefix = "$",
                 big.mark = ",", decimal.mark = ".")
}

fmt_compact_usd <- function(x) {
  ifelse(is.na(x) | is.null(x), "—",
         scales::dollar(x, accuracy = 0.1, prefix = "$",
                        scale_cut = scales::cut_short_scale()))
}

fmt_pct <- function(x, accuracy = 0.01) {
  scales::percent(x / 100, accuracy = accuracy)
}

color_pct <- function(x) {
  ifelse(is.na(x), crypto_aurora$muted,
         ifelse(x >= 0, crypto_aurora$mint, crypto_aurora$crimson))
}
