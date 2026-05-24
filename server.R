# ============================================================================
# alleria · server.R — lógica reactiva sobre stack R puro
# ============================================================================

server <- function(input, output, session) {

  # =========================================================================
  # Datos en vivo · snapshot CoinGecko (refresco cada 5 min)
  # =========================================================================
  market_snapshot <- reactivePoll(
    intervalMillis = 5 * 60 * 1000,
    session = session,
    checkFunc = function() Sys.time(),
    valueFunc = function() {
      withCallingHandlers(
        fetch_market_snapshot(),
        warning = function(w) invokeRestart("muffleWarning")
      )
    }
  )

  # ---- Navegación desde el hero -------------------------------------------
  observeEvent(input$go_market, {
    bslib::nav_select(id = "tabs", selected = "market", session = session)
  })

  # =========================================================================
  # Helper: span coloreado para porcentajes
  # =========================================================================
  pct_html <- function(x) {
    ifelse(
      is.na(x), "—",
      sprintf('<span class="alleria-%s">%+.2f%%</span>',
              ifelse(x >= 0, "up", "down"), x)
    )
  }

  # mini-sparkline SVG en línea para la tabla
  sparkline_svg <- function(v, w = 100, h = 30) {
    v <- v[!is.na(v)]
    if (length(v) < 2) return("")
    rng <- range(v)
    if (rng[2] == rng[1]) return("")
    n  <- length(v)
    xs <- seq(0, w, length.out = n)
    ys <- h - (v - rng[1]) / (rng[2] - rng[1]) * h
    pts <- paste0(round(xs, 1), ",", round(ys, 1), collapse = " ")
    color <- ifelse(utils::tail(v, 1) >= v[1],
                    crypto_aurora$mint, crypto_aurora$crimson)
    sprintf(
      '<svg width="%d" height="%d" viewBox="0 0 %d %d">
         <polyline points="%s" fill="none" stroke="%s" stroke-width="1.6"/>
       </svg>', w, h, w, h, pts, color
    )
  }

  # =========================================================================
  # Portada · KPIs vivos
  # =========================================================================
  output$kpi_mcap <- renderText({
    snap <- market_snapshot()
    fmt_compact_usd(sum(snap$market_cap, na.rm = TRUE))
  })

  output$kpi_vol <- renderText({
    snap <- market_snapshot()
    fmt_compact_usd(sum(snap$total_volume, na.rm = TRUE))
  })

  output$kpi_best <- renderText({
    snap <- market_snapshot()
    if (nrow(snap) == 0 || all(is.na(snap$change_24h))) return("—")
    i <- which.max(snap$change_24h)
    snap$symbol[i]
  })
  output$kpi_best_sub <- renderText({
    snap <- market_snapshot()
    if (nrow(snap) == 0 || all(is.na(snap$change_24h))) return("Sin datos en vivo")
    i <- which.max(snap$change_24h)
    paste0("+", scales::number(snap$change_24h[i], accuracy = 0.01), "% · 24h")
  })

  output$kpi_worst <- renderText({
    snap <- market_snapshot()
    if (nrow(snap) == 0 || all(is.na(snap$change_24h))) return("—")
    i <- which.min(snap$change_24h)
    snap$symbol[i]
  })
  output$kpi_worst_sub <- renderText({
    snap <- market_snapshot()
    if (nrow(snap) == 0 || all(is.na(snap$change_24h))) return("Sin datos en vivo")
    i <- which.min(snap$change_24h)
    paste0(scales::number(snap$change_24h[i], accuracy = 0.01), "% · 24h")
  })

  # =========================================================================
  # Tab Mercado · Treemap, KPIs, tabla con sparklines
  # =========================================================================
  output$market_treemap <- plotly::renderPlotly({
    plot_market_treemap(market_snapshot())
  })

  output$market_last_update <- renderText({
    snap <- market_snapshot()
    if (nrow(snap) == 0) return("—")
    last <- suppressWarnings(max(snap$last_updated, na.rm = TRUE))
    if (!is.finite(last)) return("—")
    format(last, "%H:%M:%S UTC · %d-%b-%Y")
  })

  output$market_total_mcap <- renderText({
    fmt_compact_usd(sum(market_snapshot()$market_cap, na.rm = TRUE))
  })
  output$market_total_vol <- renderText({
    fmt_compact_usd(sum(market_snapshot()$total_volume, na.rm = TRUE))
  })

  market_table_df <- reactive({
    snap <- market_snapshot()
    snap |>
      dplyr::transmute(
        Cripto    = paste0("<b>", symbol, "</b> · ",
                           '<span class="alleria-muted">', name, "</span>"),
        Precio    = fmt_usd(price, accuracy = 0.01),
        `1h`      = pct_html(change_1h),
        `24h`     = pct_html(change_24h),
        `7d`      = pct_html(change_7d),
        `30d`     = pct_html(change_30d),
        `Cap.`    = fmt_compact_usd(market_cap),
        `Vol. 24h` = fmt_compact_usd(total_volume),
        `Spark 7d` = vapply(sparkline, sparkline_svg, character(1))
      )
  })

  output$market_table <- DT::renderDataTable({
    df <- market_table_df()
    DT::datatable(
      df,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        dom        = "t",
        pageLength = 25,
        ordering   = TRUE,
        scrollX    = TRUE
      )
    )
  }, server = FALSE)

  output$download_market <- downloadHandler(
    filename = function() paste0("market_snapshot_",
                                 format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content  = function(file) {
      snap <- market_snapshot() |>
        dplyr::select(-sparkline, -image)
      readr::write_csv(snap, file)
    }
  )

  # =========================================================================
  # Tab Velas · OHLCV Yahoo + indicadores
  # =========================================================================
  candle_data <- reactive({
    sym <- input$candle_symbol
    win <- input$candle_window
    req(sym, win)

    # Las validaciones se hacen con `if` explícito y mensajes
    # pre-computados como character scalars. Evita los conflictos de R
    # 4.5+ con `&&` cuando un lado es length-0 (origen del error
    # `is.character(txt) is not TRUE` que llega de `need()` cuando paste0
    # devuelve character(0)).
    sym_chr <- as.character(sym)
    days    <- suppressWarnings(as.numeric(win))
    if (!isTRUE(is.finite(days))) {
      validate("Ventana inválida.")
    }

    idx <- which(crypto_catalog$symbol == sym_chr)
    if (length(idx) != 1L) {
      validate(paste0("Símbolo desconocido: ", sym_chr))
    }
    yahoo <- as.character(crypto_catalog$yahoo[idx])
    if (!isTRUE(nzchar(yahoo))) {
      validate(paste0("Ticker Yahoo vacío para ", sym_chr))
    }

    df <- tryCatch(
      fetch_history_yahoo(yahoo, from = Sys.Date() - days),
      error = function(e) {
        showNotification(paste("Yahoo:", conditionMessage(e)),
                         type = "error", duration = 6)
        NULL
      }
    )
    n_rows <- if (is.null(df)) 0L else nrow(df)
    if (n_rows <= 5L) {
      validate(paste0(
        "Yahoo Finance no devolvió datos para ", yahoo,
        ". Suele deberse al bloqueo de Yahoo a quantmod; ",
        "reintenta en 30 s o actualiza quantmod (>= 0.4.27)."
      ))
    }
    df
  })

  output$candle_plot <- plotly::renderPlotly({
    df <- candle_data()
    smas <- as.integer(c(input$candle_sma1, input$candle_sma2))
    plot_candlestick(df, sma = smas)
  })

  output$candle_rsi <- plotly::renderPlotly({
    df <- candle_data()
    plot_rsi(df, period = 14)
  })

  # =========================================================================
  # Tab Comparador · CoinGecko diario
  # =========================================================================
  compare_data <- reactive({
    syms <- input$compare_symbols
    win  <- input$compare_window
    req(syms, win)
    if (length(syms) < 1L) {
      validate("Selecciona al menos una criptodivisa.")
    }
    syms_chr <- as.character(syms)
    days     <- suppressWarnings(as.numeric(win))
    if (!isTRUE(is.finite(days))) {
      validate("Ventana inválida.")
    }
    df <- tryCatch(
      fetch_history_multi(syms_chr, days = days),
      error = function(e) {
        showNotification(paste("CoinGecko:", conditionMessage(e)),
                         type = "error", duration = 6)
        NULL
      }
    )
    n_rows <- if (is.null(df)) 0L else nrow(df)
    if (n_rows < 1L) {
      validate(paste0(
        "CoinGecko no devolvió datos para ",
        paste(syms_chr, collapse = ", "),
        ". Si llevas un rato cargando datos puede ser rate-limit ",
        "(30 req/min en el plan free); espera 1 min y reintenta."
      ))
    }
    df
  })

  output$compare_returns <- plotly::renderPlotly({
    plot_returns_indexed(compare_data())
  })
  output$compare_drawdown <- plotly::renderPlotly({
    plot_drawdown(compare_data())
  })
  output$compare_corr <- plotly::renderPlotly({
    df <- compare_data()
    df <- df |>
      dplyr::group_by(symbol) |>
      dplyr::arrange(date, .by_group = TRUE) |>
      dplyr::mutate(price = (price / dplyr::lag(price) - 1)) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(price))
    plot_corr_heatmap(df)
  })
  output$compare_calendar <- plotly::renderPlotly({
    df <- compare_data()
    sym <- if ("BTC" %in% unique(df$symbol)) "BTC" else unique(df$symbol)[1]
    plot_calendar_heatmap(df, sym = sym)
  })
  output$compare_dist <- plotly::renderPlotly({
    plot_returns_distribution(compare_data())
  })

  # =========================================================================
  # Tab Pronóstico · ARIMA / ETS / Prophet / RF
  # =========================================================================
  fc_history <- reactive({
    sym <- input$fc_symbol
    hr  <- input$fc_history
    req(sym, hr)
    days <- suppressWarnings(as.numeric(hr))
    if (!isTRUE(is.finite(days))) validate("Histórico inválido.")
    df <- tryCatch(
      fetch_history_multi(as.character(sym), days = days),
      error = function(e) {
        showNotification(paste("CoinGecko:", conditionMessage(e)),
                         type = "error", duration = 6)
        NULL
      }
    )
    n_rows <- if (is.null(df)) 0L else nrow(df)
    if (n_rows <= 30L) {
      validate("Necesitamos al menos 30 días de histórico para ajustar.")
    }
    df
  })

  # Capturamos histórico + fit en el mismo eventReactive para evitar el
  # desfase entre selectores y resultado del último ajuste.
  fc_state <- eventReactive(input$fc_run, {
    df  <- fc_history()
    sym <- input$fc_symbol
    mod <- input$fc_model
    showNotification(paste0("Ajustando modelo ", mod, " sobre ", sym, "…"),
                     type = "message", duration = 2)
    fit <- tryCatch(
      fit_model(df, model = mod, horizon = input$fc_horizon),
      error = function(e) {
        showNotification(
          paste("No se pudo ajustar:", conditionMessage(e)),
          type = "error", duration = 6
        )
        NULL
      }
    )
    list(fit = fit, history = df, symbol = sym, model = mod)
  }, ignoreNULL = FALSE)

  output$fc_label <- renderText({
    s <- fc_state()
    if (is.null(s$fit)) return("—")
    s$fit$label
  })
  output$fc_r2 <- renderText({
    s <- fc_state()
    if (is.null(s$fit)) return("—")
    formatC(s$fit$metrics$R2, digits = 3, format = "f")
  })
  output$fc_mae <- renderText({
    s <- fc_state()
    if (is.null(s$fit)) return("—")
    fmt_usd(s$fit$metrics$MAE, accuracy = 0.01)
  })
  output$fc_mape <- renderText({
    s <- fc_state()
    if (is.null(s$fit)) return("—")
    paste0(formatC(s$fit$metrics$MAPE, digits = 2, format = "f"), "%")
  })

  output$fc_plot <- plotly::renderPlotly({
    s <- fc_state()
    if (is.null(s$fit)) {
      return(plotly::plot_ly() |>
        plotly::layout(
          paper_bgcolor = crypto_aurora$bg,
          plot_bgcolor  = crypto_aurora$bg,
          annotations = list(list(
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            text = "Pulsa <b>Ajustar y pronosticar</b> para empezar.",
            showarrow = FALSE,
            font = list(family = "Crimson Pro",
                        color = crypto_aurora$muted, size = 18)
          ))
        ))
    }
    plot_forecast_fan(s$history, s$fit, symbol = s$symbol)
  })

  output$fc_table <- DT::renderDataTable({
    s <- fc_state()
    if (is.null(s$fit)) return(NULL)
    df <- s$fit$forecast |>
      dplyr::transmute(
        Fecha     = format(date, "%Y-%m-%d"),
        Pronostico   = fmt_usd(forecast),
        `IC 80% inf` = fmt_usd(lo80),
        `IC 80% sup` = fmt_usd(hi80),
        `IC 95% inf` = fmt_usd(lo95),
        `IC 95% sup` = fmt_usd(hi95)
      )
    DT::datatable(df, rownames = FALSE,
                  options = list(dom = "tip", pageLength = 14, scrollX = TRUE))
  })

  output$fc_download <- downloadHandler(
    filename = function() {
      s <- fc_state()
      paste0("forecast_",
             if (!is.null(s)) s$symbol else "x", "_",
             if (!is.null(s)) s$model else "x", "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content  = function(file) {
      s <- fc_state()
      if (is.null(s$fit)) {
        readr::write_csv(tibble::tibble(), file); return()
      }
      readr::write_csv(s$fit$forecast, file)
    }
  )
}
