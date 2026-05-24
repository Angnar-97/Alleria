# ============================================================================
# R/plots.R · galería de visualizaciones evolutivas
# ----------------------------------------------------------------------------
# Todo el output gráfico se construye con plotly (HTML interactivo) o ggplot
# tematizado con `theme_aurora`. Las funciones reciben un data frame ya
# filtrado y devuelven un widget listo para plotly::renderPlotly.
# ============================================================================

# Layout plotly base con paleta Crypto Aurora ---------------------------------
# Nota: el alto del widget lo controla `plotlyOutput(height = …)` en la UI.
# Pasar `height` a `layout()` está deprecado en plotly 4.10+ y rompe el render
# en algunas versiones; lo tramos en `plot_ly(height = …)` cuando hace falta.
.plotly_aurora <- function(p, legend_pos = "h", hide_legend = FALSE) {
  p |>
    plotly::layout(
      paper_bgcolor = crypto_aurora$bg,
      plot_bgcolor  = crypto_aurora$bg,
      font          = list(family = "Inter", color = crypto_aurora$ink_soft,
                           size = 12),
      margin        = list(l = 50, r = 20, t = 50, b = 40),
      hoverlabel    = list(bgcolor = crypto_aurora$surface,
                           bordercolor = crypto_aurora$gold,
                           font = list(family = "JetBrains Mono",
                                       color = crypto_aurora$ink)),
      xaxis = list(gridcolor = crypto_aurora$hairline,
                   zerolinecolor = crypto_aurora$hairline,
                   tickcolor = crypto_aurora$muted),
      yaxis = list(gridcolor = crypto_aurora$hairline,
                   zerolinecolor = crypto_aurora$hairline,
                   tickcolor = crypto_aurora$muted),
      legend = list(orientation = if (legend_pos == "h") "h" else "v",
                    bgcolor = "rgba(0,0,0,0)",
                    font = list(color = crypto_aurora$ink_soft)),
      showlegend = !hide_legend
    ) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("select2d", "lasso2d",
                                              "autoScale2d"))
}

.crypto_color <- function(symbols) {
  unname(crypto_palette[symbols])
}

# ---- 1. Línea de precios multimonedas ----------------------------------------
plot_price_evolution <- function(df) {
  p <- plotly::plot_ly()
  for (s in unique(df$symbol)) {
    sub <- dplyr::filter(df, symbol == s)
    p <- plotly::add_trace(
      p, data = sub, x = ~date, y = ~price, name = s,
      type = "scatter", mode = "lines",
      line = list(width = 2, color = .crypto_color(s)),
      hovertemplate = paste0("<b>", s, "</b><br>%{x|%d-%b-%Y}<br>",
                             "$%{y:,.2f}<extra></extra>")
    )
  }
  .plotly_aurora(p) |>
    plotly::layout(
      title = list(text = "<b>Evolución de precios</b>",
                   font = list(family = "Crimson Pro",
                               color = crypto_aurora$ink, size = 18),
                   x = 0.02),
      yaxis = list(title = "USD", type = "log",
                   gridcolor = crypto_aurora$hairline),
      xaxis = list(title = "")
    )
}

# ---- 2. Retornos acumulados normalizados (índice = 100) ----------------------
plot_returns_indexed <- function(df) {
  df <- df |>
    dplyr::group_by(symbol) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::mutate(idx = price / dplyr::first(price) * 100) |>
    dplyr::ungroup()

  p <- plotly::plot_ly()
  for (s in unique(df$symbol)) {
    sub <- dplyr::filter(df, symbol == s)
    p <- plotly::add_trace(
      p, data = sub, x = ~date, y = ~idx, name = s,
      type = "scatter", mode = "lines",
      line = list(width = 2, color = .crypto_color(s)),
      hovertemplate = paste0("<b>", s, "</b><br>%{x|%d-%b-%Y}<br>",
                             "Índice: %{y:.2f}<extra></extra>")
    )
  }
  .plotly_aurora(p) |>
    plotly::layout(
      title = list(text = "<b>Retornos acumulados normalizados</b>",
                   font = list(family = "Crimson Pro",
                               color = crypto_aurora$ink, size = 18),
                   x = 0.02),
      yaxis = list(title = "Índice (base = 100)"),
      xaxis = list(title = ""),
      shapes = list(list(type = "line", x0 = min(df$date), x1 = max(df$date),
                         y0 = 100, y1 = 100,
                         line = list(color = crypto_aurora$muted,
                                     dash = "dot", width = 1)))
    )
}

# ---- 3. Velas japonesas (OHLCV) ---------------------------------------------
plot_candlestick <- function(df, sma = c(20, 50)) {
  stopifnot(all(c("date", "open", "high", "low", "close", "volume") %in% names(df)))
  sym <- unique(df$symbol)[1]

  smas <- purrr::map(sma, function(k) {
    tibble::tibble(date = df$date, ma = TTR::SMA(df$close, k),
                   period = paste0("SMA", k))
  }) |> dplyr::bind_rows()

  # Construimos el panel superior (precio + SMAs). Desactivamos el rangeslider
  # del candlestick desde el propio layout del plot_ly base, antes de subplot,
  # porque post-subplot plotly puede ignorar el override.
  p_price <- plotly::plot_ly(df) |>
    plotly::add_trace(
      type = "candlestick", x = ~date,
      open = ~open, high = ~high, low = ~low, close = ~close,
      increasing = list(line = list(color = crypto_aurora$mint),
                        fillcolor = crypto_aurora$mint),
      decreasing = list(line = list(color = crypto_aurora$crimson),
                        fillcolor = crypto_aurora$crimson),
      name = sym
    ) |>
    plotly::layout(xaxis = list(rangeslider = list(visible = FALSE)))

  if (nrow(smas) > 0) {
    sma_colors <- c(crypto_aurora$gold, crypto_aurora$electric,
                    crypto_aurora$violet)[seq_along(sma)]
    for (i in seq_along(sma)) {
      sub <- dplyr::filter(smas, period == paste0("SMA", sma[i]))
      p_price <- plotly::add_lines(
        p_price, data = sub, x = ~date, y = ~ma,
        name = paste0("SMA", sma[i]),
        line = list(width = 1.5, color = sma_colors[i]),
        inherit = FALSE
      )
    }
  }

  # Panel inferior: barras de volumen coloreadas por dirección de la vela.
  p_vol <- plotly::plot_ly(df) |>
    plotly::add_bars(
      x = ~date, y = ~volume,
      marker = list(color = ifelse(df$close >= df$open,
                                   paste0(crypto_aurora$mint, "55"),
                                   paste0(crypto_aurora$crimson, "55"))),
      name = "Volumen", showlegend = FALSE
    )

  # heights c(0.7, 0.3) en lugar de c(0.74, 0.26): redondea limpio y evita
  # que algunas versiones de plotly se confundan con la suma flotante.
  p <- plotly::subplot(p_price, p_vol, nrows = 2, shareX = TRUE,
                       heights = c(0.7, 0.3), titleY = TRUE)
  .plotly_aurora(p) |>
    plotly::layout(
      title = list(text = paste0("<b>", sym, " · OHLCV diario</b>"),
                   font = list(family = "Crimson Pro",
                               color = crypto_aurora$ink, size = 18),
                   x = 0.02),
      xaxis  = list(rangeslider = list(visible = FALSE), title = ""),
      yaxis  = list(title = "Precio (USD)"),
      yaxis2 = list(title = "Volumen")
    )
}

# ---- 4. RSI ------------------------------------------------------------------
plot_rsi <- function(df, period = 14) {
  rsi <- TTR::RSI(df$close, n = period)
  d   <- tibble::tibble(date = df$date, rsi = as.numeric(rsi))

  p <- plotly::plot_ly(d, x = ~date, y = ~rsi,
                       type = "scatter", mode = "lines",
                       line = list(color = crypto_aurora$gold, width = 1.6),
                       fill = "tozeroy",
                       fillcolor = paste0(crypto_aurora$gold, "22"),
                       name = paste0("RSI(", period, ")"))
  .plotly_aurora(p) |>
    plotly::layout(
      title  = list(text = paste0("<b>RSI(", period, ")</b>"),
                    font = list(family = "Crimson Pro",
                                color = crypto_aurora$ink, size = 16),
                    x = 0.02),
      yaxis  = list(range = c(0, 100), title = ""),
      xaxis  = list(title = ""),
      shapes = list(
        list(type = "line", y0 = 70, y1 = 70, x0 = min(d$date),
             x1 = max(d$date),
             line = list(color = crypto_aurora$crimson, dash = "dot")),
        list(type = "line", y0 = 30, y1 = 30, x0 = min(d$date),
             x1 = max(d$date),
             line = list(color = crypto_aurora$mint, dash = "dot"))
      )
    )
}

# ---- 5. Heatmap de correlaciones --------------------------------------------
plot_corr_heatmap <- function(df) {
  wide <- df |>
    dplyr::select(date, symbol, price) |>
    tidyr::pivot_wider(names_from = symbol, values_from = price) |>
    dplyr::select(-date)

  cmat <- suppressWarnings(stats::cor(wide, use = "pairwise.complete.obs"))
  symbols <- colnames(cmat)

  p <- plotly::plot_ly(
    x = symbols, y = symbols, z = cmat, type = "heatmap",
    colorscale = list(
      c(0,    crypto_aurora$crimson),
      c(0.5,  crypto_aurora$surface_alt),
      c(1,    crypto_aurora$mint)
    ),
    zmin = -1, zmax = 1,
    text = round(cmat, 2),
    hovertemplate = "%{x} ~ %{y}<br>r = %{z:.2f}<extra></extra>",
    colorbar = list(title = "ρ", tickfont = list(color = crypto_aurora$ink_soft))
  )
  .plotly_aurora(p) |>
    plotly::layout(
      title = list(text = "<b>Matriz de correlación · retornos diarios</b>",
                   font = list(family = "Crimson Pro",
                               color = crypto_aurora$ink, size = 18),
                   x = 0.02),
      xaxis = list(title = "", tickangle = -45),
      yaxis = list(title = "", autorange = "reversed")
    )
}

# ---- 6. Drawdown -------------------------------------------------------------
plot_drawdown <- function(df) {
  df <- df |>
    dplyr::group_by(symbol) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::mutate(
      cummax = cummax(price),
      dd     = (price / cummax - 1) * 100
    ) |>
    dplyr::ungroup()

  p <- plotly::plot_ly()
  for (s in unique(df$symbol)) {
    sub <- dplyr::filter(df, symbol == s)
    p <- plotly::add_trace(
      p, data = sub, x = ~date, y = ~dd, name = s,
      type = "scatter", mode = "lines",
      line = list(width = 1.6, color = .crypto_color(s)),
      fill = "tozeroy",
      fillcolor = paste0(.crypto_color(s), "22"),
      hovertemplate = paste0("<b>", s,
                             "</b><br>%{x|%d-%b-%Y}<br>",
                             "Drawdown: %{y:.2f}%<extra></extra>")
    )
  }
  .plotly_aurora(p) |>
    plotly::layout(
      title = list(text = "<b>Drawdown desde máximos</b>",
                   font = list(family = "Crimson Pro",
                               color = crypto_aurora$ink, size = 18),
                   x = 0.02),
      yaxis = list(title = "% sobre el máximo histórico",
                   ticksuffix = "%"),
      xaxis = list(title = "")
    )
}

# ---- 7. Treemap de capitalización -------------------------------------------
plot_market_treemap <- function(snapshot) {
  d <- snapshot |>
    dplyr::filter(!is.na(market_cap)) |>
    dplyr::mutate(
      hover = paste0(
        "<b>", name, " (", symbol, ")</b><br>",
        "Cap: ",  fmt_compact_usd(market_cap), "<br>",
        "Vol 24h: ", fmt_compact_usd(total_volume), "<br>",
        "24h: ", ifelse(is.na(change_24h), "—",
                        scales::percent(change_24h / 100, accuracy = 0.1))
      ),
      color = ifelse(is.na(change_24h), crypto_aurora$muted,
                     ifelse(change_24h >= 0, crypto_aurora$mint,
                            crypto_aurora$crimson))
    )

  p <- plotly::plot_ly(
    d, type = "treemap",
    labels = ~symbol, parents = "", values = ~market_cap,
    textinfo = "label+value",
    text = ~ifelse(is.na(change_24h), "",
                   paste0(round(change_24h, 2), "%")),
    hovertext = ~hover,
    hoverinfo = "text",
    marker = list(colors = ~color,
                  line = list(color = crypto_aurora$bg, width = 2))
  )
  .plotly_aurora(p, hide_legend = TRUE) |>
    plotly::layout(
      title = list(text = "<b>Capitalización de mercado · 24h</b>",
                   font = list(family = "Crimson Pro",
                               color = crypto_aurora$ink, size = 18),
                   x = 0.02)
    )
}

# ---- 8. Distribución de retornos diarios -------------------------------------
plot_returns_distribution <- function(df) {
  d <- df |>
    dplyr::group_by(symbol) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::mutate(ret = (price / dplyr::lag(price) - 1) * 100) |>
    dplyr::filter(!is.na(ret)) |>
    dplyr::ungroup()

  p <- plotly::plot_ly()
  for (s in unique(d$symbol)) {
    sub <- dplyr::filter(d, symbol == s)
    p <- plotly::add_trace(
      p, data = sub, x = ~ret, name = s,
      type = "violin", side = "positive", points = FALSE, box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      line  = list(color = .crypto_color(s)),
      fillcolor = paste0(.crypto_color(s), "55")
    )
  }
  .plotly_aurora(p) |>
    plotly::layout(
      title = list(text = "<b>Distribución de retornos diarios</b>",
                   font = list(family = "Crimson Pro",
                               color = crypto_aurora$ink, size = 18),
                   x = 0.02),
      xaxis = list(title = "Retorno diario (%)", ticksuffix = "%"),
      yaxis = list(title = "")
    )
}

# ---- 9. Calendar heatmap (returns por día) ----------------------------------
plot_calendar_heatmap <- function(df, sym = "BTC") {
  d <- df |>
    dplyr::filter(.data$symbol == sym) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      ret  = (price / dplyr::lag(price) - 1) * 100,
      year = lubridate::year(date),
      week = lubridate::isoweek(date),
      dow  = lubridate::wday(date, week_start = 1, label = TRUE, abbr = TRUE)
    ) |>
    dplyr::filter(!is.na(ret))

  ggp <- ggplot2::ggplot(d,
                         ggplot2::aes(x = week, y = dow, fill = ret)) +
    ggplot2::geom_tile(colour = crypto_aurora$bg, linewidth = 0.4) +
    ggplot2::scale_fill_gradient2(
      low = crypto_aurora$crimson, mid = crypto_aurora$surface_alt,
      high = crypto_aurora$mint, midpoint = 0, name = "% diario"
    ) +
    ggplot2::facet_wrap(~ year, ncol = 1, scales = "free_x") +
    ggplot2::labs(
      title = paste0(sym, " · retornos diarios por semana"),
      x = "Semana ISO", y = ""
    )
  plotly::ggplotly(ggp) |> .plotly_aurora()
}

# ---- 10. Fan chart de pronóstico ---------------------------------------------
plot_forecast_fan <- function(history, fit_obj, symbol = "BTC") {
  fc <- fit_obj$forecast
  ft <- fit_obj$fitted

  p <- plotly::plot_ly()

  # banda 95
  p <- plotly::add_ribbons(
    p, data = fc, x = ~date, ymin = ~lo95, ymax = ~hi95,
    line = list(color = "transparent"),
    fillcolor = paste0(crypto_aurora$gold, "22"),
    name = "IC 95%", hoverinfo = "skip"
  )
  # banda 80
  p <- plotly::add_ribbons(
    p, data = fc, x = ~date, ymin = ~lo80, ymax = ~hi80,
    line = list(color = "transparent"),
    fillcolor = paste0(crypto_aurora$gold, "44"),
    name = "IC 80%", hoverinfo = "skip"
  )
  # histórico
  p <- plotly::add_trace(
    p, data = history, x = ~date, y = ~price,
    type = "scatter", mode = "lines",
    line = list(color = crypto_aurora$ink_soft, width = 1.8),
    name = "Histórico"
  )
  # fit en sample
  p <- plotly::add_trace(
    p, data = ft, x = ~date, y = ~fitted,
    type = "scatter", mode = "lines",
    line = list(color = crypto_aurora$electric, width = 1.2, dash = "dot"),
    name = "Ajuste"
  )
  # forecast media
  p <- plotly::add_trace(
    p, data = fc, x = ~date, y = ~forecast,
    type = "scatter", mode = "lines",
    line = list(color = crypto_aurora$gold, width = 2.4),
    name = "Pronóstico"
  )
  .plotly_aurora(p) |>
    plotly::layout(
      title = list(text = paste0("<b>", symbol, " · Pronóstico (",
                                 fit_obj$label, ")</b>"),
                   font = list(family = "Crimson Pro",
                               color = crypto_aurora$ink, size = 18),
                   x = 0.02),
      xaxis = list(title = ""),
      yaxis = list(title = "USD")
    )
}
