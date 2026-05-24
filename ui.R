# ============================================================================
# alleria · ui.R — page_navbar editorial · paleta Crypto Aurora
# ============================================================================

ui <- bslib::page_navbar(
  id           = "tabs",
  title        = tags$span(
    class = "alleria-brand",
    tags$img(src = "celta.png", class = "alleria-logo", alt = "Árbol celta"),
    tags$span(class = "alleria-brand-text",
              "al", tags$span(class = "alleria-accent", "l"), "eria")
  ),
  window_title = "alleria · cripto en R",
  theme        = alleria_theme,
  fillable     = FALSE,
  padding      = 0,
  navbar_options = bslib::navbar_options(
    position    = "fixed-top",
    collapsible = TRUE
  ),

  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "alleria.css"),
      tags$link(rel = "icon", type = "image/png", href = "celta.png"),
      tags$meta(name = "description",
                content = paste(
                  "alleria es un dashboard Shiny en R para rastrear",
                  "criptodivisas en tiempo real con CoinGecko + Yahoo Finance,",
                  "modelos ARIMA / ETS / Prophet / Random Forest y",
                  "visualizaciones interactivas con plotly."
                )),
      tags$meta(name = "author", content = "Alejandro Navas González (Angnar)"),
      tags$meta(name = "viewport",
                content = "width=device-width, initial-scale=1")
    ),
    tags$div(class = "alleria-navbar-spacer")
  ),

  footer = tags$footer(
    class = "alleria-footer",
    tags$div(class = "alleria-footer-grid",
      tags$div(class = "alleria-footer-col",
        tags$span(class = "alleria-eyebrow", "alleria"),
        tags$p(class = "alleria-footer-lead",
          "Cazadora de mercados cripto, en R: scraping en vivo, modelos ",
          "estadísticos clásicos y visualización editorial.")
      ),
      tags$div(class = "alleria-footer-col",
        tags$span(class = "alleria-eyebrow", "Autoría"),
        tags$a(href = "https://github.com/Angnar-97",
               target = "_blank", rel = "noopener",
               "Alejandro Navas González"),
        tags$br(),
        tags$a(href = "mailto:angnar@telaris.es", "angnar@telaris.es")
      ),
      tags$div(class = "alleria-footer-col",
        tags$span(class = "alleria-eyebrow", "Datos"),
        tags$a(href = "https://www.coingecko.com/api/documentation",
               target = "_blank", rel = "noopener", "CoinGecko API ↗"),
        tags$br(),
        tags$a(href = "https://finance.yahoo.com/", target = "_blank",
               rel = "noopener", "Yahoo Finance ↗"),
        tags$br(),
        tags$span(class = "alleria-muted", "MIT License · © 2026")
      )
    )
  ),

  # =========================================================================
  # Tab 1 · Portada
  # =========================================================================
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("house"), " Portada"),
    value = "home",

    tags$section(
      class = "alleria-hero",
      tags$div(class = "alleria-hero-inner",
        tags$div(class = "alleria-hero-art",
          tags$img(src = "celta.png", class = "alleria-hero-logo",
                   alt = "Árbol celta")
        ),
        tags$div(class = "alleria-hero-content",
          tags$span(class = "alleria-eyebrow",
            "Cripto en R · Shiny + bslib · Tiempo real"),
          tags$h1(class = "alleria-hero-title",
            "al", tags$span(class = "alleria-accent", "l"), "eria"),
          tags$p(class = "alleria-hero-tagline",
            "Cazadora de mercados cripto, en R."),
          tags$p(class = "alleria-hero-sub",
            "Un cuaderno interactivo que rastrea diez criptomonedas en ",
            "vivo: snapshot de mercado vía CoinGecko, OHLCV diario de Yahoo ",
            "Finance, indicadores técnicos clásicos (SMA, EMA, RSI) y ",
            "modelos de pronóstico estadístico (ARIMA, ETS, Prophet, ",
            "Random Forest). Sin Python, sin keys."),
          tags$div(class = "alleria-hero-actions",
            actionButton("go_market", "Ver mercado",
                         class = "btn btn-primary btn-lg alleria-cta",
                         icon = icon("arrow-right")),
            tags$a(class = "alleria-secondary-link",
                   href = "#workflow", "Cómo funciona ↓")
          )
        )
      )
    ),

    # KPIs en vivo
    tags$section(class = "alleria-section",
      tags$div(class = "alleria-section-head",
        tags$span(class = "alleria-eyebrow", "Instantánea de mercado"),
        tags$h2(class = "alleria-section-title",
                "Lo que pasa ahora en las top-10.")
      ),
      tags$div(class = "alleria-kpis",
        tags$article(class = "alleria-kpi",
          tags$span(class = "alleria-kpi-label", "Cap. total seguida"),
          tags$span(class = "alleria-kpi-value",
                    textOutput("kpi_mcap", inline = TRUE)),
          tags$span(class = "alleria-kpi-sub", "Suma de 10 monedas")
        ),
        tags$article(class = "alleria-kpi",
          tags$span(class = "alleria-kpi-label", "Volumen 24h"),
          tags$span(class = "alleria-kpi-value",
                    textOutput("kpi_vol", inline = TRUE)),
          tags$span(class = "alleria-kpi-sub", "Trading global agregado")
        ),
        tags$article(class = "alleria-kpi",
          tags$span(class = "alleria-kpi-label", "Mejor 24h"),
          tags$span(class = "alleria-kpi-value",
                    textOutput("kpi_best", inline = TRUE)),
          tags$span(class = "alleria-kpi-sub",
                    textOutput("kpi_best_sub", inline = TRUE))
        ),
        tags$article(class = "alleria-kpi",
          tags$span(class = "alleria-kpi-label", "Peor 24h"),
          tags$span(class = "alleria-kpi-value",
                    textOutput("kpi_worst", inline = TRUE)),
          tags$span(class = "alleria-kpi-sub",
                    textOutput("kpi_worst_sub", inline = TRUE))
        )
      )
    ),

    # Workflow
    tags$section(class = "alleria-section", id = "workflow",
      tags$div(class = "alleria-section-head",
        tags$span(class = "alleria-eyebrow", "Recorrido"),
        tags$h2(class = "alleria-section-title",
                "Del precio crudo al pronóstico, en cuatro pasos.")
      ),
      tags$div(class = "alleria-steps",
        tags$article(class = "alleria-step",
          tags$span(class = "alleria-step-num", "01 · Rastrea"),
          tags$h3("Tiempo real"),
          tags$p("Snapshot multimercado vía ", tags$b("CoinGecko"),
                 " (precio, mcap, volumen, 1h/24h/7d/30d) más OHLCV diario ",
                 "de ", tags$b("Yahoo Finance"), " con caché de 5 min.")
        ),
        tags$article(class = "alleria-step",
          tags$span(class = "alleria-step-num", "02 · Observa"),
          tags$h3("Snapshot"),
          tags$p("Treemap de capitalización con coloreado alcista/bajista,",
                 " tabla con sparklines y métricas comparables.")
        ),
        tags$article(class = "alleria-step",
          tags$span(class = "alleria-step-num", "03 · Apunta"),
          tags$h3("Velas + indicadores"),
          tags$p("Candlesticks con SMA y RSI, retornos normalizados, ",
                 "drawdowns, distribución de retornos diarios y heatmap ",
                 "de correlación.")
        ),
        tags$article(class = "alleria-step",
          tags$span(class = "alleria-step-num", "04 · Dispara"),
          tags$h3("Pronóstico"),
          tags$p(tags$b("ARIMA"), ", ", tags$b("ETS"), ", ", tags$b("Prophet"),
                 " y ", tags$b("Random Forest"),
                 " con fan chart de intervalos al 80 / 95%.")
        )
      )
    ),

    # Capacidades
    tags$section(class = "alleria-section",
      tags$div(class = "alleria-section-head",
        tags$span(class = "alleria-eyebrow", "Capacidades"),
        tags$h2(class = "alleria-section-title",
                "Qué puedes hacer en alleria.")
      ),
      tags$div(class = "alleria-features",
        tags$article(class = "alleria-feature",
          tags$div(class = "alleria-feature-icon",
                   bsicons::bs_icon("currency-bitcoin")),
          tags$div(
            tags$h3("Mercado vivo"),
            tags$p("Top-10 con precio, cap, volumen, 1h/24h/7d/30d y ",
                   "sparkline 7d.")
          )
        ),
        tags$article(class = "alleria-feature",
          tags$div(class = "alleria-feature-icon", bsicons::bs_icon("graph-up")),
          tags$div(
            tags$h3("Velas + RSI"),
            tags$p("OHLCV interactivo Yahoo con SMAs configurables y ",
                   "RSI(14) subordinado.")
          )
        ),
        tags$article(class = "alleria-feature",
          tags$div(class = "alleria-feature-icon",
                   bsicons::bs_icon("arrow-left-right")),
          tags$div(
            tags$h3("Comparador"),
            tags$p("Retornos normalizados base 100, drawdowns y matriz ",
                   "de correlación.")
          )
        ),
        tags$article(class = "alleria-feature",
          tags$div(class = "alleria-feature-icon",
                   bsicons::bs_icon("calendar-heart")),
          tags$div(
            tags$h3("Heatmap calendario"),
            tags$p("Retornos diarios por semana ISO con divergente ",
                   "rojo·verde.")
          )
        ),
        tags$article(class = "alleria-feature",
          tags$div(class = "alleria-feature-icon",
                   bsicons::bs_icon("magic")),
          tags$div(
            tags$h3("Pronóstico"),
            tags$p("ARIMA, ETS, Prophet y Random Forest sobre lags + ",
                   "indicadores técnicos.")
          )
        ),
        tags$article(class = "alleria-feature",
          tags$div(class = "alleria-feature-icon",
                   bsicons::bs_icon("download")),
          tags$div(
            tags$h3("Exportable"),
            tags$p("Cada tabla y serie histórica descargable en CSV / ",
                   "Excel para análisis aguas abajo.")
          )
        )
      )
    )
  ),

  # =========================================================================
  # Tab 2 · Mercado
  # =========================================================================
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("currency-exchange"), " Mercado"),
    value = "market",

    tags$section(class = "alleria-page",
      tags$div(class = "alleria-page-inner",
        tags$span(class = "alleria-eyebrow", "Snapshot · CoinGecko"),
        tags$h1(class = "alleria-page-title", "Mercado en vivo"),

        bslib::layout_columns(
          col_widths = c(8, 4),

          bslib::card(
            full_screen = TRUE,
            bslib::card_header(bsicons::bs_icon("grid-3x3-gap-fill"),
                               " Capitalización"),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput("market_treemap", height = "520px"),
                type = 6, color = crypto_aurora$gold
              )
            )
          ),

          tagList(
            bslib::value_box(
              title = "Última actualización",
              value = textOutput("market_last_update"),
              showcase = bsicons::bs_icon("clock"),
              theme = "secondary"
            ),
            bslib::value_box(
              title = "Cap. total",
              value = textOutput("market_total_mcap"),
              showcase = bsicons::bs_icon("piggy-bank"),
              theme = "warning"
            ),
            bslib::value_box(
              title = "Vol. 24h",
              value = textOutput("market_total_vol"),
              showcase = bsicons::bs_icon("activity"),
              theme = "info"
            )
          )
        ),

        tags$br(),

        bslib::card(
          bslib::card_header(bsicons::bs_icon("table"),
                             " Tabla de mercado · top 10"),
          bslib::card_body(
            shinycssloaders::withSpinner(
              DT::dataTableOutput("market_table"),
              type = 6, color = crypto_aurora$gold
            )
          ),
          bslib::card_footer(
            downloadButton("download_market", "Descargar CSV",
                           class = "btn btn-outline-primary btn-sm",
                           icon = icon("download"))
          )
        )
      )
    )
  ),

  # =========================================================================
  # Tab 3 · Velas
  # =========================================================================
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("graph-up-arrow"), " Velas"),
    value = "candles",

    tags$section(class = "alleria-page",
      tags$div(class = "alleria-page-inner",
        tags$span(class = "alleria-eyebrow", "OHLCV · Yahoo Finance"),
        tags$h1(class = "alleria-page-title", "Análisis técnico"),

        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput("candle_symbol", "Criptodivisa",
                      choices = choices_crypto, selected = "BTC"),
          selectInput("candle_window", "Ventana",
                      choices = c("90 días" = 90, "180 días" = 180,
                                  "365 días" = 365, "2 años" = 730),
                      selected = 180),
          selectInput("candle_sma1", "SMA corta",
                      choices = c(7, 14, 20, 30, 50), selected = 20),
          selectInput("candle_sma2", "SMA larga",
                      choices = c(20, 50, 100, 200), selected = 50)
        ),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(bsicons::bs_icon("graph-up"), " Velas + volumen"),
          bslib::card_body(
            shinycssloaders::withSpinner(
              plotly::plotlyOutput("candle_plot", height = "560px"),
              type = 6, color = crypto_aurora$gold
            )
          )
        ),
        bslib::card(
          bslib::card_header(bsicons::bs_icon("speedometer2"),
                             " RSI(14) · sobrecompra y sobreventa"),
          bslib::card_body(
            shinycssloaders::withSpinner(
              plotly::plotlyOutput("candle_rsi", height = "240px"),
              type = 6, color = crypto_aurora$gold
            )
          )
        )
      )
    )
  ),

  # =========================================================================
  # Tab 4 · Comparador
  # =========================================================================
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("arrow-left-right"), " Comparador"),
    value = "compare",

    tags$section(class = "alleria-page",
      tags$div(class = "alleria-page-inner",
        tags$span(class = "alleria-eyebrow", "Multi-cripto · CoinGecko"),
        tags$h1(class = "alleria-page-title", "Comparador evolutivo"),

        bslib::layout_columns(
          col_widths = c(8, 4),

          shinyWidgets::checkboxGroupButtons(
            inputId = "compare_symbols",
            label = "Selecciona varias criptodivisas",
            choices = choices_crypto,
            selected = c("BTC", "ETH", "SOL"),
            individual = TRUE,
            checkIcon = list(yes = icon("check"))
          ),
          selectInput("compare_window", "Histórico",
                      choices = c("30 días" = 30, "90 días" = 90,
                                  "180 días" = 180, "365 días" = 365,
                                  "2 años" = 730),
                      selected = 180)
        ),

        bslib::layout_columns(
          col_widths = c(12),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(bsicons::bs_icon("graph-up"),
                               " Retornos acumulados normalizados"),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput("compare_returns", height = "480px"),
                type = 6, color = crypto_aurora$gold
              )
            )
          )
        ),

        bslib::layout_columns(
          col_widths = c(6, 6),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(bsicons::bs_icon("activity"),
                               " Drawdown desde máximos"),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput("compare_drawdown", height = "420px"),
                type = 6, color = crypto_aurora$gold
              )
            )
          ),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(bsicons::bs_icon("grid-3x3"),
                               " Correlación de retornos"),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput("compare_corr", height = "420px"),
                type = 6, color = crypto_aurora$gold
              )
            )
          )
        ),

        bslib::layout_columns(
          col_widths = c(7, 5),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(bsicons::bs_icon("calendar-heart"),
                               " Heatmap calendario · BTC"),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput("compare_calendar", height = "420px"),
                type = 6, color = crypto_aurora$gold
              )
            )
          ),
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(bsicons::bs_icon("bar-chart-line"),
                               " Distribución de retornos diarios"),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput("compare_dist", height = "420px"),
                type = 6, color = crypto_aurora$gold
              )
            )
          )
        )
      )
    )
  ),

  # =========================================================================
  # Tab 5 · Pronóstico
  # =========================================================================
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("magic"), " Pronóstico"),
    value = "forecast",

    tags$section(class = "alleria-page",
      tags$div(class = "alleria-page-inner",
        tags$span(class = "alleria-eyebrow", "Modelos · forecast / prophet / ranger"),
        tags$h1(class = "alleria-page-title", "Pronóstico de precios"),

        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput("fc_symbol", "Criptodivisa",
                      choices = choices_crypto, selected = "BTC"),
          selectInput("fc_model", "Modelo",
                      choices = c("ARIMA", "ETS", "Prophet", "RandomForest"),
                      selected = "ARIMA"),
          selectInput("fc_history", "Histórico (días)",
                      choices = c(90, 180, 365, 730),
                      selected = 180),
          sliderInput("fc_horizon", "Horizonte (días)",
                      min = 3, max = 60, value = 14, step = 1)
        ),

        tags$div(class = "alleria-hero-actions", style = "margin-bottom: 1.5rem;",
          actionButton("fc_run", "Ajustar y pronosticar",
                       class = "btn btn-primary alleria-cta",
                       icon = icon("play"))
        ),

        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          bslib::value_box(
            title = "Modelo", value = textOutput("fc_label"),
            showcase = bsicons::bs_icon("cpu"),
            theme = "secondary"
          ),
          bslib::value_box(
            title = "R²", value = textOutput("fc_r2"),
            showcase = bsicons::bs_icon("bullseye"),
            theme = "warning"
          ),
          bslib::value_box(
            title = "MAE (USD)", value = textOutput("fc_mae"),
            showcase = bsicons::bs_icon("rulers"),
            theme = "info"
          ),
          bslib::value_box(
            title = "MAPE (%)", value = textOutput("fc_mape"),
            showcase = bsicons::bs_icon("percent"),
            theme = "primary"
          )
        ),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(bsicons::bs_icon("graph-up"),
                             " Histórico + ajuste + pronóstico"),
          bslib::card_body(
            shinycssloaders::withSpinner(
              plotly::plotlyOutput("fc_plot", height = "520px"),
              type = 6, color = crypto_aurora$gold
            )
          ),
          bslib::card_footer(
            downloadButton("fc_download", "Descargar predicciones (CSV)",
                           class = "btn btn-outline-primary btn-sm",
                           icon = icon("download"))
          )
        ),

        bslib::card(
          bslib::card_header(bsicons::bs_icon("table"), " Tabla de pronóstico"),
          bslib::card_body(
            DT::dataTableOutput("fc_table")
          )
        )
      )
    )
  ),

  # =========================================================================
  # Tab 6 · Metodología
  # =========================================================================
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("info-circle"), " Metodología"),
    value = "methodology",

    tags$section(class = "alleria-page",
      tags$div(class = "alleria-page-inner",
        tags$span(class = "alleria-eyebrow", "Cómo funciona"),
        tags$h1(class = "alleria-page-title", "Metodología y fuentes"),

        bslib::layout_columns(
          col_widths = c(6, 6),

          bslib::card(
            bslib::card_header(bsicons::bs_icon("cloud-download"),
                               " Fuentes de datos"),
            bslib::card_body(
              tags$dl(class = "alleria-deps",
                tags$dt("CoinGecko"),
                tags$dd("Endpoints públicos /coins/markets y ",
                        "/coins/{id}/market_chart sin API key. Caché en ",
                        "disco con TTL de 5 min."),
                tags$dt("Yahoo Finance"),
                tags$dd("OHLCV diario via quantmod::getSymbols('BTC-USD',",
                        " src = 'yahoo'). Robusto para velas e ",
                        "indicadores técnicos."),
                tags$dt("Fallback CSV"),
                tags$dd("Si falla la red, los snapshots Binance 2021-2022 ",
                        "originales del proyecto se cargan desde data/.")
              )
            )
          ),

          bslib::card(
            bslib::card_header(bsicons::bs_icon("cpu"),
                               " Modelos de pronóstico"),
            bslib::card_body(
              tags$dl(class = "alleria-deps",
                tags$dt("ARIMA"),
                tags$dd("forecast::auto.arima sobre serie diaria ts() con ",
                        "frecuencia 7. Selección AICc. Intervalos al ",
                        "80 / 95%."),
                tags$dt("ETS"),
                tags$dd("forecast::ets — descomposición ",
                        "Error / Trend / Seasonal en estado-espacio."),
                tags$dt("Prophet"),
                tags$dd("Bayesiano aditivo (Stan) con estacionalidad ",
                        "semanal y anual. Banda 95% nativa, 80% derivada ",
                        "por cociente de cuantiles normales."),
                tags$dt("Random Forest"),
                tags$dd("ranger sobre 7 lags + SMA7/SMA21 + RSI14 + ",
                        "log-retorno con forecast recursivo y bandas ",
                        "cuantílicas (quantreg).")
              )
            )
          )
        ),

        tags$br(),

        bslib::card(
          bslib::card_header(bsicons::bs_icon("palette"),
                             " Sistema visual · Crypto Aurora"),
          bslib::card_body(
            tags$p("La paleta combina la noche profunda de las terminales ",
                   "de exchange (#0E1116) con el dorado Bitcoin/Doge ",
                   "(#F2A900), el verde mint para movimientos alcistas ",
                   "(#3FB950) y el crimson para los bajistas (#F85149). El ",
                   "azul eléctrico (#58A6FF) y el violeta (#A371F7) actúan ",
                   "como acentos secundarios. La tipografía sigue la línea ",
                   "editorial del portfolio: ", tags$b("Crimson Pro"),
                   " para titulares serif, ", tags$b("Inter"),
                   " para texto y ", tags$b("JetBrains Mono"),
                   " para código y etiquetas."),
            tags$p(class = "alleria-muted",
              "El árbol celta enlaza esta app con el resto de proyectos ",
              "independientes de Angnar (rnaseqr · firesr · ecorangr · ",
              "phytora · andera).")
          )
        )
      )
    )
  ),

  bslib::nav_spacer(),

  # =========================================================================
  # Tab 7 · Contacto
  # =========================================================================
  bslib::nav_panel(
    title = tagList(bsicons::bs_icon("envelope"), " Contacto"),
    value = "contact",

    tags$section(class = "alleria-page",
      tags$div(class = "alleria-page-inner",
        tags$span(class = "alleria-eyebrow", "Contacto"),
        tags$h1(class = "alleria-page-title", "Contacto y créditos"),

        bslib::layout_columns(
          col_widths = c(6, 6),

          bslib::card(
            bslib::card_header(bsicons::bs_icon("person-circle"), " Autoría"),
            bslib::card_body(
              tags$p(tags$strong("Alejandro Navas González"),
                     " (", tags$em("Angnar"), ")"),
              tags$p("Bioinformático y científico de datos. Desarrollo de ",
                     "herramientas interactivas en R / Shiny para análisis ",
                     "ómico, ambiental y, ahora, financiero-cripto."),
              tags$p(
                tags$a(href = "mailto:angnar@telaris.es",
                       bsicons::bs_icon("envelope"), " angnar@telaris.es"),
                tags$br(),
                tags$a(href = "https://github.com/Angnar-97",
                       target = "_blank", rel = "noopener",
                       bsicons::bs_icon("github"),
                       " @Angnar-97 en GitHub")
              )
            )
          ),

          bslib::card(
            bslib::card_header(bsicons::bs_icon("file-earmark-code"),
                               " Licencia y código"),
            bslib::card_body(
              tags$p("alleria se distribuye bajo licencia ",
                     tags$b("MIT"),
                     ". Originalmente publicada como doge_whisperer en ",
                     "2022 con un stack mixto Python/Keras, esta iteración ",
                     "la moderniza por completo en R puro y la rebautiza ",
                     "en honor a ", tags$em("Alleria Windrunner"),
                     ", la cazadora de WoW."),
              tags$p("Forma parte del portfolio independiente de Angnar ",
                     "junto a ", tags$b("rnaseqr"), ", ",
                     tags$b("firesr"), ", ", tags$b("ecorangr"),
                     ", ", tags$b("phytora"), " y ", tags$b("andera"),
                     ". No está afiliado a ninguna empresa.")
            )
          )
        )
      )
    )
  )
)
