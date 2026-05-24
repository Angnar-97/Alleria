# ============================================================================
# Alleria · global.R
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
    "Alleria: faltan paquetes requeridos -> ",
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
# Paleta "Silvermoon" — apuesta lunar / elven sobre azul-noche.
# Plata como metal primario (frío), champagne como acento de acción y brand
# (cálido, sustituye al gold Bitcoin sin perder la lectura "metal precioso"),
# mint celadón + crimson rosewood con cast frío para que no chillen sobre la
# base lunar, violeta crepuscular como herencia Void, bone-parchment para
# titulares (warm ivory leído sobre azul-noche = efecto "pergamino bajo
# luz de luna").
silvermoon <- list(
  bg          = "#0A0F1A",   # azul-noche profundo
  bg_deep     = "#060912",   # navbar / footer
  surface     = "#131A28",   # cards
  surface_alt = "#1C2538",   # card-cap / inputs
  hairline    = "#28324A",   # bordes

  ink         = "#E7EAF2",   # texto base (cool)
  ink_soft    = "#BAC2D2",   # texto secundario
  muted       = "#7A8499",   # texto terciario / labels mono

  # Tipografía editorial cálida sobre superficies frías
  moonglow    = "#E4E7F0",   # plata luminosa (highlights de texto)
  bone        = "#DCD6C2",   # warm parchment — H1/eyebrow/iconos nav

  # Metales — el doble eje "luna + alba"
  silver      = "#C4C9D6",   # plata primaria (neutro metal)
  silver_soft = "#DCE0EA",
  champagne   = "#D4B864",   # champán-oro pálido (CTA / brand accent / glow)
  champagne_soft = "#E8D49C",

  # Semánticas trading con cast frío sutil
  mint        = "#5FA579",   # alcista — celadón
  mint_soft   = "#7DBA94",
  crimson     = "#D74E5A",   # bajista — rosewood
  electric    = "#6BA6E0",   # info — azul lunar
  violet      = "#B398E8"    # acento crepuscular — herencia Void
)

# Escala discreta consistente para criptodivisas (10 colores).
# Bitcoin conserva el champagne — el resto se distribuye sobre el arco
# silver-violet-mint-electric para tener variedad sin volver al cliché
# "todo gold" del periodo Doge.
crypto_palette <- c(
  BTC  = "#D4B864",  # champagne
  ETH  = "#B398E8",  # violet crepuscular
  BNB  = "#E8D49C",  # champagne-soft
  SOL  = "#6BA6E0",  # azul lunar
  ADA  = "#5FA579",  # mint celadón
  XRP  = "#9EC5E5",  # azul lunar suave
  DOT  = "#D74E5A",  # rosewood
  LTC  = "#C4C9D6",  # silver
  LINK = "#7DBA94",  # mint claro
  UNI  = "#D49AC8"   # rosa lunar
)

# Tema ggplot editorial — lunar dark
theme_aurora <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = "Inter") +
    theme(
      plot.background    = element_rect(fill = silvermoon$bg, colour = NA),
      panel.background   = element_rect(fill = silvermoon$bg, colour = NA),
      panel.grid.major   = element_line(colour = silvermoon$hairline,
                                        linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = silvermoon$muted),
      axis.title         = element_text(colour = silvermoon$ink_soft),
      plot.title         = element_text(family = "Crimson Pro",
                                        face = "bold", size = base_size * 1.4,
                                        colour = silvermoon$bone),
      plot.subtitle      = element_text(colour = silvermoon$muted,
                                        size = base_size * 0.95),
      plot.caption       = element_text(colour = silvermoon$muted,
                                        size = base_size * 0.8, hjust = 1),
      strip.background   = element_rect(fill = silvermoon$surface_alt,
                                        colour = NA),
      strip.text         = element_text(colour = silvermoon$ink_soft,
                                        face = "bold"),
      legend.background  = element_rect(fill = silvermoon$bg, colour = NA),
      legend.key         = element_rect(fill = silvermoon$bg, colour = NA),
      legend.text        = element_text(colour = silvermoon$ink_soft),
      legend.title       = element_text(colour = silvermoon$ink_soft)
    )
}

theme_set(theme_aurora())

# ---- Tema bslib (Bootstrap 5 · Silvermoon) ----------------------------------
alleria_theme <- bslib::bs_theme(
  version      = 5,
  preset       = NULL,
  bootswatch   = NULL,
  primary      = silvermoon$champagne,   # acción
  secondary    = silvermoon$silver,      # neutro metal
  success      = silvermoon$mint,
  info         = silvermoon$electric,
  warning      = silvermoon$champagne_soft,
  danger       = silvermoon$crimson,
  bg           = silvermoon$bg,
  fg           = silvermoon$ink,
  base_font    = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Crimson Pro"),
  code_font    = bslib::font_google("JetBrains Mono"),

  "body-bg"          = silvermoon$bg,
  "body-color"       = silvermoon$ink,
  "border-radius"    = "0.55rem",
  "border-radius-sm" = "0.4rem",
  "border-radius-lg" = "0.75rem",

  "card-bg"           = silvermoon$surface,
  "card-cap-bg"       = silvermoon$surface_alt,
  "card-border-color" = silvermoon$hairline,

  "navbar-bg"                = silvermoon$bg_deep,
  "navbar-dark-color"        = "rgba(231,234,242,0.72)",
  "navbar-dark-hover-color"  = silvermoon$champagne,
  "navbar-dark-active-color" = silvermoon$champagne,
  "navbar-brand-font-size"   = "1.3rem",

  "input-bg"                = silvermoon$surface_alt,
  "input-color"             = silvermoon$ink,
  "input-border-color"      = silvermoon$hairline,
  "input-focus-border-color"= silvermoon$champagne,
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
  ifelse(is.na(x), silvermoon$muted,
         ifelse(x >= 0, silvermoon$mint, silvermoon$crimson))
}
