# alleria

> Cazadora de mercados cripto, en R.

Aplicación Shiny independiente para análisis e inferencia sobre criptodivisas
con scraping en vivo, indicadores técnicos clásicos, modelos de pronóstico
puramente en R y visualización editorial sobre la paleta **Crypto Aurora**.

Originalmente publicada en 2022 como `doge_whisperer` con un stack mixto
Python/Keras y CSV cacheados desde Google Drive, esta iteración la
moderniza por completo: 100 % R, datos en tiempo real, modelos estadísticos
clásicos, UI tematizada con `bslib` y rebautizada en honor a *Alleria
Windrunner*, la cazadora alta elfa de WoW (la metáfora encaja: rastrear
mercados, leer las velas con ojo de Ranger y disparar pronósticos al
horizonte).

## Demo histórica

https://alejandrongs7.shinyapps.io/dogewhisperer/

(la versión vieja como `doge_whisperer`, mientras se redespliega esta refactorización)

## Características

- **Mercado vivo** vía CoinGecko (precio, market cap, volumen, 1h / 24h / 7d / 30d, sparklines).
- **OHLCV diario** desde Yahoo Finance (`quantmod::getSymbols`).
- **Indicadores técnicos**: SMA, EMA, RSI(14) — `TTR`.
- **Visualización interactiva** con plotly: candlesticks + volumen, retornos
  acumulados normalizados, drawdowns, heatmap de correlación, heatmap calendario,
  distribución de retornos diarios y treemap de capitalización.
- **Pronóstico** con cuatro motores en R:
  - ARIMA (`forecast::auto.arima`)
  - ETS (`forecast::ets`)
  - Prophet (`prophet::prophet`) — banda 95 % nativa, 80 % derivada por cociente de cuantiles normales
  - Random Forest (`ranger`) sobre 7 lags + SMA7/SMA21 + RSI14 + log-retorno con forecast recursivo y bandas cuantílicas
- **Caché en disco** con `cachem` + `memoise`, TTL de 5 minutos para no agotar
  los rate limits de CoinGecko.
- **Sin API keys**, sin Google Drive, sin Python.

## Pestañas

1. **Portada** · hero con árbol celta + KPIs vivos + workflow (rastrea / observa / apunta / dispara) + capacidades.
2. **Mercado** · treemap de mcap, KPIs agregados y tabla con sparklines.
3. **Velas** · candlesticks Yahoo con SMA configurables y RSI(14).
4. **Comparador** · retornos normalizados, drawdowns, correlación, heatmap calendario y distribución.
5. **Pronóstico** · selector de modelo + horizonte + métricas + fan chart.
6. **Metodología** · fuentes, modelos y sistema visual.
7. **Contacto** · autoría y créditos.

## Arquitectura

```
alleria/
├── global.R             # paquetes, paleta, tema bslib, catálogo, caché
├── ui.R                 # page_navbar editorial
├── server.R             # lógica reactiva
├── R/
│   ├── data_fetch.R     # CoinGecko (httr2) + Yahoo (quantmod)
│   ├── models.R         # ARIMA / ETS / Prophet / Random Forest
│   └── plots.R          # 10 visualizaciones plotly + theme_aurora
├── www/
│   ├── celta.png        # logo (árbol celta · portfolio Angnar)
│   └── alleria.css
└── data/                # CSVs Binance 2021-2022 (fallback offline)
```

## Stack

- shiny · bslib · bsicons · shinyWidgets · shinycssloaders · DT
- httr2 · jsonlite · cachem · memoise
- quantmod · TTR · forecast · prophet (opcional) · ranger (opcional)
- plotly · ggplot2 · scales · lubridate · tidyverse

## Ejecución

```r
# Desde la raíz del proyecto:
shiny::runApp(".")
```

Requiere R ≥ 4.2. Si faltan paquetes el `global.R` aborta con un mensaje
explícito listando exactamente cuáles instalar.

## Paleta · Crypto Aurora

| Token | Hex | Uso |
|---|---|---|
| `bg` | `#0E1116` | canvas nocturno tipo terminal exchange |
| `surface` / `surface_alt` | `#161B22` / `#1F2731` | tarjetas |
| `gold` | `#F2A900` | primario · Doge / Bitcoin |
| `mint` | `#3FB950` | alcista |
| `crimson` | `#F85149` | bajista |
| `electric` | `#58A6FF` | info |
| `violet` | `#A371F7` | acento |
| `ink` / `muted` | `#E6EDF3` / `#8B949E` | texto |

Tipografía: **Crimson Pro** (display serif) + **Inter** (body) + **JetBrains Mono** (code).

## Autoría

**Alejandro Navas González** ([Angnar](https://github.com/Angnar-97))
· angnar@telaris.es

Proyecto independiente del portfolio personal de Angnar — no afiliado a
ninguna empresa. Comparte el árbol celta (`celta.png`) con el resto de
proyectos: rnaseqr · firesr · ecorangr · phytora · andera.

## Licencia

MIT © 2026 Alejandro Navas González.
