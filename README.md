# Alleria

> Cazadora de mercados cripto, en R.

Aplicación Shiny independiente para análisis e inferencia sobre criptodivisas
con scraping en vivo, indicadores técnicos clásicos, modelos de pronóstico
puramente en R y visualización editorial sobre la paleta **Silvermoon**.

Originalmente publicada en 2022 como `doge_whisperer` con un stack mixto
Python/Keras y CSV cacheados desde Google Drive, esta iteración la
moderniza por completo: 100 % R, datos en tiempo real, modelos estadísticos
clásicos, UI tematizada con `bslib` y rebautizada en honor a *Alleria
Windrunner*, la cazadora alta elfa de WoW (la metáfora encaja: rastrear
mercados, leer las velas con ojo de Ranger y disparar pronósticos al
horizonte).

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

## Paleta · Silvermoon

Apuesta lunar / elven sobre azul-noche. Plata como metal primario (frío) +
champagne como acento de acción y brand (cálido, sustituye al gold Bitcoin
sin perder la lectura "metal precioso"). Mint celadón y crimson rosewood con
cast frío sutil para no chillar sobre la base lunar. Violeta crepuscular
como herencia Void. Bone-parchment para titulares — efecto "pergamino bajo
luz de luna". El árbol celta del portfolio se filtra a plata.

| Token | Hex | Uso |
|---|---|---|
| `bg` / `bg_deep` | `#0A0F1A` / `#060912` | azul-noche profundo |
| `surface` / `surface_alt` | `#131A28` / `#1C2538` | tarjetas |
| `hairline` | `#28324A` | bordes |
| `silver` / `silver_soft` | `#C4C9D6` / `#DCE0EA` | metal primario · neutro |
| `champagne` / `champagne_soft` | `#D4B864` / `#E8D49C` | CTA · brand accent · glow celta |
| `mint` | `#5FA579` | alcista — celadón |
| `crimson` | `#D74E5A` | bajista — rosewood |
| `electric` | `#6BA6E0` | info — azul lunar |
| `violet` | `#B398E8` | acento — herencia Void |
| `bone` | `#DCD6C2` | warm parchment · titulares + iconos nav |
| `moonglow` | `#E4E7F0` | highlights luminosos |
| `ink` / `ink_soft` / `muted` | `#E7EAF2` / `#BAC2D2` / `#7A8499` | texto cool |

Tipografía: **Crimson Pro** (display serif) + **Inter** (body) + **JetBrains Mono** (code).

## Autoría

**Alejandro Navas González** ([Angnar](https://github.com/Angnar-97))
· angnar@telaris.es

## Licencia

MIT © 2026 Alejandro Navas González.
