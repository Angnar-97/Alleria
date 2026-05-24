# ============================================================================
# R/models.R · modelos de pronóstico 100% R
# ----------------------------------------------------------------------------
# Sustituye el stack Python/Keras/LSTM original por modelos clásicos en R:
#   · ARIMA          -> forecast::auto.arima  (lineal, robusto)
#   · ETS            -> forecast::ets         (estado-espacio exponencial)
#   · Prophet        -> prophet::prophet      (tendencia + estacionalidad)
#   · Random Forest  -> ranger sobre ventanas (lags) + features técnicos
#
# Todas las funciones devuelven una lista homogénea:
#   list(model = ..., fitted = tibble(date, fitted), forecast = tibble(date,
#        forecast, lo80, hi80, lo95, hi95), metrics = list(R2, MAE, RMSE,
#        MAPE), label = "ARIMA(p,d,q)")
# ============================================================================

.metrics <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  actual <- actual[ok]; predicted <- predicted[ok]
  if (length(actual) < 2) {
    return(list(R2 = NA_real_, MAE = NA_real_, RMSE = NA_real_, MAPE = NA_real_))
  }
  resid <- actual - predicted
  ss_res <- sum(resid ^ 2)
  ss_tot <- sum((actual - mean(actual)) ^ 2)
  list(
    R2   = 1 - ss_res / ss_tot,
    MAE  = mean(abs(resid)),
    RMSE = sqrt(mean(resid ^ 2)),
    MAPE = mean(abs(resid / actual)) * 100
  )
}

# ---- ARIMA ------------------------------------------------------------------
fit_arima <- function(df, horizon = 14, level = c(80, 95)) {
  stopifnot(all(c("date", "price") %in% names(df)))
  df <- dplyr::arrange(df, date)
  y  <- stats::ts(df$price, frequency = 7)

  fit <- forecast::auto.arima(y, stepwise = TRUE, approximation = FALSE)
  fc  <- forecast::forecast(fit, h = horizon, level = level)

  fitted_tbl <- tibble::tibble(
    date   = df$date,
    fitted = as.numeric(stats::fitted(fit))
  )
  future_dates <- seq.Date(max(df$date) + 1L, by = "day", length.out = horizon)
  forecast_tbl <- tibble::tibble(
    date     = future_dates,
    forecast = as.numeric(fc$mean),
    lo80     = as.numeric(fc$lower[, 1]),
    hi80     = as.numeric(fc$upper[, 1]),
    lo95     = as.numeric(fc$lower[, 2]),
    hi95     = as.numeric(fc$upper[, 2])
  )
  list(
    model    = fit,
    fitted   = fitted_tbl,
    forecast = forecast_tbl,
    metrics  = .metrics(df$price, fitted_tbl$fitted),
    label    = sprintf("ARIMA(%d,%d,%d)",
                       fit$arma[1], fit$arma[6], fit$arma[2])
  )
}

# ---- ETS --------------------------------------------------------------------
fit_ets <- function(df, horizon = 14, level = c(80, 95)) {
  df <- dplyr::arrange(df, date)
  y  <- stats::ts(df$price, frequency = 7)
  fit <- forecast::ets(y)
  fc  <- forecast::forecast(fit, h = horizon, level = level)

  fitted_tbl <- tibble::tibble(
    date   = df$date,
    fitted = as.numeric(stats::fitted(fit))
  )
  future_dates <- seq.Date(max(df$date) + 1L, by = "day", length.out = horizon)
  forecast_tbl <- tibble::tibble(
    date     = future_dates,
    forecast = as.numeric(fc$mean),
    lo80     = as.numeric(fc$lower[, 1]),
    hi80     = as.numeric(fc$upper[, 1]),
    lo95     = as.numeric(fc$lower[, 2]),
    hi95     = as.numeric(fc$upper[, 2])
  )
  list(
    model    = fit,
    fitted   = fitted_tbl,
    forecast = forecast_tbl,
    metrics  = .metrics(df$price, fitted_tbl$fitted),
    label    = paste0("ETS(", fit$method, ")")
  )
}

# ---- Prophet ----------------------------------------------------------------
# Prophet sólo expone una banda configurable por `interval.width`. Para tener
# bandas 80 y 95 simultáneas derivamos la 80 de la 95 vía cociente de
# cuantiles normales:  z(0.975) ≈ 1.960  ·  z(0.90) ≈ 1.282  →  ratio ≈ 0.654.
fit_prophet <- function(df, horizon = 14) {
  if (!requireNamespace("prophet", quietly = TRUE)) {
    stop("El paquete 'prophet' no está disponible. Instala prophet o elige otro modelo.")
  }
  df <- dplyr::arrange(df, date)
  ds <- data.frame(ds = as.Date(df$date), y = df$price)

  m <- prophet::prophet(ds,
                        daily.seasonality  = FALSE,
                        weekly.seasonality = TRUE,
                        yearly.seasonality = TRUE,
                        interval.width     = 0.95)
  future <- prophet::make_future_dataframe(m, periods = horizon)
  fc <- predict(m, future)

  fitted_tbl <- tibble::tibble(
    date   = as.Date(fc$ds[seq_len(nrow(df))]),
    fitted = fc$yhat[seq_len(nrow(df))]
  )

  fc_tail <- utils::tail(fc, horizon)
  half95  <- (fc_tail$yhat_upper - fc_tail$yhat_lower) / 2
  half80  <- half95 * (stats::qnorm(0.90) / stats::qnorm(0.975))

  forecast_tbl <- tibble::tibble(
    date     = as.Date(fc_tail$ds),
    forecast = fc_tail$yhat,
    lo80     = fc_tail$yhat - half80,
    hi80     = fc_tail$yhat + half80,
    lo95     = fc_tail$yhat_lower,
    hi95     = fc_tail$yhat_upper
  )
  list(
    model    = m,
    fitted   = fitted_tbl,
    forecast = forecast_tbl,
    metrics  = .metrics(df$price, fitted_tbl$fitted),
    label    = "Prophet"
  )
}

# ---- Random Forest sobre lags + features técnicos ----------------------------
# Constructor de features: dado un vector y[1..n] devuelve la fila de
# predictores asociada al instante n+1 (es decir, los predictores que se
# observan justo antes de t+1). Esto se usa tanto para construir el set de
# entrenamiento como para iterar el pronóstico recursivo.
.rf_row <- function(y, lags = 1:7) {
  n <- length(y)
  if (n < max(lags) + 21) return(NULL)  # necesita al menos 21 días para SMA21
  row <- tibble::tibble(!!!setNames(
    lapply(lags, function(k) y[n - k + 1]),
    paste0("lag_", lags)
  ))
  row$sma7  <- mean(utils::tail(y, 7),  na.rm = TRUE)
  row$sma21 <- mean(utils::tail(y, 21), na.rm = TRUE)
  rsi <- TTR::RSI(y, 14)
  row$rsi14 <- as.numeric(utils::tail(rsi, 1))
  row$ret1  <- log(y[n] / y[n - 1])
  row
}

fit_rf <- function(df, horizon = 14, lags = 1:7) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("El paquete 'ranger' no está disponible. Instala ranger o elige otro modelo.")
  }
  df <- dplyr::arrange(df, date)
  y  <- df$price
  if (length(y) < max(lags) + 22) {
    stop("Histórico insuficiente para Random Forest (mínimo ", max(lags) + 22,
         " días).")
  }

  # Set de entrenamiento: para cada t > max(lags)+21 calculamos los
  # predictores con y[1..t-1] y la respuesta y[t].
  start <- max(lags) + 21L
  rows <- purrr::map_dfr(seq.int(start, length(y) - 1L), function(i) {
    feats <- .rf_row(y[seq_len(i)], lags)
    if (is.null(feats)) return(NULL)
    feats$y    <- y[i + 1L]
    feats$date <- df$date[i + 1L]
    feats
  })
  rows <- na.omit(rows)
  if (nrow(rows) < 30) stop("Pocas filas tras construir features (",
                            nrow(rows), ").")

  fit <- ranger::ranger(
    y ~ . - date,
    data       = rows,
    num.trees  = 500,
    importance = "impurity",
    keep.inbag = TRUE,
    quantreg   = TRUE
  )
  pred_in    <- predict(fit, rows)$predictions
  fitted_tbl <- tibble::tibble(date = rows$date, fitted = pred_in)

  # Pronóstico recursivo: en cada paso construimos la fila con TODAS las
  # features (lags + sma + rsi + ret1) y predecimos cuantiles.
  yhat  <- y
  dates <- df$date
  fc_rows <- vector("list", horizon)
  for (i in seq_len(horizon)) {
    feats <- .rf_row(yhat, lags)
    qpred <- predict(fit, feats, type = "quantiles",
                     quantiles = c(0.025, 0.10, 0.50, 0.90, 0.975))$predictions
    pred  <- predict(fit, feats)$predictions
    yhat  <- c(yhat,  as.numeric(pred))
    dates <- c(dates, max(dates) + 1L)
    fc_rows[[i]] <- list(forecast = as.numeric(pred),
                         lo95 = qpred[1], lo80 = qpred[2],
                         hi80 = qpred[4], hi95 = qpred[5])
  }
  fc_df <- purrr::map_dfr(fc_rows, tibble::as_tibble)
  forecast_tbl <- tibble::tibble(
    date     = utils::tail(dates, horizon),
    forecast = fc_df$forecast,
    lo80     = fc_df$lo80, hi80 = fc_df$hi80,
    lo95     = fc_df$lo95, hi95 = fc_df$hi95
  )
  list(
    model    = fit,
    fitted   = fitted_tbl,
    forecast = forecast_tbl,
    metrics  = .metrics(rows$y, fitted_tbl$fitted),
    label    = sprintf("RandomForest (ranger · %d árboles)", fit$num.trees)
  )
}

# ---- Despacho central --------------------------------------------------------
fit_model <- function(df, model = c("ARIMA", "ETS", "Prophet", "RandomForest"),
                      horizon = 14) {
  model <- match.arg(model)
  switch(model,
    ARIMA        = fit_arima(df,   horizon = horizon),
    ETS          = fit_ets(df,     horizon = horizon),
    Prophet      = fit_prophet(df, horizon = horizon),
    RandomForest = fit_rf(df,      horizon = horizon)
  )
}
