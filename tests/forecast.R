source("global.R", encoding = "UTF-8")

expect_true <- function(ok, msg) {
  if (!isTRUE(ok)) {
    cli::cli_abort(msg)
  }
}

line_1 <- ent |> filter(line_number == "1")
last_obs <- max(line_1$date[!is.na(line_1$value)])
fc <- forecast_line(line_1)

## The ensemble returns a well-formed 12-month path ----
expect_true(
  is.data.frame(fc) && nrow(fc) == FORECAST_H,
  "The forecast must have {FORECAST_H} rows."
)
expect_true(
  identical(
    fc$date,
    seq(last_obs, by = "month", length.out = FORECAST_H + 1)[-1]
  ),
  "Forecast dates must be the months after the last observation."
)
expect_true(
  all(fc$fc_lower > 0) &&
    all(fc$fc_lower <= fc$fc_mean) &&
    all(fc$fc_mean <= fc$fc_upper),
  "Intervals must be positive and contain the point forecast."
)
# a sane forecast stays in the range of recent demand
recent <- line_1 |> filter(date > last_obs - 365)
expect_true(
  all(fc$fc_mean > 0.5 * min(recent$value)) &&
    all(fc$fc_mean < 1.5 * max(recent$value)),
  "The point forecast drifts far from recent demand."
)

## The fit window ignores the pandemic years ----
# Changing pre-2022 values must not move the forecast
shocked <- line_1 |>
  mutate(value = if_else(date < FORECAST_START, value * 10, value))
expect_true(
  isTRUE(all.equal(fc, forecast_line(shocked))),
  "Data before FORECAST_START must not enter the fit."
)

## Calendar regressors cover the fit window and the horizon ----
expect_true(
  all(c("bdays", "sats") %in% names(calendar_monthly)) &&
    all(calendar_monthly$bdays > 0),
  "calendar_monthly must hold business-day and Saturday counts."
)
expect_true(
  is.null(forecast_line(line_1, h = 12L * 20L)),
  "A horizon past the calendar must return NULL."
)

## Short, ended or gappy series return NULL ----
short <- line_1 |> filter(date >= last_obs - 365 * 2)
expect_true(
  is.null(forecast_line(short)),
  "Series with fewer than {FORECAST_MIN_OBS} months must return NULL."
)
expect_true(
  is.null(forecast_line(trans |> filter(line_number == "5"))),
  "A series with no data after FORECAST_START must return NULL."
)
gappy <- line_1 |> filter(date != as.Date("2024-06-01"))
expect_true(
  is.null(forecast_line(gappy)),
  "Series with missing months in the fit window must return NULL."
)

## Forecast growth compares the next 12 months with the last 12 ----
toy_obs <- tibble::tibble(
  date = seq(as.Date("2024-01-01"), by = "month", length.out = 24),
  value = 100
)
toy_fc <- tibble::tibble(
  date = seq(as.Date("2026-01-01"), by = "month", length.out = 12),
  fc_mean = 110
)
expect_true(
  isTRUE(all.equal(forecast_growth(toy_obs, toy_fc), 10)),
  "forecast_growth() must return the percent change of the 12-month sums."
)

cli::cli_alert_success("Forecast checks passed.")
