# Rolling-origin backtest for the line forecasts ----
# Refits the ensemble at every month-end origin with at least
# FORECAST_MIN_OBS months of history, for every line and metric. Collects
# log errors by horizon, standardized by each fit's residual scale, and
# writes their pooled quantiles to data/forecast-quantiles.csv. The app
# reads that file to draw forecast intervals.
#
# Takes a few minutes. Rerun when the model or the fit window changes, or
# every few months as new data accumulates.
#
# Run from the app root: Rscript tools/forecast-backtest.R

source("global.R", encoding = "UTF-8")

probs <- c(0.5 - FORECAST_LEVEL / 200, 0.5 + FORECAST_LEVEL / 200)

## Collect errors ----

backtest_series <- function(d) {
  d <- d |> filter(!is.na(value)) |> arrange(date)
  fit_months <- d$date[d$date >= FORECAST_START]
  if (length(fit_months) <= FORECAST_MIN_OBS) {
    return(NULL)
  }
  origins <- fit_months[FORECAST_MIN_OBS:(length(fit_months) - 1)]

  errors <- lapply(origins, function(origin) {
    pt <- forecast_point(d |> filter(date <= origin))
    if (is.null(pt)) {
      return(NULL)
    }
    actual <- d$value[match(pt$date, d$date)]
    monthly <- tibble::tibble(
      target = "month",
      h = seq_along(pt$date),
      z = (log(actual) - pt$log_mean) / pt$scale
    ) |>
      filter(!is.na(z))
    # the 12-month sum needs the whole horizon observed
    sum12 <- if (all(!is.na(actual))) {
      tibble::tibble(
        target = "sum12",
        h = FORECAST_H,
        z = (log(sum(actual)) - log(sum(exp(pt$log_mean)))) / pt$scale
      )
    }
    return(bind_rows(monthly, sum12) |> mutate(origin = origin))
  })
  return(bind_rows(errors))
}

series <- bind_rows(entrance = ent, transported = trans, .id = "metric")
keys <- series |> distinct(metric, line_number)

errors <- lapply(seq_len(nrow(keys)), function(i) {
  key <- keys[i, ]
  cli::cli_inform("Backtesting {key$metric}, line {key$line_number}.")
  d <- series |>
    filter(metric == key$metric, line_number == key$line_number)
  out <- backtest_series(d)
  if (is.null(out)) {
    return(NULL)
  }
  return(out |> mutate(metric = key$metric, line_number = key$line_number))
}) |>
  bind_rows()

## Pool quantiles ----

quantiles <- errors |>
  group_by(target, h) |>
  summarise(
    q_lower = stats::quantile(z, probs[1], names = FALSE),
    q_upper = stats::quantile(z, probs[2], names = FALSE),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(target, h) |>
  # uncertainty should not shrink with the horizon; this also smooths the
  # noisier long-horizon quantiles, which rest on fewer errors
  group_by(target) |>
  mutate(q_lower = cummin(q_lower), q_upper = cummax(q_upper)) |>
  ungroup()

readr::write_csv(quantiles, "data/forecast-quantiles.csv")

coverage <- errors |>
  left_join(quantiles, by = c("target", "h")) |>
  group_by(target, line_number) |>
  summarise(coverage = mean(z >= q_lower & z <= q_upper), .groups = "drop")

cli::cli_alert_success(
  "Wrote {nrow(quantiles)} quantile rows from {nrow(errors)} errors."
)
print(quantiles)
print(coverage)
