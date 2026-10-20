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

## Intervals come from the backtest error quantiles ----
q <- forecast_quantiles
expect_true(
  is.data.frame(q) &&
    identical(as.integer(q$h), seq_len(FORECAST_H)) &&
    all(q$q_lower < q$q_mid & q$q_mid < q$q_upper),
  "forecast_quantiles must hold lower, median and upper errors for each step."
)
scale <- attr(fc, "scale")
expect_true(
  is.numeric(scale) && length(scale) == 1 && scale > 0,
  "forecast_line() must attach a positive error scale."
)
# the point and both bounds are the ensemble median shifted by the error
# quantiles, so the path recenters on the median backtest error
fc_median <- attr(fc, "median")
expect_true(
  isTRUE(all.equal(log(fc$fc_mean / fc_median) / scale, q$q_mid)) &&
    isTRUE(all.equal(log(fc$fc_upper / fc_median) / scale, q$q_upper)) &&
    isTRUE(all.equal(log(fc$fc_lower / fc_median) / scale, q$q_lower)),
  "The forecast path and bounds must apply the error quantiles to the median."
)

## The 12-month KPI matches the chart and gets its own interval ----
sq <- forecast_sum_quantiles
growth <- forecast_growth_interval(line_1, fc)
expect_true(
  isTRUE(all.equal(growth$point, forecast_growth(line_1, fc))),
  "The KPI point must be the growth of the plotted forecast path."
)
# 12-month-sum errors were measured against the summed ensemble median
median_growth <- forecast_growth(line_1, dplyr::mutate(fc, fc_mean = fc_median))
expected <- ((1 + median_growth / 100) *
  exp(c(sq$q_lower, sq$q_upper) * scale) -
  1) *
  100
expect_true(
  isTRUE(all.equal(c(growth$lower, growth$upper), expected)),
  "The KPI bounds must apply the 12-month-sum error quantiles."
)
expect_true(
  all(c("point", "lower", "upper") %in% names(growth)) &&
    growth$lower < growth$point &&
    growth$point < growth$upper,
  "forecast_growth_interval() must bracket the point growth."
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

## The band tooltip shows both interval bounds ----
# e_band2() ships each point as [date, lower, upper]; the tooltip must quote
# the range, not just the lower bound
tooltip_js <- paste(js_tooltip_pt_br("month"), collapse = "\n")
expect_true(
  grepl("p.value.length >= 3", tooltip_js, fixed = TRUE) &&
    grepl("fmtN(p.value[2])", tooltip_js, fixed = TRUE),
  "The tooltip must show the lower and upper bounds of band series."
)

## A line forecast does not break the station tab ----
server <- source("server.R", encoding = "UTF-8")$value
shiny::testServer(server, {
  session$setInputs(
    lines_line = "1",
    lines_metric = "entrance",
    lines_period = "inicio",
    lines_forecast = TRUE,
    sta_line = "1",
    sta_station = stations_by_line$station_id[
      stations_by_line$line_number == "1"
    ][[1]]
  )
  # the station observer freezes sta_year until the client sends one
  session$setInputs(sta_year = "")
  expect_true(
    !is.null(lines_forecast()),
    "The test needs an active line forecast."
  )
  kpis <- tryCatch(output$sta_kpis, error = identity)
  expect_true(
    !inherits(kpis, "error"),
    "Station KPIs must render while a line forecast is active."
  )
  expect_true(
    grepl("kpi-grid-4", kpis$html, fixed = TRUE),
    "Station KPIs keep the four-card grid."
  )
})

## The navbar omits the data-source label ----
navbar <- as.character(
  htmltools::renderTags(
    source("ui.R", encoding = "UTF-8")$value(NULL)
  )$html
)
source_tag <- regmatches(
  navbar,
  regexpr("<span class=\"source-tag\">.*?</span>", navbar)
)
expect_true(
  length(source_tag) == 1 &&
    !grepl(DATA_SOURCE_LABEL, source_tag, fixed = TRUE),
  "The navbar source tag must not show the data-source label."
)

cli::cli_alert_success("Forecast checks passed.")
