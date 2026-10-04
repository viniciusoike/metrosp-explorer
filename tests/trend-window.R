source("global.R", encoding = "UTF-8")
server <- source("server.R", encoding = "UTF-8")$value

if (!HAS_TRENDSERIES) {
  cli::cli_abort("trendseries is required for the trend-window regression.")
}

check_window <- function(actual, full, period) {
  expected <- full |> filter(date >= period_start(period))
  if (!"trend_stl" %in% names(actual) || !any(is.finite(actual$trend_stl))) {
    cli::cli_abort("The {period} window must retain an estimated STL trend.")
  }
  if (!isTRUE(all.equal(actual, expected))) {
    cli::cli_abort("Changing the display period must not change fitted trends.")
  }
}

# Missing months and values split the fit without changing observed data.
dates <- seq(as.Date("2010-01-01"), by = "month", length.out = 120)
fixture <- tibble::tibble(
  date = dates,
  value = 100 + seq_along(dates) + 10 * sin(seq_along(dates) * pi / 6)
)
fixture$value[100] <- NA_real_
fixture <- fixture[-40, ]
segmented <- augment_monthly_stl(fixture)
stopifnot(
  identical(segmented$date, fixture$date),
  identical(segmented$value, fixture$value),
  all(is.finite(segmented$trend_stl[segmented$date < dates[40]])),
  all(is.finite(segmented$trend_stl[
    segmented$date > dates[40] & segmented$date < dates[100]
  ])),
  all(is.na(segmented$trend_stl[segmented$date >= dates[100]]))
)

shiny::testServer(server, {
  session$setInputs(
    lines_line = "1",
    lines_metric = "transported",
    lines_period = "inicio",
    lines_trend = TRUE,
    sta_line = "1",
    sta_station = "ana-rosa",
    sta_period = "inicio",
    sta_trend = TRUE
  )
  full_lines <- lines_data()
  full_station <- sta_monthly_data()

  for (period in c("12m", "24m")) {
    session$setInputs(lines_period = period, sta_period = period)
    check_window(lines_data(), full_lines, period)
    check_window(sta_monthly_data(), full_station, period)
  }

  session$setInputs(lines_metric = "entrance", lines_period = "inicio")
  full_entries <- lines_data()
  session$setInputs(lines_period = "12m")
  check_window(lines_data(), full_entries, "12m")

  session$setInputs(
    lines_period = "24m",
    lines_trend = FALSE,
    sta_trend = FALSE
  )
  stopifnot(
    !"trend_stl" %in% names(lines_data()),
    !"trend_stl" %in% names(sta_monthly_data()),
    all(lines_data()$date >= period_start("24m")),
    all(sta_monthly_data()$date >= period_start("24m"))
  )

  session$setInputs(sta_line = "5", sta_station = "aacd-servidor")
  stopifnot(nrow(sta_monthly_data()) == 0L)
})

cli::cli_alert_success("STL display-window contract passed.")
