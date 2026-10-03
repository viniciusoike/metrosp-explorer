source("global.R", encoding = "UTF-8")

year_updates <- list()
updateSelectInput <- function(session, inputId, choices, selected, ...) {
  if (identical(inputId, "sta_year")) {
    year_updates[[length(year_updates) + 1L]] <<- list(
      choices = as.character(choices),
      selected = as.character(selected)
    )
  }
}
server <- source("server.R", encoding = "UTF-8")$value

expect_years <- function(line, station_id) {
  expected <- sta_daily_years |>
    filter(line_number == .env$line, station_id == .env$station_id) |>
    pull(year) |>
    as.character()
  actual <- year_updates[[length(year_updates)]]$choices
  if (!identical(actual, expected)) {
    cli::cli_abort("Daily-year choices for line {line} are stale.")
  }
}

shiny::testServer(server, {
  session$setInputs(sta_line = "2")
  session$setInputs(sta_station = "consolacao-paulista")
  expect_years("2", "consolacao-paulista")

  previous_updates <- length(year_updates)
  session$setInputs(sta_line = "4")
  if (length(year_updates) <= previous_updates) {
    cli::cli_abort("Changing to Paulista must refresh the daily-year choices.")
  }
  expect_years("4", "consolacao-paulista")

  previous_updates <- length(year_updates)
  session$setInputs(sta_line = "2")
  if (length(year_updates) <= previous_updates) {
    cli::cli_abort(
      "Changing back to Consolação must refresh the daily-year choices."
    )
  }
  expect_years("2", "consolacao-paulista")

  previous_updates <- length(year_updates)
  session$setInputs(sta_line = "1")
  if (length(year_updates) != previous_updates) {
    cli::cli_abort(
      "Changing to an unrelated line must wait for a valid station."
    )
  }
  line1_station <- stations_by_line$station_id[
    stations_by_line$line_number == "1"
  ][[1]]
  session$setInputs(sta_station = line1_station)
  expect_years("1", line1_station)

  session$setInputs(sta_line = "4", sta_station = "republica")
  expect_years("4", "republica")
  session$setInputs(sta_year = NULL)
  daily <- sta_daily_data()
  if (nrow(daily) != 0L || !identical(names(daily), names(sta_daily))) {
    cli::cli_abort("Monthly-only República must return an empty daily table.")
  }
  if (!identical(output$sta_daily_title, "República — Entradas diárias")) {
    cli::cli_abort("The daily title must omit empty year parentheses.")
  }
  chart_error <- tryCatch(output$sta_daily_chart, error = identity)
  if (
    !inherits(chart_error, "error") ||
      !grepl(
        "Sem dados diários para esta estação",
        conditionMessage(chart_error),
        fixed = TRUE
      )
  ) {
    cli::cli_abort("The daily chart must explain unavailable daily data.")
  }
})

cli::cli_alert_success("Station reactivity contract passed.")
