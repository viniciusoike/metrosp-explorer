source("global.R", encoding = "UTF-8")

calls <- character()
fake_reader <- function(dataset, source, quiet) {
  calls <<- c(calls, paste(dataset, source, sep = ":"))
  if (identical(source, "remote") && identical(dataset, DEMAND_DATASETS[[3]])) {
    cli::cli_abort("simulated remote failure")
  }
  return(data.frame(dataset = dataset, source = source))
}

fallback <- suppressWarnings(load_demand_data(fake_reader))
expected_bundled <- paste(DEMAND_DATASETS, "bundled", sep = ":")

if (!identical(attr(fallback, "source"), "bundled")) {
  cli::cli_abort("A failed remote read must select the bundled source.")
}
if (!all(expected_bundled %in% calls)) {
  cli::cli_abort("A failed remote read must reload all four bundled tables.")
}
if (
  !all(vapply(fallback, function(x) x$source[[1]], character(1)) == "bundled")
) {
  cli::cli_abort("The fallback demand set mixes remote and bundled tables.")
}

line5_daily_only <- anti_join(
  sta_daily |> distinct(line_number, station_id),
  sta_avg |> distinct(line_number, station_id),
  by = c("line_number", "station_id")
) |>
  filter(line_number == "5")

if (nrow(line5_daily_only) == 0) {
  cli::cli_abort("Expected at least one daily-only Line 5 station.")
}
if (!all(line5_daily_only$station_id %in% stations_by_line$station_id)) {
  cli::cli_abort("Daily-only stations must remain selectable.")
}

complex_rows <- sf_stations_map |>
  sf::st_drop_geometry() |>
  filter(station_id == "consolacao-paulista")
if (nrow(sf_stations_map) != dplyr::n_distinct(sf_stations$station_id)) {
  cli::cli_abort("The map must contain one marker per station complex ID.")
}
if (nrow(complex_rows) != 1L || complex_rows$n_lines != 2L) {
  cli::cli_abort("Consolação and Paulista must render as one two-line marker.")
}
if (
  !grepl("Consolação", complex_rows$popup_html, fixed = TRUE) ||
    !grepl("Paulista", complex_rows$popup_html, fixed = TRUE)
) {
  cli::cli_abort("The shared complex popup must retain both station names.")
}

cli::cli_alert_success("App data-boundary contract passed.")
