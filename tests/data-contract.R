required_version <- package_version("2.0.0")
installed_version <- packageVersion("metrosp")

if (installed_version < required_version) {
  cli::cli_abort(c(
    "{.pkg metrosp} 2.0.0 or newer is required.",
    "i" = "Installed version: {installed_version}."
  ))
}

demand_names <- c(
  "line_entries_monthly",
  "line_transported_monthly",
  "station_transported_monthly",
  "station_entries_daily"
)

expected_columns <- list(
  line_entries_monthly = c(
    "date",
    "year",
    "line_number",
    "line_name",
    "line_name_pt",
    "metric",
    "metric_name",
    "metric_name_pt",
    "value"
  ),
  line_transported_monthly = c(
    "date",
    "year",
    "line_number",
    "line_name",
    "line_name_pt",
    "metric",
    "metric_name",
    "metric_name_pt",
    "value"
  ),
  station_transported_monthly = c(
    "date",
    "year",
    "line_number",
    "station_id",
    "station_name",
    "line_name",
    "line_name_pt",
    "metric",
    "metric_name",
    "metric_name_pt",
    "value"
  ),
  station_entries_daily = c(
    "date",
    "year",
    "line_number",
    "station_id",
    "station_name",
    "station_code",
    "line_name",
    "line_name_pt",
    "value"
  )
)

check_columns <- function(dat, dataset) {
  if (!identical(names(dat), expected_columns[[dataset]])) {
    cli::cli_abort(c(
      "{.val {dataset}} does not have the metrosp 2.0 schema.",
      "i" = "Found: {paste(names(dat), collapse = ', ')}."
    ))
  }
}

read_demand_set <- function(source, vintage = "latest") {
  out <- lapply(
    demand_names,
    metrosp::read_metro_demand,
    source = source,
    vintage = vintage,
    cache = FALSE,
    quiet = TRUE
  )
  names(out) <- demand_names
  return(out)
}

check_demand_set <- function(datasets) {
  invisible(Map(check_columns, datasets, names(datasets)))

  line4 <- datasets$line_transported_monthly
  line4_june_2025 <- line4$value[
    line4$line_number == 4L &
      line4$date == as.Date("2025-06-01") &
      line4$metric == "total"
  ]
  if (!identical(line4_june_2025, 16289690)) {
    cli::cli_abort(
      "Line 4 transported demand for June 2025 must equal 16,289,690."
    )
  }

  line4_total <- line4[
    line4$line_number == 4L & line4$metric == "total",
    ,
    drop = FALSE
  ]
  if (min(line4_total$date) != as.Date("2012-01-01")) {
    cli::cli_abort("Line 4 transported coverage must begin in January 2012.")
  }

  line5_monthly <- datasets$station_transported_monthly[
    datasets$station_transported_monthly$line_number == 5L,
    ,
    drop = FALSE
  ]
  if (max(line5_monthly$date) != as.Date("2018-07-01")) {
    cli::cli_abort("Line 5 monthly station coverage must end in July 2018.")
  }
}

bundled <- read_demand_set("bundled")
check_demand_set(bundled)

rolling <- read_demand_set("remote")
check_demand_set(rolling)

# The rolling release uses the 2.0 asset names. The first dated archive still
# uses the 1.x names, so this also exercises the reader's legacy translation.
pinned_legacy <- read_demand_set("remote", vintage = "2026-09")
invisible(Map(check_columns, pinned_legacy, names(pinned_legacy)))
if (any(pinned_legacy$line_entries_monthly$line_number == 99L)) {
  cli::cli_abort("The legacy vintage must drop aggregate line 99 rows.")
}
if (anyNA(pinned_legacy$station_transported_monthly$station_id)) {
  cli::cli_abort("The legacy vintage must receive stable station IDs.")
}

station_platforms <- unique(
  metrosp::rail_stations[
    metrosp::rail_stations$station_name %in% c("Consolação", "Paulista"),
    c("station_id", "station_name", "line_number")
  ]
)
if (
  nrow(station_platforms) != 2L ||
    length(unique(station_platforms$station_id)) != 1L ||
    unique(station_platforms$station_id) != "consolacao-paulista"
) {
  cli::cli_abort("Consolação and Paulista must share one station complex ID.")
}

cli::cli_alert_success("metrosp 2.0 data contract passed.")
