source("global.R", encoding = "UTF-8")

tabular <- list(
  line_entries_monthly = line_entries_monthly,
  line_transported_monthly = line_transported_monthly,
  station_transported_monthly = station_transported_monthly,
  station_entries_daily = station_entries_daily
)

for (dataset in names(tabular)) {
  csv_path <- tempfile(fileext = ".csv")
  xlsx_path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(csv_path, xlsx_path)), add = TRUE)

  readr::write_excel_csv2(tabular[[dataset]], csv_path)
  writexl::write_xlsx(tabular[[dataset]], xlsx_path)

  csv <- readr::read_csv2(csv_path, show_col_types = FALSE)
  if (!identical(names(csv), names(tabular[[dataset]]))) {
    cli::cli_abort("CSV columns do not match {.val {dataset}}.")
  }
  if (file.info(xlsx_path)$size <= 0) {
    cli::cli_abort("Excel download for {.val {dataset}} is empty.")
  }
}

spatial <- list(
  rail_lines = metrosp::rail_lines,
  rail_stations = metrosp::rail_stations
)

for (dataset in names(spatial)) {
  for (driver in c("GPKG", "GeoJSON")) {
    ext <- if (identical(driver, "GPKG")) ".gpkg" else ".geojson"
    path <- tempfile(fileext = ext)
    on.exit(unlink(path), add = TRUE)

    sf::st_write(
      spatial[[dataset]],
      path,
      driver = driver,
      quiet = TRUE,
      delete_dsn = TRUE
    )
    downloaded <- sf::st_read(path, quiet = TRUE)
    source_columns <- setdiff(
      names(spatial[[dataset]]),
      attr(spatial[[dataset]], "sf_column")
    )
    downloaded_columns <- setdiff(
      names(downloaded),
      attr(downloaded, "sf_column")
    )
    if (!identical(downloaded_columns, source_columns)) {
      cli::cli_abort(
        "{.val {driver}} columns do not match {.val {dataset}}."
      )
    }
  }
}

cli::cli_alert_success("Download schema contract passed.")
