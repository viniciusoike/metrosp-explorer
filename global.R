# Global setup for the Metro SP explorer ----
# Loaded once at app startup and shared by ui.R and server.R. Shiny sources
# this file as UTF-8, so accented strings and em-dashes are tagged correctly
# (a plain source() would leave them "unknown" and the widgets would mangle
# them — that was the old shared.R bug).

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(leaflet)
library(echarts4r)
library(metrosp)
library(sf)
library(htmltools)
library(writexl)
library(readr)

# Ensure a UTF-8 locale ----
# Accented strings and em-dashes ("—") only survive renderText()'s native
# encoding conversion under a UTF-8 locale; a C/ASCII locale turns "—" into a
# literal "<U+2014>". Try a few common UTF-8 locales; harmless where one is
# already active, and a no-op if none can be set.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (loc in c("en_US.UTF-8", "C.UTF-8", "en_US.utf8", "C.utf8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
  }
}

enableBookmarking("url")

# Line metadata ----

line_colors <- c(
  "1" = "#171796",
  "2" = "#007A5E",
  "3" = "#ED2E38",
  "4" = "#B89000",
  "5" = "#874ABF",
  "15" = "#6B6B68"
)

line_labels <- c(
  "1" = "Linha 1 — Azul",
  "2" = "Linha 2 — Verde",
  "3" = "Linha 3 — Vermelha",
  "4" = "Linha 4 — Amarela",
  "5" = "Linha 5 — Lilás",
  "15" = "Linha 15 — Prata"
)

line_short <- c(
  "1" = "Azul",
  "2" = "Verde",
  "3" = "Vermelha",
  "4" = "Amarela",
  "5" = "Lilás",
  "15" = "Prata"
)

LINES <- names(line_labels)

if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

metro_primary <- "#171796"

# Formatting helpers ----

fmt_n <- function(x) {
  if (!length(x) || all(is.na(x))) return("—")
  x <- x[!is.na(x)][1]
  if (x >= 1e9) sprintf("%.2f bi", x / 1e9)
  else if (x >= 1e6) sprintf("%.1f M", x / 1e6)
  else if (x >= 1e3) sprintf("%.1f K", x / 1e3)
  else formatC(round(x), format = "d", big.mark = ".")
}

fmt_pct <- function(x, signed = TRUE) {
  if (is.na(x)) return("—")
  if (signed) sprintf("%+.1f%%", x) else sprintf("%.1f%%", x)
}

kpi_card <- function(label, value, sub = NULL) {
  div(
    class = "kpi",
    div(class = "kpi-label", label),
    div(class = "kpi-value", value),
    if (!is.null(sub)) div(class = "kpi-sub", sub)
  )
}

# Trailing k-observation moving average. Operates on observations, not
# calendar days: gaps in the underlying dates are not gap-aware. NA-tolerant
# within each window; returns NA for the first k-1 positions, and all-NA when
# fewer than k observations are available (guards against seq() inverting).
roll_mean <- function(x, k = 7L) {
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n < k) return(out)
  for (i in seq.int(k, n)) out[i] <- mean(x[(i - k + 1L):i], na.rm = TRUE)
  out
}

MONTHS_PT <- c(
  "jan",
  "fev",
  "mar",
  "abr",
  "mai",
  "jun",
  "jul",
  "ago",
  "set",
  "out",
  "nov",
  "dez"
)

fmt_month_pt <- function(x) {
  ifelse(
    is.na(x),
    "—",
    paste0(MONTHS_PT[as.integer(format(x, "%m"))], "/", format(x, "%Y"))
  )
}

# A cleared dateInput returns a length-0 Date (not NULL), so `%||%` alone
# does not catch it
date_or <- function(x, default) {
  if (length(x) == 1 && !is.na(x)) x else default
}

# echarts4r JS formatters ----

js_axis_label_compact <- htmlwidgets::JS(
  "function(v) {",
  "  if (v >= 1e9) return (v/1e9).toFixed(1) + 'bi';",
  "  if (v >= 1e6) return (v/1e6).toFixed(1) + 'M';",
  "  if (v >= 1e3) return Math.round(v/1e3) + 'K';",
  "  return v;",
  "}"
)

js_tooltip_pt_br <- htmlwidgets::JS(
  "function(params) {",
  "  if (!Array.isArray(params)) params = [params];",
  "  var t = '<div style=\"font-weight:600;margin-bottom:4px;color:#0E1130\">' + params[0].axisValueLabel + '</div>';",
  "  params.forEach(function(p) {",
  "    var v = (typeof p.value === 'object' ? p.value[1] : p.value);",
  "    var label = v != null ? v.toLocaleString('pt-BR', {maximumFractionDigits: 1}) : '—';",
  "    t += '<div style=\"display:flex;align-items:center;gap:6px;\">';",
  "    t += '<span style=\"display:inline-block;width:8px;height:8px;border-radius:50%;background:' + p.color + '\"></span>';",
  "    t += '<span style=\"color:#4A4F6B\">' + p.seriesName + '</span>';",
  "    t += '<span style=\"margin-left:auto;font-weight:600;color:#0E1130\">' + label + '</span>';",
  "    t += '</div>';",
  "  });",
  "  return t;",
  "}"
)

# echarts4r shared defaults ----

e_metro_defaults <- function(e, grid_bottom = 70) {
  e |>
    e_x_axis(type = "time") |>
    e_y_axis(
      axisLabel = list(formatter = js_axis_label_compact),
      splitLine = list(lineStyle = list(color = "#EDEEF3"))
    ) |>
    e_tooltip(trigger = "axis", formatter = js_tooltip_pt_br) |>
    e_legend(bottom = 0, itemWidth = 14, itemHeight = 8) |>
    e_grid(left = 60, right = 24, top = 20, bottom = grid_bottom) |>
    e_datazoom(type = "inside") |>
    e_datazoom(type = "slider", bottom = 8, height = 20) |>
    e_toolbox_feature(feature = "saveAsImage", title = "Salvar")
}

# bslib theme ----

metro_theme <- bs_theme(
  version = 5,
  bootswatch = NULL,
  primary = metro_primary,
  secondary = "#4A4F6B",
  success = "#2E7D32",
  danger = "#C62828",
  info = "#1565C0",
  warning = "#B89000",
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  bg = "#F7F8FB",
  fg = "#0E1130"
)

# trendseries (optional): graceful degradation ----

HAS_TRENDSERIES <- requireNamespace("trendseries", quietly = TRUE)
if (!HAS_TRENDSERIES) {
  message(
    "trendseries not installed; STL trend lines will be disabled. ",
    "Install with: pak::pak(\"viniciusoike/trendseries\")"
  )
}

# Constants ----

DEFAULT_START <- as.Date("2019-01-01")

# Pre-build data ----

## Line-level monthly (entrance) ----
ent <- metrosp::passengers_entrance |>
  filter(
    metric_abb == "total",
    line_number %in% as.integer(LINES)
  ) |>
  mutate(line_number = as.character(line_number)) |>
  select(date, line_number, value, year)

## Line-level monthly (transported) ----
trans <- metrosp::passengers_transported |>
  filter(
    metric_abb == "total",
    line_number %in% as.integer(LINES)
  ) |>
  mutate(line_number = as.character(line_number)) |>
  select(date, line_number, value, year)

## Station averages (monthly weekday avg) ----
sta_avg <- metrosp::station_averages |>
  mutate(line_number = as.character(line_number)) |>
  filter(line_number %in% LINES) |>
  select(date, line_number, station_name, value = avg_passenger, year)

## Station daily ----
sta_daily <- metrosp::station_daily |>
  mutate(line_number = as.character(line_number)) |>
  filter(line_number %in% LINES) |>
  select(date, line_number, station_name, value = passengers, year)

## Data window (drives copy, input limits, freshness stamp) ----
DATA_MIN <- min(ent$date, na.rm = TRUE)
DATA_MAX <- max(c(ent$date, sta_daily$date), na.rm = TRUE)

## Spatial data ----
sf_lines <- tryCatch(
  metrosp::lines |>
    filter(status == "current", type == "metro") |>
    mutate(line_number = as.character(line_number)) |>
    filter(line_number %in% LINES),
  error = function(e) {
    message("Failed to load line geometries: ", conditionMessage(e))
    NULL
  }
)

sf_stations <- tryCatch(
  metrosp::stations |>
    filter(status == "current", type == "metro") |>
    mutate(line_number = as.character(line_number)) |>
    filter(line_number %in% LINES),
  error = function(e) {
    message("Failed to load station geometries: ", conditionMessage(e))
    NULL
  }
)

## Station lookup per line ----
stations_by_line <- sta_avg |>
  distinct(line_number, station_name) |>
  arrange(line_number, station_name)

## Station demand for map markers ----
sta_demand_cutoff <- max(sta_avg$date, na.rm = TRUE) - 365
sta_demand_map <- sta_avg |>
  filter(!is.na(value), date > sta_demand_cutoff) |>
  group_by(line_number, station_name) |>
  summarise(avg = mean(value, na.rm = TRUE), .groups = "drop")

sf_stations_map <- if (!is.null(sf_stations)) {
  # Falls back to 1 if the demand window is empty/degenerate so the radius
  # scaling below never divides by -Inf/0
  max_avg <- max(sta_demand_map$avg, na.rm = TRUE)
  if (!is.finite(max_avg) || max_avg <= 0) {
    max_avg <- 1
  }
  sf_stations |>
    left_join(sta_demand_map, by = c("line_number", "station_name")) |>
    mutate(radius = ifelse(is.na(avg), 4, 4 + 12 * sqrt(avg / max_avg))) |>
    # Interchange stations have one row per line at the same point; draw the
    # larger circle first so the smaller one lands on top and stays visible
    arrange(desc(radius))
} else {
  NULL
}

## Available years for daily station data ----
sta_daily_years <- sta_daily |>
  distinct(line_number, station_name, year) |>
  arrange(line_number, station_name, desc(year))

## Ramp-up windows (stations opened within the data window) ----
ramp_windows <- metrosp::station_inauguration |>
  filter(!is.na(inauguration_date), !is.na(ramp_up_end)) |>
  mutate(line_number = as.character(line_number)) |>
  select(line_number, station_name, inauguration_date, ramp_up_end)

## Dataset metadata for download tab ----
# Downloads serve the package datasets as-is, so the schema here matches the
# pkgdown documentation. Computed from the data so it never drifts.
dataset_info <- list(
  passengers_entrance = list(
    label = "passengers_entrance (mensal)",
    desc = paste(
      "Passageiros entrando nas estações, agregado por linha.",
      "Inclui todas as métricas (coluna metric_abb), não apenas o total."
    ),
    cols = names(metrosp::passengers_entrance),
    rows = nrow(metrosp::passengers_entrance),
    range = paste(
      min(metrosp::passengers_entrance$date),
      "a",
      max(metrosp::passengers_entrance$date)
    ),
    source = "METRO SP / Insper Dataverse"
  ),
  passengers_transported = list(
    label = "passengers_transported (mensal)",
    desc = paste(
      "Passageiros transportados por linha por mês.",
      "Inclui todas as métricas (coluna metric_abb)."
    ),
    cols = names(metrosp::passengers_transported),
    rows = nrow(metrosp::passengers_transported),
    range = paste(
      min(metrosp::passengers_transported$date),
      "a",
      max(metrosp::passengers_transported$date)
    ),
    source = "METRO SP"
  ),
  station_averages = list(
    label = "station_averages (mensal)",
    desc = "Média de embarques em dias úteis por estação, mensal.",
    cols = names(metrosp::station_averages),
    rows = nrow(metrosp::station_averages),
    range = paste(
      min(metrosp::station_averages$date),
      "a",
      max(metrosp::station_averages$date)
    ),
    source = "METRO SP / Insper Dataverse"
  ),
  station_daily = list(
    label = "station_daily (diário)",
    desc = "Embarques diários em cada estação do metrô.",
    cols = names(metrosp::station_daily),
    rows = nrow(metrosp::station_daily),
    range = paste(
      min(metrosp::station_daily$date),
      "a",
      max(metrosp::station_daily$date)
    ),
    source = "METRO SP / Insper Dataverse"
  ),
  lines_spatial = list(
    label = "lines (espacial)",
    desc = paste(
      "Traçados de metrô e trem (CPTM), atuais e planejados",
      "(LINESTRING, WGS84)."
    ),
    cols = names(metrosp::lines),
    rows = nrow(metrosp::lines),
    range = NULL,
    source = "GeoSampa"
  ),
  stations_spatial = list(
    label = "stations (espacial)",
    desc = paste(
      "Ponto de cada estação de metrô e trem, atuais e planejadas",
      "(POINT, WGS84)."
    ),
    cols = names(metrosp::stations),
    rows = nrow(metrosp::stations),
    range = NULL,
    source = "GeoSampa"
  )
)

# Download card helper ----

download_card_configs <- list(
  list(
    key = "passengers_entrance",
    dl_ids = c("dl_ent_csv", "dl_ent_xlsx"),
    dl_labels = c("CSV", "Excel"),
    spatial = FALSE
  ),
  list(
    key = "passengers_transported",
    dl_ids = c("dl_trans_csv", "dl_trans_xlsx"),
    dl_labels = c("CSV", "Excel"),
    spatial = FALSE
  ),
  list(
    key = "station_averages",
    dl_ids = c("dl_staavg_csv", "dl_staavg_xlsx"),
    dl_labels = c("CSV", "Excel"),
    spatial = FALSE
  ),
  list(
    key = "station_daily",
    dl_ids = c("dl_stadaily_csv", "dl_stadaily_xlsx"),
    dl_labels = c("CSV", "Excel"),
    spatial = FALSE
  ),
  list(
    key = "lines_spatial",
    dl_ids = c("dl_lines_gpkg", "dl_lines_geojson"),
    dl_labels = c("GPKG", "GeoJSON"),
    spatial = TRUE
  ),
  list(
    key = "stations_spatial",
    dl_ids = c("dl_stations_gpkg", "dl_stations_geojson"),
    dl_labels = c("GPKG", "GeoJSON"),
    spatial = TRUE
  )
)

make_download_card <- function(cfg) {
  info <- dataset_info[[cfg$key]]
  size_label <- if (cfg$spatial) "Feições: " else "Linhas: "
  size_est <- if (!cfg$spatial && info$rows > 0) {
    bytes <- info$rows * length(info$cols) * 12
    if (bytes >= 1e6) {
      sprintf("~%.1f MB", bytes / 1e6)
    } else {
      sprintf("~%.0f KB", max(1, bytes / 1e3))
    }
  }
  card(
    card_header(info$label),
    card_body(
      tags$p(class = "small text-muted", info$desc),
      tags$p(
        class = "small",
        tags$b("Colunas: "),
        paste(info$cols, collapse = ", "),
        tags$br(),
        tags$b(size_label),
        format(info$rows, big.mark = "."),
        if (!is.null(info$range)) {
          tagList(tags$br(), tags$b("Período: "), info$range)
        },
        if (!is.null(size_est)) {
          tagList(tags$br(), tags$b("Tamanho CSV: "), size_est)
        },
        tags$br(),
        tags$b("Fonte: "),
        info$source
      ),
      div(
        class = "d-flex gap-2",
        downloadButton(
          cfg$dl_ids[1],
          cfg$dl_labels[1],
          class = "btn-sm btn-outline-primary"
        ),
        downloadButton(
          cfg$dl_ids[2],
          cfg$dl_labels[2],
          class = "btn-sm btn-outline-primary"
        )
      )
    )
  )
}

