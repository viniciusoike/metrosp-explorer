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
# pt-BR numbers everywhere: "." thousands, "," decimals, mil/mi/bi
# abbreviations. sprintf() and the echarts JS formatters ignore R options
# like OutDec, so every user-facing number funnels through these helpers
# (mirrored in the JS formatters below) rather than a global option.

fmt_dec <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
}

fmt_int <- function(x) {
  # decimal.mark is unused for "d" but silences the prettyNum warning about
  # big.mark and decimal.mark both being "."
  formatC(round(x), format = "d", big.mark = ".", decimal.mark = ",")
}

fmt_n <- function(x) {
  if (!length(x) || all(is.na(x))) {
    return("—")
  }
  x <- x[!is.na(x)][1]
  if (x >= 1e9) {
    paste0(fmt_dec(x / 1e9, 2), " bi")
  } else if (x >= 1e6) {
    paste0(fmt_dec(x / 1e6, 1), " mi")
  } else if (x >= 1e3) {
    paste0(fmt_dec(x / 1e3, 1), " mil")
  } else {
    fmt_int(x)
  }
}

# fmt_n() reduces to the first non-NA value; this maps it element-wise
fmt_n_vec <- function(x) {
  vapply(x, fmt_n, character(1))
}

fmt_pct <- function(x, signed = TRUE) {
  if (is.na(x)) {
    return("—")
  }
  flag <- if (signed) "+" else ""
  paste0(
    formatC(x, format = "f", digits = 1, flag = flag, decimal.mark = ","),
    "%"
  )
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
  if (n < k) {
    return(out)
  }
  for (i in seq.int(k, n)) {
    out[i] <- mean(x[(i - k + 1L):i], na.rm = TRUE)
  }
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

# Mirrors fmt_n(): mil/mi/bi with a decimal comma
js_axis_label_compact <- htmlwidgets::JS(
  "function(v) {",
  "  if (v >= 1e9) return (v/1e9).toFixed(1).replace('.', ',') + ' bi';",
  "  if (v >= 1e6) return (v/1e6).toFixed(1).replace('.', ',') + ' mi';",
  "  if (v >= 1e3) return Math.round(v/1e3) + ' mil';",
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
  fg = "#0E1130",
  "min-contrast-ratio" = 4.1
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

## Map palettes ----
# Line colors are METRO SP brand colors (fixed). In the comparison modes the
# lines dim to neutral gray so the metric ramp owns the hue channel.
map_line_neutral <- "#C3C6D1"
map_na_color <- "#CDD0DA"

# Sequential: single-hue ramp on the metro blue, light -> dark
map_seq_colors <- c("#C6CDF0", "#98A3E2", "#6B79D0", "#3F49B8", "#171796")
map_seq_breaks <- c(0, 10e3, 25e3, 50e3, 100e3, Inf)
map_seq_labels <- c(
  "até 10 mil",
  "10–25 mil",
  "25–50 mil",
  "50–100 mil",
  "mais de 100 mil"
)

# Diverging: ColorBrewer RdBu (CVD-safe), neutral gray midpoint at ~0
map_div_colors <- c(
  "#B2182B",
  "#D6604D",
  "#F4A582",
  "#E6E6E6",
  "#92C5DE",
  "#4393C3",
  "#2166AC"
)
map_div_breaks <- list(
  vs2019 = c(-Inf, -30, -15, -5, 5, 15, 30, Inf),
  yoy = c(-Inf, -15, -5, -1, 1, 5, 15, Inf)
)
map_div_labels <- list(
  vs2019 = c(
    "abaixo de −30%",
    "−30% a −15%",
    "−15% a −5%",
    "−5% a +5%",
    "+5% a +15%",
    "+15% a +30%",
    "acima de +30%"
  ),
  yoy = c(
    "abaixo de −15%",
    "−15% a −5%",
    "−5% a −1%",
    "−1% a +1%",
    "+1% a +5%",
    "+5% a +15%",
    "acima de +15%"
  )
)

map_bin_color <- function(x, breaks, colors) {
  out <- colors[cut(x, breaks, labels = FALSE)]
  out[is.na(out)] <- map_na_color
  out
}

## Station metrics for the map ----
# One marker per station. Interchange stations come as one point per line up
# to ~200 m apart, so collapse to the centroid. All metrics share one
# reference month per station: the last month every serving line reports
# (lines 4/5 stop a year before the rest, so a hub like Luz anchors to the
# older date instead of mixing windows). The reference month is always
# disclosed in the popup, and the percent changes compare only lines present
# in both windows, so a line opening mid-window cannot show up as growth.
mean_or_na <- function(x) {
  if (!length(x) || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sf_stations_map <- NULL
if (!is.null(sf_stations)) {
  # dates are first-of-month, so the same calendar month one year earlier
  # always exists as a plain date
  prev_year_month <- function(d) {
    as.Date(paste0(as.integer(format(d, "%Y")) - 1L, format(d, "-%m-%d")))
  }

  map_line_ref <- sta_avg |>
    filter(!is.na(value)) |>
    group_by(line_number, station_name) |>
    summarise(line_max = max(date), .groups = "drop") |>
    group_by(station_name) |>
    mutate(ref_date = min(line_max)) |>
    ungroup() |>
    mutate(prev_date = prev_year_month(ref_date)) |>
    select(line_number, station_name, ref_date, prev_date)

  sta_map_metrics <- sta_avg |>
    filter(!is.na(value)) |>
    inner_join(map_line_ref, by = c("line_number", "station_name")) |>
    filter(date <= ref_date) |>
    group_by(line_number, station_name, ref_date, prev_date) |>
    summarise(
      avg_12m = mean_or_na(value[date > ref_date - 365]),
      avg_prior = mean_or_na(
        value[date > ref_date - 730 & date <= ref_date - 365]
      ),
      avg_2019 = mean_or_na(value[year == 2019]),
      latest_val = mean_or_na(value[date == ref_date]),
      prev_val = mean_or_na(value[date == prev_date]),
      .groups = "drop"
    )

  map_per_line <- sf_stations |>
    select(line_number, station_name) |>
    left_join(sta_map_metrics, by = c("line_number", "station_name")) |>
    arrange(station_name, as.integer(line_number))

  # avg_12m must be summarised last: it rebinds the name the pct_* blocks read
  sf_stations_map <- map_per_line |>
    group_by(station_name) |>
    summarise(
      first_line = line_number[1],
      n_lines = dplyr::n(),
      # constant within a station by construction; [1] with an NA guard in
      # case a station-line ever fails the demand join
      ref_date = {
        d <- ref_date[!is.na(ref_date)]
        if (length(d)) d[1] else as.Date(NA)
      },
      latest_month = if (all(is.na(latest_val))) {
        NA_real_
      } else {
        sum(latest_val, na.rm = TRUE)
      },
      pct_mom = {
        ok <- !is.na(latest_val) & !is.na(prev_val) & prev_val > 0
        if (any(ok)) {
          (sum(latest_val[ok]) / sum(prev_val[ok]) - 1) * 100
        } else {
          NA_real_
        }
      },
      pct_2019 = {
        ok <- !is.na(avg_12m) & !is.na(avg_2019)
        if (any(ok)) {
          (sum(avg_12m[ok]) / sum(avg_2019[ok]) - 1) * 100
        } else {
          NA_real_
        }
      },
      pct_yoy = {
        ok <- !is.na(avg_12m) & !is.na(avg_prior)
        if (any(ok)) {
          (sum(avg_12m[ok]) / sum(avg_prior[ok]) - 1) * 100
        } else {
          NA_real_
        }
      },
      avg_12m = if (all(is.na(avg_12m))) {
        NA_real_
      } else {
        sum(avg_12m, na.rm = TRUE)
      },
      .groups = "drop"
    )
  # lon/lat centroid warning is irrelevant at station scale
  sf_stations_map <- suppressWarnings(sf::st_centroid(sf_stations_map))

  ## Yearly demand per station (drives the Demanda year slider) ----
  # One numeric vector per year, aligned to sf_stations_map rows so the
  # redraw is a plain lookup. Fixed bins across years keep the animation
  # comparable. 2017 covers only Oct-Dec (known source limitation).
  map_yearly <- sta_avg |>
    filter(!is.na(value)) |>
    group_by(line_number, station_name, year) |>
    summarise(avg = mean(value), .groups = "drop") |>
    group_by(station_name, year) |>
    summarise(avg = sum(avg), .groups = "drop")

  # before 2017 only Line 4 (Insper) reports station data, which would give
  # five nearly-all-gray slider steps; start at the first multi-line year
  map_line_years <- sta_avg |>
    filter(!is.na(value)) |>
    distinct(year, line_number) |>
    count(year)
  MAP_YEAR_MIN <- min(map_line_years$year[map_line_years$n > 1])
  MAP_YEARS <- sort(unique(map_yearly$year[map_yearly$year >= MAP_YEAR_MIN]))
  map_demand_by_year <- lapply(
    setNames(MAP_YEARS, MAP_YEARS),
    function(y) {
      d <- map_yearly[map_yearly$year == y, ]
      d$avg[match(sf_stations_map$station_name, d$station_name)]
    }
  )

  ## Map hover labels and popups (static, built once) ----
  map_metric_row <- function(label, value) {
    paste0(
      '<div class="map-popup-row"><span>',
      label,
      "</span><b>",
      value,
      "</b></div>"
    )
  }

  map_line_link <- function(ln, station, avg) {
    # station names contain no quotes today; escape defensively anyway
    target <- gsub("'", "\\\\'", paste(ln, station, sep = "||"))
    sprintf(
      paste0(
        '<a href="#" class="map-popup-link" onclick="',
        "Shiny.setInputValue('map_go_station', '%s', {priority: 'event'});",
        ' return false;">',
        '<span class="map-dot" style="background:%s"></span>%s',
        '<span class="map-popup-link-val">%s</span>',
        '<span class="map-arrow">&rarr;</span></a>'
      ),
      target,
      line_colors[ln],
      line_labels[ln],
      if (is.na(avg)) "—" else fmt_n(avg)
    )
  }

  map_station_info <- sf::st_drop_geometry(sf_stations_map)
  map_per_line_df <- sf::st_drop_geometry(map_per_line) |>
    semi_join(stations_by_line, by = c("line_number", "station_name"))

  map_popup_html <- vapply(
    seq_len(nrow(map_station_info)),
    function(i) {
      s <- map_station_info[i, ]
      d <- map_per_line_df[map_per_line_df$station_name == s$station_name, ]
      links <- if (nrow(d) > 0) {
        paste0(
          '<div class="map-popup-caption">Ver série mensal</div>',
          paste(
            vapply(
              seq_len(nrow(d)),
              function(j) {
                map_line_link(d$line_number[j], d$station_name[j], d$avg_12m[j])
              },
              character(1)
            ),
            collapse = ""
          )
        )
      } else {
        ""
      }
      paste0(
        '<div class="map-popup">',
        '<div class="map-popup-title">',
        s$station_name,
        "</div>",
        '<div class="map-popup-metrics">',
        # same order and definitions as the KPI cards on the other tabs
        map_metric_row(
          "Último mês",
          if (is.na(s$latest_month)) {
            "—"
          } else {
            paste0(fmt_n(s$latest_month), " pass./dia útil")
          }
        ),
        map_metric_row("Variação mensal (a/a)", fmt_pct(s$pct_mom)),
        map_metric_row("Variação anual", fmt_pct(s$pct_yoy)),
        map_metric_row("vs. 2019", fmt_pct(s$pct_2019)),
        # replaced per redraw with the selected year's value in Demanda
        # mode, stripped in the other modes
        "{{YEAR_ROW}}",
        # always disclose the reference month: lines 4/5 lag the rest of
        # the network, so "último mês" is not the same month everywhere
        if (!is.na(s$ref_date)) {
          paste0(
            '<div class="map-popup-note">dados até ',
            fmt_month_pt(s$ref_date),
            "</div>"
          )
        } else {
          ""
        },
        "</div>",
        '<div class="map-popup-links">',
        links,
        "</div></div>"
      )
    },
    character(1)
  )

  map_lines_lbl <- vapply(
    seq_len(nrow(map_station_info)),
    function(i) {
      s <- map_station_info[i, ]
      if (s$n_lines == 1) {
        unname(line_labels[s$first_line])
      } else {
        lines_i <- map_per_line_df$line_number[
          map_per_line_df$station_name == s$station_name
        ]
        paste0("Linhas ", paste(sort(as.integer(lines_i)), collapse = " e "))
      }
    },
    character(1)
  )

  map_hover_html <- lapply(seq_len(nrow(map_station_info)), function(i) {
    s <- map_station_info[i, ]
    demand <- if (is.na(s$avg_12m)) {
      "Sem dados de demanda"
    } else {
      paste0("Média dias úteis: <b>", fmt_n(s$avg_12m), "</b> pass./dia")
    }
    htmltools::HTML(paste0(
      "<b>",
      s$station_name,
      "</b><br/>",
      map_lines_lbl[i],
      "<br/>",
      demand,
      if (!is.na(s$ref_date)) {
        paste0(
          '<br/><span class="map-hover-note">dados até ',
          fmt_month_pt(s$ref_date),
          "</span>"
        )
      } else {
        ""
      }
    ))
  })

  sf_stations_map$popup_html <- map_popup_html
  sf_stations_map$lines_lbl <- map_lines_lbl
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
    label = "Entrada de passageiros por linha (mensal)",
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
    label = "Passageiros transportados por linha (mensal)",
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
    label = "Média de embarques por estação (mensal)",
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
    label = "Embarques diários por estação",
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
    label = "Traçados das linhas (espacial)",
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
    label = "Estações do metrô (espacial)",
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
  # "Observações", not "Linhas": in this app "linhas" reads as subway lines
  size_label <- if (cfg$spatial) "Feições: " else "Observações: "
  size_est <- if (!cfg$spatial && info$rows > 0) {
    bytes <- info$rows * length(info$cols) * 12
    if (bytes >= 1e6) {
      paste0("~", fmt_dec(bytes / 1e6, 1), " MB")
    } else {
      paste0("~", fmt_int(max(1, bytes / 1e3)), " KB")
    }
  }
  card(
    card_header(info$label),
    card_body(
      tags$p(class = "small text-muted", info$desc),
      tags$p(
        class = "small",
        # titles are human-readable, so keep the package dataset name
        # visible for anyone loading metrosp directly
        tags$b("Dataset: "),
        tags$code(paste0("metrosp::", sub("_spatial$", "", cfg$key))),
        tags$br(),
        tags$b("Colunas: "),
        paste(info$cols, collapse = ", "),
        tags$br(),
        tags$b(size_label),
        fmt_int(info$rows),
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
