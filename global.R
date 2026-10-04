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
# if (!isTRUE(l10n_info()[["UTF-8"]])) {
#   for (loc in c("en_US.UTF-8", "C.UTF-8", "en_US.utf8", "C.utf8")) {
#     if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
#   }
# }

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

# EKIO brand chrome (ekioplot "ekio_brand" and "blue" palettes). Line colors
# above are METRO SP brand colors and stay as they are.
metro_primary <- "#225A7E"
metro_ink <- "#0D1B2A"
metro_ink_soft <- "#3C3935"
metro_page <- "#FBFBF6" # ekioplot basic.offwhite
metro_grid <- "#E6E0D4"

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

# Sign of a change metric, as a CSS modifier. Absolute levels ("último mês")
# have no sign to convey and stay neutral.
kpi_tone <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    "neutral"
  } else if (x > 0) {
    "pos"
  } else if (x < 0) {
    "neg"
  } else {
    "neutral"
  }
}

kpi_card <- function(label, value, sub = NULL, tone = "neutral") {
  div(
    class = paste0("kpi kpi--", tone),
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

# Tooltip header is "dez/2020" for monthly charts and "05/12/2020" for daily
# ones. Values mirror fmt_n(); echarts4r ships each point as a [date, value]
# pair, so the value arrives as a string and needs Number() first.
js_tooltip_pt_br <- function(date_format = c("month", "day")) {
  date_format <- match.arg(date_format)
  months_js <- paste0("['", paste(MONTHS_PT, collapse = "','"), "']")
  header_js <- switch(
    date_format,
    month = paste0(months_js, "[d.getMonth()] + '/' + d.getFullYear()"),
    day = paste0(
      "String(d.getDate()).padStart(2, '0') + '/' + ",
      "String(d.getMonth() + 1).padStart(2, '0') + '/' + d.getFullYear()"
    )
  )

  htmlwidgets::JS(
    "function(params) {",
    "  if (!Array.isArray(params)) params = [params];",
    "  var dec = function(x, n) {",
    "    return x.toLocaleString('pt-BR', {minimumFractionDigits: n, maximumFractionDigits: n});",
    "  };",
    "  var fmtN = function(v) {",
    "    if (v == null || v === '' || isNaN(Number(v))) return '—';",
    "    v = Number(v);",
    "    if (v >= 1e9) return dec(v / 1e9, 2) + ' bi';",
    "    if (v >= 1e6) return dec(v / 1e6, 1) + ' mi';",
    "    if (v >= 1e3) return dec(v / 1e3, 1) + ' mil';",
    "    return dec(Math.round(v), 0);",
    "  };",
    "  var d = new Date(params[0].axisValue);",
    paste0(
      "  var header = isNaN(d) ? params[0].axisValueLabel : ",
      header_js,
      ";"
    ),
    paste0(
      "  var t = '<div style=\"font-weight:600;margin-bottom:4px;color:",
      metro_ink,
      "\">' + header + '</div>';"
    ),
    "  params.forEach(function(p) {",
    "    var v = Array.isArray(p.value) ? p.value[1] : p.value;",
    "    t += '<div style=\"display:flex;align-items:center;gap:6px;\">';",
    "    t += '<span style=\"display:inline-block;width:8px;height:8px;border-radius:50%;background:' + p.color + '\"></span>';",
    paste0(
      "    t += '<span style=\"color:",
      metro_ink_soft,
      "\">' + p.seriesName + '</span>';"
    ),
    paste0(
      "    t += '<span style=\"margin-left:auto;font-weight:600;color:",
      metro_ink,
      "\">' + fmtN(v) + '</span>';"
    ),
    "    t += '</div>';",
    "  });",
    "  return t;",
    "}"
  )
}

# echarts4r shared defaults ----

# The legend sits at the top: the datazoom slider owns the bottom strip
# (bottom 8 + height 20), and a bottom legend lands on top of it. Charts with
# a single series pass legend = FALSE, since the card header already names it.
e_metro_defaults <- function(e, legend = TRUE, date_format = "month") {
  e |>
    e_x_axis(type = "time") |>
    e_y_axis(
      axisLabel = list(formatter = js_axis_label_compact),
      splitLine = list(lineStyle = list(color = metro_grid))
    ) |>
    e_tooltip(trigger = "axis", formatter = js_tooltip_pt_br(date_format)) |>
    e_legend(show = legend, top = 0, itemWidth = 14, itemHeight = 8) |>
    e_grid(
      left = 60,
      right = 24,
      top = if (legend) 34 else 16,
      bottom = 56
    ) |>
    e_datazoom(type = "inside") |>
    e_datazoom(
      type = "slider",
      bottom = 8,
      height = 20,
      borderColor = metro_grid,
      fillerColor = "rgba(34, 90, 126, 0.12)",
      dataBackground = list(
        lineStyle = list(color = "#B4B0AB"),
        areaStyle = list(color = metro_grid)
      ),
      handleStyle = list(color = "#FBFBF6", borderColor = metro_primary),
      moveHandleStyle = list(color = metro_grid),
      selectedDataBackground = list(
        lineStyle = list(color = metro_primary),
        areaStyle = list(color = "rgba(34, 90, 126, 0.15)")
      )
    ) |>
    e_toolbox_feature(feature = "saveAsImage", title = "Salvar") |>
    # echarts draws on canvas and does not inherit the page font
    e_text_style(fontFamily = paste0("'", APP_FONT, "', sans-serif"))
}

# bslib theme ----

APP_FONT <- "Host Grotesk"

metro_theme <- bs_theme(
  version = 5,
  bootswatch = NULL,
  primary = metro_primary,
  secondary = "#59544F",
  success = "#006261",
  danger = "#B44D47",
  info = "#3E76AC",
  warning = "#B88715",
  # without wght Google Fonts serves only the regular face, and every 500,
  # 600 and 700 in styles.css becomes a synthetic bold
  base_font = font_google(
    APP_FONT,
    local = FALSE,
    wght = c(400, 500, 600, 700)
  ),
  heading_font = font_google(
    APP_FONT,
    local = FALSE,
    wght = c(400, 500, 600, 700)
  ),
  bg = metro_page,
  fg = metro_ink,
  "min-contrast-ratio" = 4.1
)

# Basemap tiles ----
# CARTO tiles now need an API key. addProviderTiles() drops the `key` option
# because leaflet.providers 2.0.0 has no {key} slot in the CartoDB template
# (rstudio/leaflet#965), so build the tile layer by hand. The key is
# restricted by referrer in the CARTO dashboard; without it (or from a domain
# outside the allowlist) tiles come back watermarked or 403. On Connect Cloud
# the key must be set as an app environment variable.

CARTO_KEY <- Sys.getenv("CARTO_BASEMAP_SHINY")

if (!nzchar(CARTO_KEY)) {
  cli::cli_warn(c(
    "{.envvar CARTO_BASEMAP_SHINY} is not set.",
    "i" = "CARTO basemap tiles will show an API key watermark."
  ))
}

add_carto_tiles <- function(map, variant = "light_all") {
  url <- paste0(
    "https://basemaps.cartocdn.com/rastertiles/",
    variant,
    "/{z}/{x}/{y}{r}.png"
  )
  if (nzchar(CARTO_KEY)) {
    url <- paste0(url, "?key=", CARTO_KEY)
  }

  map <- addTiles(
    map,
    urlTemplate = url,
    attribution = paste(
      '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
      'contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
    ),
    options = tileOptions(maxZoom = 20)
  )
  return(map)
}

# trendseries (optional): graceful degradation ----

HAS_TRENDSERIES <- requireNamespace("trendseries", quietly = TRUE)
if (!HAS_TRENDSERIES) {
  message(
    "trendseries not installed; STL trend lines will be disabled. ",
    "Install with: pak::pak(\"viniciusoike/trendseries\")"
  )
}

# Fit one monthly series without filling official gaps or missing values.
augment_monthly_stl <- function(dat) {
  dat$trend_stl <- NA_real_
  rows <- which(is.finite(dat$value))
  rows <- rows[order(dat$date[rows])]
  if (length(rows) == 0L) {
    return(dat)
  }
  month <- as.integer(format(dat$date[rows], "%Y")) *
    12L +
    as.integer(format(dat$date[rows], "%m"))
  segments <- split(rows, cumsum(c(TRUE, diff(month) != 1L)))
  for (segment in segments) {
    # Monthly STL requires more than two complete annual cycles.
    if (length(segment) <= 24L) {
      next
    }
    fitted <- trendseries::augment_trends(
      dat[segment, c("date", "value")],
      date_col = "date",
      value_col = "value",
      frequency = 12,
      methods = "stl",
      params = list(robust = TRUE, s.window = 13),
      .quiet = TRUE
    )
    dat$trend_stl[segment] <- fitted$trend_stl
  }
  return(dat)
}

# Line forecasts ----
# Equal-weight ensemble of three models on log demand, fitted only on
# post-pandemic months so the 2020-21 collapse and recovery do not drive the
# trend: STL + ETS, ETS, and ARIMA with calendar regressors. Rolling-origin
# backtests favored the ensemble over each member. Back-transformed point
# forecasts are medians, and the interval averages the members' bounds.

FORECAST_START <- as.Date("2022-01-01")
FORECAST_H <- 12L
FORECAST_MIN_OBS <- 36L
FORECAST_LEVEL <- 90

# Business days and non-holiday Saturdays per month (São Paulo calendar)
calendar_monthly <- metrosp::calendar_spo |>
  mutate(date = as.Date(format(date, "%Y-%m-01"))) |>
  group_by(date) |>
  summarise(
    bdays = sum(is_business_day),
    sats = sum(weekday == 7 & !is_holiday),
    .groups = "drop"
  )

calendar_xreg <- function(dates) {
  rows <- match(dates, calendar_monthly$date)
  return(as.matrix(calendar_monthly[rows, c("bdays", "sats")]))
}

# One line's monthly series in, an h-month forecast out. Returns NULL when
# the fit window is too short or has gaps, or the horizon outruns the
# calendar.
forecast_line <- function(df, h = FORECAST_H, start = FORECAST_START) {
  fit_df <- df |>
    filter(date >= start, !is.na(value), value > 0) |>
    arrange(date)
  if (nrow(fit_df) < FORECAST_MIN_OBS) {
    return(NULL)
  }
  months <- seq(min(fit_df$date), max(fit_df$date), by = "month")
  if (length(months) != nrow(fit_df)) {
    return(NULL)
  }
  dates <- seq(max(fit_df$date), by = "month", length.out = h + 1)[-1]
  if (!all(dates %in% calendar_monthly$date)) {
    return(NULL)
  }

  first <- as.POSIXlt(fit_df$date[1])
  y <- stats::ts(
    log(fit_df$value),
    start = c(first$year + 1900, first$mon + 1),
    frequency = 12
  )

  members <- list(
    forecast::stlf(y, h = h, method = "ets", robust = TRUE, level = FORECAST_LEVEL),
    forecast::forecast(forecast::ets(y), h = h, level = FORECAST_LEVEL),
    forecast::forecast(
      forecast::auto.arima(y, xreg = calendar_xreg(fit_df$date)),
      xreg = calendar_xreg(dates),
      level = FORECAST_LEVEL
    )
  )
  avg <- function(part) {
    return(rowMeans(sapply(members, function(m) as.numeric(m[[part]]))))
  }

  return(tibble::tibble(
    date = dates,
    fc_mean = exp(avg("mean")),
    fc_lower = exp(avg("lower")),
    fc_upper = exp(avg("upper"))
  ))
}

# Percent change of the next 12 forecast months over the last 12 observed
forecast_growth <- function(observed, fc) {
  last_12 <- observed |>
    filter(!is.na(value)) |>
    slice_max(date, n = 12)
  next_12 <- fc |> slice_min(date, n = 12)
  return((sum(next_12$fc_mean) / sum(last_12$value) - 1) * 100)
}

# Pre-build data ----

DEMAND_DATASETS <- c(
  "line_entries_monthly",
  "line_transported_monthly",
  "station_transported_monthly",
  "station_entries_daily"
)

read_demand_manifest <- function() {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  utils::download.file(
    "https://github.com/viniciusoike/metrosp/releases/download/data-latest/manifest.json",
    path,
    quiet = TRUE,
    mode = "wb"
  )
  return(jsonlite::read_json(path, simplifyVector = FALSE))
}

load_demand_data <- function(
  read_fun = metrosp::read_metro_demand,
  manifest_fun = read_demand_manifest
) {
  rolling <- tryCatch(
    {
      manifest_before <- manifest_fun()
      tables <- lapply(
        DEMAND_DATASETS,
        read_fun,
        source = "remote",
        quiet = TRUE
      )
      manifest_after <- manifest_fun()
      if (!identical(manifest_before, manifest_after)) {
        cli::cli_abort(
          "The rolling release changed while loading demand tables."
        )
      }
      tables
    },
    error = function(e) {
      cli::cli_warn(c(
        "Could not load a coherent rolling demand release.",
        "x" = conditionMessage(e),
        "i" = "Using all four bundled snapshot tables."
      ))
      NULL
    }
  )

  if (is.null(rolling)) {
    rolling <- lapply(
      DEMAND_DATASETS,
      read_fun,
      source = "bundled",
      quiet = TRUE
    )
    attr(rolling, "source") <- "bundled"
  } else {
    attr(rolling, "source") <- "rolling"
  }
  names(rolling) <- DEMAND_DATASETS
  return(rolling)
}

demand_data <- load_demand_data()
DATA_SOURCE <- attr(demand_data, "source")
DATA_SOURCE_LABEL <- if (identical(DATA_SOURCE, "rolling")) {
  "publicação contínua"
} else {
  "snapshot incluído no pacote"
}

line_entries_monthly <- demand_data$line_entries_monthly
line_transported_monthly <- demand_data$line_transported_monthly
station_transported_monthly <- demand_data$station_transported_monthly
station_entries_daily <- demand_data$station_entries_daily

## Line-level monthly (entries) ----
ent <- line_entries_monthly |>
  filter(
    metric == "total",
    line_number %in% as.integer(LINES)
  ) |>
  mutate(line_number = as.character(line_number)) |>
  select(date, line_number, value, year)

## Line-level monthly (transported) ----
trans <- line_transported_monthly |>
  filter(
    metric == "total",
    line_number %in% as.integer(LINES)
  ) |>
  mutate(line_number = as.character(line_number)) |>
  select(date, line_number, value, year)

## Station transported (monthly weekday average) ----
sta_avg <- station_transported_monthly |>
  mutate(line_number = as.character(line_number)) |>
  filter(line_number %in% LINES) |>
  select(date, line_number, station_id, station_name, value, year)

## Station entries (daily) ----
sta_daily <- station_entries_daily |>
  mutate(line_number = as.character(line_number)) |>
  filter(line_number %in% LINES) |>
  select(date, line_number, station_id, station_name, value, year)

## Data window (drives copy, input limits, freshness stamp) ----
DATA_MIN <- min(
  ent$date,
  trans$date,
  sta_avg$date,
  sta_daily$date,
  na.rm = TRUE
)
DATA_MAX <- max(
  ent$date,
  trans$date,
  sta_avg$date,
  sta_daily$date,
  na.rm = TRUE
)

## Period presets ----
# The series are monthly, so a day-granular dateInput offered precision the
# data does not have. These anchor on the last published month rather than
# Sys.Date() so the window does not drift past the data. Unknown values (old
# bookmarks) fall back to the full series.
PERIOD_CHOICES <- c(
  "Desde o início" = "inicio",
  "12 meses" = "12m",
  "24 meses" = "24m"
)

period_start <- function(period) {
  n_months <- switch(period %||% "inicio", "12m" = 12L, "24m" = 24L, NULL)
  if (is.null(n_months)) {
    return(DATA_MIN)
  }
  # first-of-month anchor: stepping back by month from a 31st overflows
  last_month <- as.Date(format(DATA_MAX, "%Y-%m-01"))
  start <- seq(last_month, by = "-1 month", length.out = n_months)[n_months]
  return(start)
}

## Spatial data ----
sf_lines <- tryCatch(
  metrosp::rail_lines |>
    filter(status == "current", type == "metro") |>
    mutate(line_number = as.character(line_number)) |>
    filter(line_number %in% LINES),
  error = function(e) {
    message("Failed to load line geometries: ", conditionMessage(e))
    NULL
  }
)

sf_stations <- tryCatch(
  metrosp::rail_stations |>
    filter(status == "current", type == "metro") |>
    mutate(line_number = as.character(line_number)) |>
    filter(line_number %in% LINES),
  error = function(e) {
    message("Failed to load station geometries: ", conditionMessage(e))
    NULL
  }
)

## Station lookup per line ----
stations_by_line <- bind_rows(
  sta_avg |> select(line_number, station_id, station_name),
  sta_daily |> select(line_number, station_id, station_name)
) |>
  distinct(line_number, station_id, station_name) |>
  arrange(line_number, station_name)

## Map palettes ----
# Line colors are METRO SP brand colors (fixed). In the comparison modes the
# lines dim to neutral gray so the metric ramp owns the hue channel.
map_line_neutral <- "#C9C3B8"
map_na_color <- "#B9B3A9"

# Sequential: ekioplot "blue" ramp, light -> dark
map_seq_colors <- c("#B1D8F2", "#84B8DD", "#5597CC", "#3E76AC", "#1E3A5F")
map_seq_breaks <- c(0, 10e3, 25e3, 50e3, 100e3, Inf)
map_seq_labels <- c(
  "até 10 mil",
  "10–25 mil",
  "25–50 mil",
  "50–100 mil",
  "mais de 100 mil"
)

# Diverging: ekioplot "blue_red" (red-blue stays CVD-safe), reversed so
# losses are red, trimmed to seven steps around a neutral midpoint at ~0
map_div_colors <- c(
  "#8C3431",
  "#B44D47",
  "#E9998E",
  "#F5F3EF",
  "#84B8DD",
  "#3E76AC",
  "#305687"
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
    group_by(line_number, station_id) |>
    summarise(line_max = max(date), .groups = "drop") |>
    group_by(station_id) |>
    mutate(ref_date = min(line_max)) |>
    ungroup() |>
    mutate(prev_date = prev_year_month(ref_date)) |>
    select(line_number, station_id, ref_date, prev_date)

  sta_map_metrics <- sta_avg |>
    filter(!is.na(value)) |>
    inner_join(map_line_ref, by = c("line_number", "station_id")) |>
    filter(date <= ref_date) |>
    group_by(line_number, station_id, ref_date, prev_date) |>
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
    select(line_number, station_id, station_name) |>
    left_join(sta_map_metrics, by = c("line_number", "station_id")) |>
    arrange(station_id, as.integer(line_number))

  # avg_12m must be summarised last: it rebinds the name the pct_* blocks read
  sf_stations_map <- map_per_line |>
    group_by(station_id) |>
    summarise(
      station_name = paste(unique(station_name), collapse = " / "),
      first_line = line_number[1],
      n_lines = dplyr::n_distinct(line_number),
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
    group_by(line_number, station_id, year) |>
    summarise(avg = mean(value), .groups = "drop") |>
    group_by(station_id, year) |>
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
      d$avg[match(sf_stations_map$station_id, d$station_id)]
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

  map_line_link <- function(ln, station_id, station_name, avg) {
    target <- gsub("'", "\\\\'", paste(ln, station_id, sep = "||"))
    sprintf(
      paste0(
        '<a href="#" class="map-popup-link" onclick="',
        "Shiny.setInputValue('map_go_station', '%s', {priority: 'event'});",
        ' return false;">',
        '<span class="map-dot" style="background:%s"></span>%s · %s',
        '<span class="map-popup-link-val">%s</span>',
        '<span class="map-arrow">&rarr;</span></a>'
      ),
      target,
      line_colors[ln],
      station_name,
      line_labels[ln],
      if (is.na(avg)) "—" else fmt_n(avg)
    )
  }

  map_station_info <- sf::st_drop_geometry(sf_stations_map)
  map_per_line_df <- sf::st_drop_geometry(map_per_line) |>
    semi_join(stations_by_line, by = c("line_number", "station_id"))

  map_popup_html <- vapply(
    seq_len(nrow(map_station_info)),
    function(i) {
      s <- map_station_info[i, ]
      d <- map_per_line_df[map_per_line_df$station_id == s$station_id, ]
      links <- if (nrow(d) > 0) {
        paste0(
          '<div class="map-popup-caption">Abrir dados da estação</div>',
          paste(
            vapply(
              seq_len(nrow(d)),
              function(j) {
                map_line_link(
                  d$line_number[j],
                  d$station_id[j],
                  d$station_name[j],
                  d$avg_12m[j]
                )
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
          "Transportados — último mês",
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
          map_per_line_df$station_id == s$station_id
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
      paste0(
        "Transportados/dia útil: <b>",
        fmt_n(s$avg_12m),
        "</b> pass./dia"
      )
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
  distinct(line_number, station_id, year) |>
  arrange(line_number, station_id, desc(year))

## Selection info boxes ----
# Definitions and sources follow the metrosp documentation
# (?line_entries_monthly, ?line_transported_monthly,
# ?station_transported_monthly, ?station_entries_daily); coverage dates are
# computed from the loaded tables.

metric_info <- list(
  entrance = list(
    label = "Embarques (entrada)",
    definition = paste(
      "Passageiros que entraram pelas catracas das estações da linha no mês.",
      "Quem faz baldeação vindo de outra linha não conta como nova entrada."
    )
  ),
  transported = list(
    label = "Passageiros transportados",
    definition = paste(
      "Passageiros que viajaram na linha no mês: quem entrou pelas catracas",
      "mais quem chegou por baldeação de outra linha. A soma de linhas conta",
      "uma viagem com baldeação mais de uma vez; não é um total único da rede."
    )
  ),
  station = list(
    label = "Demanda da estação",
    definition = paste(
      "A série mensal mostra a média de passageiros transportados por dia útil.",
      "A série diária mostra as entradas de cada dia, com a média móvel de",
      "7 dias em destaque. As duas séries medem conceitos diferentes."
    )
  )
)

trend_note <- if (HAS_TRENDSERIES) {
  "Tendência estimada por decomposição STL robusta (s.window = 13)."
} else {
  "Instale o pacote trendseries para habilitar tendência STL."
}

forecast_note <- paste0(
  "Projeção: média de três modelos (STL + ETS, ETS e ARIMA com dias úteis), ",
  "ajustados desde ",
  fmt_month_pt(FORECAST_START),
  ". A faixa indica o intervalo de ",
  FORECAST_LEVEL,
  "%."
)

line_coverage <- bind_rows(
  entrance = ent,
  transported = trans,
  .id = "metric"
) |>
  filter(!is.na(value)) |>
  group_by(metric, line_number) |>
  summarise(first = min(date), last = max(date), .groups = "drop")

line_source <- function(line, dataset, first) {
  if (!line %in% c("4", "5")) {
    return("METRO SP")
  }

  if (line == "4") {
    return("Insper Dataverse")
  }

  if (dataset == "station_entries_daily") {
    return("Insper Dataverse")
  }
  if (dataset %in% c("transported", "station_transported_monthly")) {
    return("METRO SP")
  }
  if (!is.null(first) && first >= as.Date("2018-08-01")) {
    return("Insper Dataverse")
  }
  return("METRO SP até jul/2018, Insper Dataverse depois")
}

fmt_span_pt <- function(first, last) {
  return(paste(fmt_month_pt(first), "a", fmt_month_pt(last)))
}

info_row <- function(line, detail) {
  return(div(
    class = "info-row",
    tags$span(
      class = "map-dot",
      style = paste0("background:", line_colors[[line]])
    ),
    div(
      div(class = "info-row-title", line_labels[[line]]),
      lapply(detail, function(x) div(class = "info-row-detail", x))
    )
  ))
}

info_box <- function(title, definition, rows, notes = NULL) {
  return(div(
    class = "info-box",
    div(class = "info-box-eyebrow", bs_icon("info-circle"), "Sobre a seleção"),
    div(class = "info-box-title", title),
    tags$p(class = "info-box-text", definition),
    div(class = "info-box-rows", rows),
    if (length(notes)) {
      tags$ul(class = "info-box-notes", lapply(notes, tags$li))
    }
  ))
}

lines_info_box <- function(
  lines,
  metric,
  start,
  show_trend,
  show_forecast = FALSE
) {
  info <- metric_info[[metric]]
  coverage <- line_coverage[line_coverage$metric == metric, ]

  rows <- lapply(lines, function(ln) {
    cov <- coverage[coverage$line_number == ln, ]
    detail <- if (nrow(cov) == 0) {
      "A fonte não publica esta variável para a linha"
    } else {
      c(
        fmt_span_pt(cov$first, cov$last),
        paste("Origem:", line_source(ln, metric, cov$first))
      )
    }
    return(info_row(ln, detail))
  })

  notes <- c(
    if (
      metric == "entrance" &&
        any(lines != "4") &&
        start <= as.Date("2017-07-01")
    ) {
      "Jul/2017 sem dados de embarque: o METRO não publicou a tabela do mês."
    },
    if (
      metric == "transported" &&
        "5" %in% lines &&
        start <= as.Date("2018-08-01")
    ) {
      "Linha 5: série termina em ago/2018, quando passou à ViaMobilidade."
    },
    if (show_trend) trend_note,
    if (show_forecast) forecast_note
  )

  return(info_box(info$label, info$definition, rows, notes))
}

station_info_box <- function(line, station_id, start, show_trend) {
  monthly <- sta_avg[
    sta_avg$line_number == line &
      sta_avg$station_id == station_id &
      !is.na(sta_avg$value),
  ]
  years <- sta_daily_years$year[
    sta_daily_years$line_number == line &
      sta_daily_years$station_id == station_id
  ]

  sources <- unique(c(
    if (nrow(monthly) > 0) {
      line_source(line, "station_transported_monthly", min(monthly$date))
    },
    if (length(years) > 0) line_source(line, "station_entries_daily", NULL)
  ))
  detail <- c(
    if (nrow(monthly) > 0) {
      paste("Mensal:", fmt_span_pt(min(monthly$date), max(monthly$date)))
    },
    if (length(years) > 0) {
      paste("Diária:", paste(range(years), collapse = " a "))
    },
    if (length(sources) == 1) {
      paste("Origem:", sources)
    } else if (length(sources) == 2) {
      c(
        paste("Origem mensal:", sources[1]),
        paste("Origem diária:", sources[2])
      )
    }
  )
  if (is.null(detail)) {
    detail <- "Sem dados para esta estação"
  }

  notes <- c(
    if (line == "5" && nrow(monthly) > 0) {
      "A série mensal transportada termina em jul/2018; a série diária de entradas continua depois."
    },
    if (
      line == "1" &&
        start <= as.Date("2016-06-01") &&
        any(format(monthly$date, "%Y") == "2016")
    ) {
      paste(
        "Fev a jun/2016: os valores da Linha 1 vêm subestimados e mal",
        "distribuídos entre estações na publicação original do METRO."
      )
    },
    if (show_trend) trend_note
  )

  return(info_box(
    metric_info$station$label,
    metric_info$station$definition,
    info_row(line, detail),
    notes
  ))
}

## Dataset metadata for download tab ----
# Downloads serve the package datasets as-is, so the schema here matches the
# pkgdown documentation. Computed from the data so it never drifts.
dataset_info <- list(
  line_entries_monthly = list(
    label = "Entrada de passageiros por linha (mensal)",
    desc = paste(
      "Passageiros entrando nas estações, agregados por linha.",
      "Inclui todas as métricas, não apenas o total."
    ),
    cols = names(line_entries_monthly),
    rows = nrow(line_entries_monthly),
    range = paste(
      min(line_entries_monthly$date),
      "a",
      max(line_entries_monthly$date)
    ),
    source = "METRO SP / Insper Dataverse"
  ),
  line_transported_monthly = list(
    label = "Passageiros transportados por linha (mensal)",
    desc = paste(
      "Passageiros transportados em cada linha, em indivíduos por mês.",
      "Inclui todas as métricas."
    ),
    cols = names(line_transported_monthly),
    rows = nrow(line_transported_monthly),
    range = paste(
      min(line_transported_monthly$date),
      "a",
      max(line_transported_monthly$date)
    ),
    source = "METRO SP / Insper Dataverse"
  ),
  station_transported_monthly = list(
    label = "Transportados por estação (mensal)",
    desc = "Média de passageiros transportados por dia útil em cada estação.",
    cols = names(station_transported_monthly),
    rows = nrow(station_transported_monthly),
    range = paste(
      min(station_transported_monthly$date),
      "a",
      max(station_transported_monthly$date)
    ),
    source = "METRO SP / Insper Dataverse"
  ),
  station_entries_daily = list(
    label = "Entradas diárias por estação",
    desc = "Entradas diárias em cada estação do metrô.",
    cols = names(station_entries_daily),
    rows = nrow(station_entries_daily),
    range = paste(
      min(station_entries_daily$date),
      "a",
      max(station_entries_daily$date)
    ),
    source = "METRO SP / Insper Dataverse"
  ),
  rail_lines = list(
    label = "Traçados das linhas (espacial)",
    desc = paste(
      "Traçados de metrô e trem (CPTM), atuais e planejados",
      "(LINESTRING, WGS84)."
    ),
    cols = names(metrosp::rail_lines),
    rows = nrow(metrosp::rail_lines),
    range = NULL,
    source = "GeoSampa"
  ),
  rail_stations = list(
    label = "Estações do metrô (espacial)",
    desc = paste(
      "Ponto de cada estação de metrô e trem, atuais e planejadas",
      "(POINT, WGS84)."
    ),
    cols = names(metrosp::rail_stations),
    rows = nrow(metrosp::rail_stations),
    range = NULL,
    source = "GeoSampa"
  )
)

# Download card helper ----

download_card_configs <- list(
  list(
    key = "line_entries_monthly",
    dl_ids = c("dl_ent_csv", "dl_ent_xlsx"),
    dl_labels = c("CSV", "Excel"),
    spatial = FALSE
  ),
  list(
    key = "line_transported_monthly",
    dl_ids = c("dl_trans_csv", "dl_trans_xlsx"),
    dl_labels = c("CSV", "Excel"),
    spatial = FALSE
  ),
  list(
    key = "station_transported_monthly",
    dl_ids = c("dl_staavg_csv", "dl_staavg_xlsx"),
    dl_labels = c("CSV", "Excel"),
    spatial = FALSE
  ),
  list(
    key = "station_entries_daily",
    dl_ids = c("dl_stadaily_csv", "dl_stadaily_xlsx"),
    dl_labels = c("CSV", "Excel"),
    spatial = FALSE
  ),
  list(
    key = "rail_lines",
    dl_ids = c("dl_lines_gpkg", "dl_lines_geojson"),
    dl_labels = c("GPKG", "GeoJSON"),
    spatial = TRUE
  ),
  list(
    key = "rail_stations",
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
    card_header(info$label, container = tags$h3),
    card_body(
      tags$p(class = "small text-muted", info$desc),
      tags$p(
        class = "small",
        # titles are human-readable, so keep the package dataset name
        # visible for anyone loading metrosp directly
        tags$b("Dataset: "),
        tags$code(paste0("metrosp::", cfg$key)),
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
