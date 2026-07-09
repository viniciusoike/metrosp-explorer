# Server for the Metro SP explorer ----
# Data objects and helpers are defined in global.R.

function(input, output, session) {
  ## Lines tab ----

  lines_data <- reactive({
    lns <- input$lines_line
    req(length(lns) > 0)

    start <- date_or(input$lines_start, DEFAULT_START)
    base <- if (input$lines_metric == "entrance") ent else trans
    df <- base |> filter(line_number %in% lns, date >= start)

    show_trend <- isTRUE(input$lines_trend) &&
      length(lns) == 1 &&
      HAS_TRENDSERIES
    if (!show_trend || nrow(df) == 0) {
      return(df)
    }

    tryCatch(
      trendseries::augment_trends(
        df,
        date_col = "date",
        value_col = "value",
        group_cols = "line_number",
        methods = "stl",
        params = list(robust = TRUE, s.window = 13),
        .quiet = TRUE
      ),
      error = function(e) {
        message("STL trend failed for line ", lns, ": ", conditionMessage(e))
        df |> mutate(trend_stl = NA_real_)
      }
    )
  }) |>
    bindCache(
      input$lines_line,
      input$lines_metric,
      input$lines_start,
      input$lines_trend
    )

  output$lines_kpis <- renderUI({
    lns <- input$lines_line
    req(length(lns) > 0)

    # full series (not start-date filtered) so KPIs describe the selection,
    # not the visible window; keep only months every selected line reported
    base <- if (input$lines_metric == "entrance") ent else trans
    monthly <- base |>
      filter(line_number %in% lns, !is.na(value)) |>
      group_by(date) |>
      summarise(value = sum(value), n_lines = n(), .groups = "drop") |>
      filter(n_lines == length(lns))
    req(nrow(monthly) > 0)

    latest <- monthly |> slice_max(date, n = 1)
    recent <- monthly |> filter(date > latest$date - 365)
    prior <- monthly |>
      filter(date > latest$date - 730, date <= latest$date - 365)
    base_2019 <- monthly |> filter(format(date, "%Y") == "2019")

    yoy_label <- if (nrow(recent) >= 6 && nrow(prior) >= 6) {
      fmt_pct((mean(recent$value) / mean(prior$value) - 1) * 100)
    } else {
      "—"
    }
    vs2019_label <- if (nrow(recent) >= 6 && nrow(base_2019) >= 6) {
      fmt_pct((mean(recent$value) / mean(base_2019$value) - 1) * 100)
    } else {
      "—"
    }
    # same calendar month one year earlier (monthly dates, so exact match)
    prev_month_date <- seq(latest$date, by = "-1 year", length.out = 2)[2]
    prev_month <- monthly |> filter(date == prev_month_date)
    mom_yoy_label <- if (nrow(prev_month) == 1 && prev_month$value > 0) {
      fmt_pct((latest$value / prev_month$value - 1) * 100)
    } else {
      "—"
    }

    div(
      class = "kpi-grid kpi-grid-4",
      kpi_card("Último mês", fmt_n(latest$value), fmt_month_pt(latest$date)),
      kpi_card(
        "Variação mensal (a/a)",
        mom_yoy_label,
        paste0(
          fmt_month_pt(latest$date),
          " vs. ",
          fmt_month_pt(prev_month_date)
        )
      ),
      kpi_card("Variação anual", yoy_label, "últimos 12m vs. anteriores"),
      kpi_card("vs. 2019", vs2019_label, "últimos 12m vs. média de 2019")
    )
  })

  output$lines_title <- renderText({
    lns <- input$lines_line
    metric_lbl <- if (input$lines_metric == "entrance") {
      "Embarques"
    } else {
      "Transportados"
    }
    if (length(lns) == 1) {
      paste0(metric_lbl, " — ", line_labels[lns])
    } else {
      paste0(metric_lbl, " — ", length(lns), " linhas")
    }
  })

  output$lines_chart <- renderEcharts4r({
    df <- lines_data()
    validate(need(
      nrow(df) > 0,
      paste(
        "Sem dados para a seleção atual. Verifique o período escolhido;",
        "as linhas 4 e 5 não possuem dados de passageiros transportados."
      )
    ))
    lns <- input$lines_line
    show_trend <- isTRUE(input$lines_trend) &&
      length(lns) == 1 &&
      HAS_TRENDSERIES

    if (length(lns) == 1) {
      col <- unname(line_colors[lns])
      e <- df |>
        e_charts(date) |>
        e_line(
          value,
          name = "Observado",
          symbol = "none",
          smooth = FALSE,
          lineStyle = list(width = if (show_trend) 1.2 else 2.2, color = col),
          itemStyle = list(color = col)
        )

      if (show_trend && any(!is.na(df$trend_stl %||% NA))) {
        e <- e |>
          e_line(
            trend_stl,
            name = "Tendência (STL)",
            symbol = "none",
            smooth = TRUE,
            lineStyle = list(width = 2.8, color = col, type = "solid"),
            itemStyle = list(color = col)
          )
      }
    } else {
      df <- df |>
        mutate(
          line_label = factor(
            unname(line_labels[line_number]),
            levels = unname(line_labels[lns])
          )
        ) |>
        arrange(line_label, date)
      cols <- unname(line_colors[lns])

      e <- df |>
        group_by(line_label) |>
        e_charts(date) |>
        e_line(
          value,
          symbol = "none",
          smooth = FALSE,
          lineStyle = list(width = 2.2),
          emphasis = list(focus = "series", lineStyle = list(width = 3))
        ) |>
        e_color(cols)
    }

    e |> e_metro_defaults()
  })

  output$lines_note <- renderUI({
    df <- lines_data()
    lns <- input$lines_line
    missing <- setdiff(lns, unique(df$line_number))
    if (nrow(df) == 0 || length(missing) == 0) {
      return(NULL)
    }
    div(
      class = "small text-muted px-3 pb-2",
      bs_icon("exclamation-triangle", class = "me-1"),
      paste0(
        "Sem dados para: ",
        paste(line_labels[missing], collapse = ", "),
        if (input$lines_metric == "transported") {
          " (linhas 4 e 5 não possuem passageiros transportados)."
        } else {
          " no período selecionado."
        }
      )
    )
  })

  output$dl_lines_csv <- downloadHandler(
    filename = function() {
      lns <- paste(input$lines_line, collapse = "-")
      paste0("metrosp-linhas-", lns, "-", input$lines_metric, ".csv")
    },
    content = function(file) {
      readr::write_excel_csv2(lines_data(), file)
    }
  )

  ## Stations tab ----

  # Station requested by a map-marker click, applied when the line observer
  # below rebuilds the station choices
  pending_station <- reactiveVal(NULL)

  # freezeReactiveValue() stops downstream reactives from seeing the stale
  # station/year during the flush before the update lands. Valid selections
  # are preserved so switching lines (and bookmark restore) keeps them.
  observeEvent(input$sta_line, {
    choices <- stations_by_line |>
      filter(line_number == input$sta_line) |>
      pull(station_name)
    target <- pending_station() %||% input$sta_station
    pending_station(NULL)
    freezeReactiveValue(input, "sta_station")
    updateSelectizeInput(
      session,
      "sta_station",
      choices = choices,
      selected = if (isTRUE(target %in% choices)) target else choices[1]
    )
  })

  observeEvent(input$sta_station, {
    req(input$sta_station)
    years <- sta_daily_years |>
      filter(
        line_number == input$sta_line,
        station_name == input$sta_station
      ) |>
      pull(year)
    current <- input$sta_year
    freezeReactiveValue(input, "sta_year")
    updateSelectInput(
      session,
      "sta_year",
      choices = years,
      selected = if (isTRUE(current %in% as.character(years))) {
        current
      } else if (length(years)) {
        years[1]
      }
    )
  })

  sta_monthly_data <- reactive({
    ln <- input$sta_line
    sta <- input$sta_station
    req(ln, sta)

    start <- date_or(input$sta_start, DEFAULT_START)
    df <- sta_avg |>
      filter(line_number == ln, station_name == sta, date >= start)
    show_trend <- isTRUE(input$sta_trend) && HAS_TRENDSERIES

    if (!show_trend || sum(!is.na(df$value)) < 24L) {
      return(df)
    }

    tryCatch(
      trendseries::augment_trends(
        df,
        date_col = "date",
        value_col = "value",
        group_cols = c("line_number", "station_name"),
        methods = "stl",
        params = list(robust = TRUE, s.window = 13),
        .quiet = TRUE
      ),
      error = function(e) {
        message(
          "STL trend failed for ",
          sta,
          " (L",
          ln,
          "): ",
          conditionMessage(e)
        )
        df |> mutate(trend_stl = NA_real_)
      }
    )
  }) |>
    bindCache(
      input$sta_line,
      input$sta_station,
      input$sta_start,
      input$sta_trend
    )

  sta_daily_data <- reactive({
    ln <- input$sta_line
    sta <- input$sta_station
    yr <- input$sta_year
    req(ln, sta, yr)
    sta_daily |>
      filter(line_number == ln, station_name == sta, year == as.integer(yr)) |>
      arrange(date)
  })

  output$sta_kpis <- renderUI({
    req(input$sta_station, input$sta_line)

    df_monthly <- sta_monthly_data()
    req(nrow(df_monthly) > 0)
    yr <- input$sta_year
    # Reuse the daily reactive (same line/station/year filter) instead of
    # re-filtering sta_daily, so the KPI and the daily chart can't diverge
    df_daily <- if (isTruthy(yr)) sta_daily_data() else sta_daily[0, ]

    wd_avg <- if (nrow(df_daily) > 0) {
      wd <- df_daily |> filter(as.integer(format(date, "%u")) <= 5)
      if (nrow(wd) > 0) fmt_n(mean(wd$value, na.rm = TRUE)) else "—"
    } else {
      "—"
    }

    we_avg <- if (nrow(df_daily) > 0) {
      we <- df_daily |> filter(as.integer(format(date, "%u")) >= 6)
      if (nrow(we) > 0) fmt_n(mean(we$value, na.rm = TRUE)) else "—"
    } else {
      "—"
    }

    # full series (not start-date filtered) so the prior-year month is
    # available even when the visible window is shorter than 12 months
    mo_full <- sta_avg |>
      filter(
        line_number == input$sta_line,
        station_name == input$sta_station,
        !is.na(value)
      )
    latest_mo <- mo_full |> slice_max(date, n = 1)
    mom_yoy_val <- "—"
    mom_yoy_sub <- ""
    if (nrow(latest_mo) == 1) {
      # same calendar month one year earlier (monthly dates, so exact match)
      prev_mo_date <- seq(latest_mo$date, by = "-1 year", length.out = 2)[2]
      prev_mo <- mo_full |> filter(date == prev_mo_date)
      if (nrow(prev_mo) == 1 && prev_mo$value > 0) {
        mom_yoy_val <- fmt_pct((latest_mo$value / prev_mo$value - 1) * 100)
      }
      mom_yoy_sub <- paste0(
        fmt_month_pt(latest_mo$date),
        " vs. ",
        fmt_month_pt(prev_mo_date)
      )
    }

    latest <- max(df_monthly$date, na.rm = TRUE)
    recent <- df_monthly |> filter(date > latest - 365, !is.na(value))
    prior <- df_monthly |>
      filter(date > latest - 730, date <= latest - 365, !is.na(value))
    yoy_label <- if (nrow(recent) >= 6 && nrow(prior) >= 6) {
      yoy <- (mean(recent$value, na.rm = TRUE) /
        mean(prior$value, na.rm = TRUE) -
        1) *
        100
      fmt_pct(yoy)
    } else {
      "—"
    }

    yr_label <- if (!is.null(yr) && nzchar(yr)) yr else ""

    div(
      class = "kpi-grid kpi-grid-4",
      kpi_card(
        "Média dias úteis",
        wd_avg,
        paste0("embarques/dia — ", yr_label)
      ),
      kpi_card(
        "Média fins de semana",
        we_avg,
        paste0("embarques/dia — ", yr_label)
      ),
      kpi_card("Variação mensal (a/a)", mom_yoy_val, mom_yoy_sub),
      kpi_card("Variação anual", yoy_label, "últimos 12m vs. anteriores")
    )
  })

  output$sta_monthly_title <- renderText({
    paste0(input$sta_station, " — Média dias úteis (mensal)")
  })

  output$sta_chart <- renderEcharts4r({
    df <- sta_monthly_data()
    validate(need(nrow(df) > 0, "Sem dados mensais para esta estação."))

    col <- unname(line_colors[input$sta_line])
    show_trend <- isTRUE(input$sta_trend) && HAS_TRENDSERIES

    e <- df |>
      e_charts(date) |>
      e_line(
        value,
        name = "Observado",
        symbol = "circle",
        symbolSize = 4,
        smooth = FALSE,
        lineStyle = list(width = if (show_trend) 1.2 else 2.2, color = col),
        itemStyle = list(color = col)
      )

    if (show_trend && any(!is.na(df$trend_stl %||% NA))) {
      e <- e |>
        e_line(
          trend_stl,
          name = "Tendência (STL)",
          symbol = "none",
          smooth = TRUE,
          lineStyle = list(width = 2.8, color = col),
          itemStyle = list(color = col)
        )
    }

    # Shade opening + ramp-up window (excluded from baseline comparisons)
    rw <- ramp_windows |>
      filter(
        line_number == input$sta_line,
        station_name == input$sta_station,
        ramp_up_end >= min(df$date)
      )
    if (nrow(rw) == 1) {
      e <- e |>
        e_mark_area(
          data = list(
            list(
              xAxis = format(max(rw$inauguration_date, min(df$date))),
              name = "Abertura / ramp-up"
            ),
            list(xAxis = format(rw$ramp_up_end))
          ),
          itemStyle = list(color = "rgba(184, 144, 0, 0.12)"),
          label = list(color = "#8A6D00", fontSize = 11),
          silent = TRUE
        )
    }

    e |> e_metro_defaults(grid_bottom = 50)
  })

  output$sta_daily_title <- renderText({
    yr <- if (!is.null(input$sta_year) && nzchar(input$sta_year)) {
      input$sta_year
    } else {
      ""
    }
    paste0(input$sta_station, " — Série diária (", yr, ")")
  })

  output$sta_daily_chart <- renderEcharts4r({
    df <- sta_daily_data()
    validate(need(nrow(df) > 0, "Sem dados diários para o ano selecionado."))

    col <- unname(line_colors[input$sta_line])
    df <- df |> mutate(rolling7 = roll_mean(value))

    e <- df |>
      e_charts(date) |>
      e_line(
        value,
        name = "Diário",
        symbol = "none",
        smooth = FALSE,
        lineStyle = list(width = 1, color = "#C8CAD3")
      )

    if (nrow(df) >= 7L) {
      e <- e |>
        e_line(
          rolling7,
          name = "Média 7 dias",
          symbol = "none",
          smooth = FALSE,
          lineStyle = list(width = 2.4, color = col),
          connectNulls = FALSE
        )
    }

    e |>
      e_metro_defaults(grid_bottom = 50) |>
      e_legend(top = 0, itemWidth = 12, itemHeight = 6)
  })

  output$dl_sta_csv <- downloadHandler(
    filename = function() {
      sta_slug <- gsub(" ", "-", tolower(input$sta_station))
      paste0("metrosp-estacao-", sta_slug, "-mensal.csv")
    },
    content = function(file) {
      readr::write_excel_csv2(sta_monthly_data(), file)
    }
  )

  output$dl_sta_daily_csv <- downloadHandler(
    filename = function() {
      sta_slug <- gsub(" ", "-", tolower(input$sta_station))
      paste0("metrosp-estacao-", sta_slug, "-diario-", input$sta_year, ".csv")
    },
    content = function(file) {
      readr::write_excel_csv2(sta_daily_data(), file)
    }
  )

  ## Map tab ----

  # Adds markers + legend for the given metric and toggles the line variant.
  # Only additive so it works on both the fresh widget and a leafletProxy;
  # the proxy path clears the "stations" group first (addLegend replaces
  # itself via layerId).
  map_draw_stations <- function(m, metric, year = max(MAP_YEARS)) {
    df <- sf_stations_map

    if (metric == "rede") {
      # transit convention: interchange hubs are white with a dark stroke
      hub <- df$n_lines > 1
      fill <- ifelse(hub, "#FFFFFF", unname(line_colors[df$first_line]))
      stroke <- ifelse(hub, "#0E1130", "#FFFFFF")
      stroke_opacity <- 1
      m <- m |> showGroup("lines_color") |> hideGroup("lines_neutral")
    } else {
      # markers missing in the chosen year fall to the "sem dados" gray,
      # which is what makes stations visibly appear/vanish in the animation
      demand_year <- map_demand_by_year[[as.character(year)]]
      fill <- switch(
        metric,
        demanda = map_bin_color(demand_year, map_seq_breaks, map_seq_colors),
        vs2019 = map_bin_color(
          df$pct_2019,
          map_div_breaks$vs2019,
          map_div_colors
        ),
        yoy = map_bin_color(df$pct_yoy, map_div_breaks$yoy, map_div_colors)
      )
      # soft ink ring keeps the light ramp steps visible on the pale tiles
      stroke <- "#0E1130"
      stroke_opacity <- 0.35
      m <- m |> showGroup("lines_neutral") |> hideGroup("lines_color")
    }

    leg <- switch(
      metric,
      rede = list(
        colors = unname(line_colors),
        labels = unname(line_labels),
        title = "Linhas"
      ),
      demanda = list(
        colors = c(map_seq_colors, map_na_color),
        labels = c(map_seq_labels, "sem dados"),
        title = paste0("Embarques/dia útil (", year, ")")
      ),
      vs2019 = list(
        colors = c(map_div_colors, map_na_color),
        labels = c(map_div_labels$vs2019, "sem dados"),
        title = "Últimos 12m vs. 2019"
      ),
      yoy = list(
        colors = c(map_div_colors, map_na_color),
        labels = c(map_div_labels$yoy, "sem dados"),
        title = "Últimos 12m vs. anteriores"
      )
    )

    # In Demanda mode the tooltip and popup must quote the selected year,
    # otherwise the slider looks inert: bins are coarse, so many fills
    # survive a year change and the static 12m numbers never move
    if (metric == "demanda") {
      hover <- lapply(seq_len(nrow(df)), function(i) {
        demand <- if (is.na(demand_year[i])) {
          paste0("Sem dados em ", year)
        } else {
          paste0(
            "Média dias úteis em ",
            year,
            ": <b>",
            fmt_n(demand_year[i]),
            "</b> pass./dia"
          )
        }
        htmltools::HTML(paste0(
          "<b>",
          df$station_name[i],
          "</b><br/>",
          df$lines_lbl[i],
          "<br/>",
          demand
        ))
      })
      year_rows <- map_metric_row(
        paste0("Média em ", year),
        ifelse(
          is.na(demand_year),
          "sem dados",
          paste0(fmt_n_vec(demand_year), " pass./dia útil")
        )
      )
      popup <- mapply(
        sub,
        replacement = year_rows,
        x = df$popup_html,
        MoreArgs = list(pattern = "{{YEAR_ROW}}", fixed = TRUE),
        USE.NAMES = FALSE
      )
    } else {
      hover <- map_hover_html
      popup <- sub("{{YEAR_ROW}}", "", df$popup_html, fixed = TRUE)
    }

    m |>
      addCircleMarkers(
        data = df,
        group = "stations",
        layerId = df$station_name,
        radius = 6,
        weight = 1.5,
        color = stroke,
        opacity = stroke_opacity,
        fillColor = fill,
        fillOpacity = 0.95,
        label = hover,
        popup = popup,
        popupOptions = popupOptions(maxWidth = 300),
        labelOptions = labelOptions(
          style = list(
            "font-family" = "Inter, -apple-system, sans-serif",
            "font-size" = "13px",
            "padding" = "8px 12px",
            "border-radius" = "6px",
            "box-shadow" = "0 2px 8px rgba(14,17,48,0.12)"
          ),
          direction = "auto"
        )
      ) |>
      addLegend(
        "bottomright",
        colors = leg$colors,
        labels = leg$labels,
        title = leg$title,
        opacity = 1,
        layerId = "map_legend"
      )
  }

  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$CartoDB.PositronNoLabels) |>
      addProviderTiles(providers$CartoDB.PositronOnlyLabels)

    if (!is.null(sf_stations_map)) {
      bbox <- sf::st_bbox(sf_stations_map)
      m <- m |>
        fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )
    } else {
      m <- m |> setView(lng = -46.633, lat = -23.555, zoom = 12)
    }

    if (!is.null(sf_lines)) {
      # all casings first so a later line's white halo never cuts an earlier
      # line's colored stroke at crossings
      for (ln in LINES) {
        geom <- sf_lines |> filter(line_number == ln)
        if (nrow(geom) > 0) {
          m <- m |>
            addPolylines(
              data = geom,
              color = "#FFFFFF",
              weight = 7,
              opacity = 1,
              group = "lines_casing"
            )
        }
      }
      for (ln in LINES) {
        geom <- sf_lines |> filter(line_number == ln)
        if (nrow(geom) > 0) {
          m <- m |>
            addPolylines(
              data = geom,
              color = line_colors[ln],
              weight = 4,
              opacity = 0.95,
              label = line_labels[ln],
              group = "lines_color",
              highlightOptions = highlightOptions(weight = 6, opacity = 1)
            ) |>
            addPolylines(
              data = geom,
              color = map_line_neutral,
              weight = 4,
              opacity = 0.9,
              label = line_labels[ln],
              group = "lines_neutral"
            )
        }
      }
    }

    if (!is.null(sf_stations_map)) {
      m <- map_draw_stations(
        m,
        isolate(input$map_metric) %||% "demanda",
        isolate(input$map_year) %||% max(MAP_YEARS)
      )
    }

    m
  })

  # One observer for both controls: a year change while outside Demanda
  # redraws the same thing (cheap at ~90 markers), which keeps the logic flat
  observeEvent(
    list(input$map_metric, input$map_year),
    {
      req(!is.null(sf_stations_map), input$map_metric)
      leafletProxy("map") |>
        clearGroup("stations") |>
        map_draw_stations(
          input$map_metric,
          input$map_year %||% max(MAP_YEARS)
        )
    },
    ignoreInit = TRUE
  )

  # Fired by the "Ver série" links inside marker popups (via
  # Shiny.setInputValue); links are only rendered for line-stations that
  # have demand data
  observeEvent(input$map_go_station, {
    id <- input$map_go_station
    req(is.character(id), nzchar(id))
    parts <- strsplit(id, "||", fixed = TRUE)[[1]]
    req(length(parts) == 2)
    ln <- parts[1]
    sta <- parts[2]

    nav_select("main_nav", "estacoes")
    if (identical(input$sta_line, ln)) {
      updateSelectizeInput(session, "sta_station", selected = sta)
    } else {
      pending_station(sta)
      updateSelectInput(session, "sta_line", selected = ln)
    }
  })

  ## Download handlers ----
  # Serve the package datasets verbatim so downloads match the documented
  # schemas (charts use filtered/renamed copies; downloads must not)

  make_csv_handler <- function(data, prefix) {
    downloadHandler(
      filename = function() paste0("metrosp-", prefix, ".csv"),
      content = function(file) {
        readr::write_excel_csv2(data, file)
      }
    )
  }

  make_xlsx_handler <- function(data, prefix) {
    downloadHandler(
      filename = function() paste0("metrosp-", prefix, ".xlsx"),
      content = function(file) writexl::write_xlsx(data, file)
    )
  }

  output$dl_ent_csv <- make_csv_handler(
    metrosp::passengers_entrance,
    "passengers-entrance"
  )
  output$dl_ent_xlsx <- make_xlsx_handler(
    metrosp::passengers_entrance,
    "passengers-entrance"
  )
  output$dl_trans_csv <- make_csv_handler(
    metrosp::passengers_transported,
    "passengers-transported"
  )
  output$dl_trans_xlsx <- make_xlsx_handler(
    metrosp::passengers_transported,
    "passengers-transported"
  )
  output$dl_staavg_csv <- make_csv_handler(
    metrosp::station_averages,
    "station-averages"
  )
  output$dl_staavg_xlsx <- make_xlsx_handler(
    metrosp::station_averages,
    "station-averages"
  )
  output$dl_stadaily_csv <- make_csv_handler(
    metrosp::station_daily,
    "station-daily"
  )
  output$dl_stadaily_xlsx <- make_xlsx_handler(
    metrosp::station_daily,
    "station-daily"
  )

  make_spatial_handler <- function(sf_data, prefix, driver) {
    ext <- tolower(driver)
    downloadHandler(
      filename = function() paste0("metrosp-", prefix, ".", ext),
      content = function(file) {
        # Write to a correctly-suffixed tempfile first: Shiny's download path
        # has no extension, and GDAL (esp. GPKG/GeoJSON) is picky about that
        # and about pre-existing files. delete_dsn guards repeat downloads.
        tmp <- tempfile(fileext = paste0(".", ext))
        on.exit(unlink(tmp), add = TRUE)
        sf::st_write(
          sf_data,
          tmp,
          driver = driver,
          quiet = TRUE,
          delete_dsn = TRUE
        )
        file.copy(tmp, file, overwrite = TRUE)
      }
    )
  }

  output$dl_lines_gpkg <- make_spatial_handler(metrosp::lines, "lines", "GPKG")
  output$dl_lines_geojson <- make_spatial_handler(
    metrosp::lines,
    "lines",
    "GeoJSON"
  )
  output$dl_stations_gpkg <- make_spatial_handler(
    metrosp::stations,
    "stations",
    "GPKG"
  )
  output$dl_stations_geojson <- make_spatial_handler(
    metrosp::stations,
    "stations",
    "GeoJSON"
  )
}
