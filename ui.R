# UI for the Metro SP explorer ----
# Objects referenced here (line_labels, LINES, metro_theme, DATA_MIN/MAX,
# HAS_TRENDSERIES, make_download_card, …) come from global.R.

function(request) {
  page_navbar(
    id = "main_nav",
    # the brand is the page title, so it carries the h1 the card-header h2s
    # need above them; .navbar-title resets the browser heading scale
    # the brand is the page title and doubles as the home link: the h1
    # keeps the heading scale and fires brand_home (click + Enter) instead
    # of nesting an <a> inside bslib's own .navbar-brand anchor
    title = tags$h1(
      class = "navbar-title navbar-brand-home",
      role = "link",
      tabindex = "0",
      title = "Ir para o Início",
      onclick = "Shiny.setInputValue('brand_home', Date.now(), {priority: 'event'})",
      onkeydown = "if (event.key === 'Enter') Shiny.setInputValue('brand_home', Date.now(), {priority: 'event'})",
      bs_icon("train-front-fill", size = "1.05em", class = "me-2"),
      "Metrô em Dados"
    ),
    # without this the browser tab shows the icon's raw SVG markup: bslib
    # flattens the HTML `title` into <title> verbatim
    window_title = "Metrô em Dados",
    theme = metro_theme,
    # dark navbar: light links and a light toggler icon on small screens
    navbar_options = navbar_options(theme = "dark"),
    lang = "pt-BR",
    # the three chart tabs fill the viewport so the charts scale with the
    # window; Início, Download and Sobre stay as normal scrolling pages
    fillable = c("linhas", "estacoes", "mapa"),
    header = tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      )
    ),

    ## Tab: Início ----
    nav_panel(
      title = "Início",
      value = "inicio",
      icon = bs_icon("house"),

      div(
        class = "home-hero",
        tags$h2(
          class = "home-title",
          "Explore a demanda do Metrô de São Paulo"
        ),
        tags$p(
          class = "home-lede",
          sprintf(
            paste0(
              "Séries mensais por linha, médias por estação e o mapa da rede, ",
              "de %s a %s."
            ),
            fmt_month_pt(DATA_MIN),
            fmt_month_pt(DATA_MAX)
          )
        ),
        tags$p(
          class = "home-cover",
          "Dados do pacote ",
          tags$a(
            href = "https://viniciusoike.github.io/metrosp/",
            target = "_blank",
            "{metrosp}"
          ),
          " · escolha um ponto de partida abaixo"
        )
      ),

      layout_column_wrap(
        width = 1 / 2,
        card(
          card_header("Linhas", container = tags$h3),
          card_body(
            tags$p(
              class = "small text-muted",
              "Embarques e transportados por linha, com KPIs e tendência."
            ),
            actionLink(
              "go_linhas",
              tagList("Explorar linhas", bs_icon("arrow-right", class = "ms-1"))
            )
          )
        ),
        card(
          card_header("Estações", container = tags$h3),
          card_body(
            tags$p(
              class = "small text-muted",
              "Média em dias úteis e série diária por estação."
            ),
            actionLink(
              "go_estacoes",
              tagList(
                "Explorar estações",
                bs_icon("arrow-right", class = "ms-1")
              )
            )
          )
        ),
        card(
          card_header("Mapa", container = tags$h3),
          card_body(
            tags$p(
              class = "small text-muted",
              "Demanda e variações na rede — clique numa estação para a série."
            ),
            actionLink(
              "go_mapa",
              tagList("Abrir o mapa", bs_icon("arrow-right", class = "ms-1"))
            )
          )
        ),
        card(
          card_header("Download", container = tags$h3),
          card_body(
            tags$p(
              class = "small text-muted",
              "Bases completas em CSV, Excel e formatos espaciais."
            ),
            actionLink(
              "go_download",
              tagList("Baixar dados", bs_icon("arrow-right", class = "ms-1"))
            )
          )
        )
      )
    ),

    ## Tab: Linhas ----
    nav_panel(
      title = "Linhas",
      value = "linhas",
      icon = bs_icon("graph-up"),

      layout_sidebar(
        sidebar = sidebar(
          title = div(class = "sidebar-title", "Filtros"),
          width = 260,
          selectizeInput(
            "lines_line",
            "Linhas",
            choices = setNames(LINES, unname(line_labels)),
            selected = "1",
            multiple = TRUE,
            options = list(
              plugins = list("remove_button"),
              placeholder = "Selecione uma ou mais linhas"
            )
          ),
          selectInput(
            "lines_metric",
            "Variável",
            choices = c(
              "Embarques (entrada)" = "entrance",
              "Passageiros transportados" = "transported"
            ),
            selected = "entrance"
          ),
          div(
            class = "pill-group",
            radioButtons(
              "lines_period",
              "Período",
              inline = TRUE,
              choices = PERIOD_CHOICES,
              selected = "inicio"
            )
          ),
          if (HAS_TRENDSERIES) {
            conditionalPanel(
              condition = "input.lines_line !== null && input.lines_line.length === 1",
              checkboxInput(
                "lines_trend",
                "Mostrar tendência (STL)",
                value = FALSE
              )
            )
          },
          conditionalPanel(
            condition = "input.lines_line !== null && input.lines_line.length === 1",
            checkboxInput(
              "lines_forecast",
              "Mostrar projeção (12 meses)",
              value = FALSE
            )
          ),
          uiOutput("lines_info")
        ),

        uiOutput("lines_kpis"),

        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex align-items-center justify-content-between gap-2",
            container = tags$h2,
            textOutput("lines_title", inline = TRUE),
            downloadButton(
              "dl_lines_csv",
              tagList(
                bs_icon("download"),
                tags$span(class = "visually-hidden", "Baixar CSV")
              ),
              icon = NULL,
              class = "btn-sm btn-link p-1 download-icon",
              title = "Baixar CSV"
            )
          ),
          echarts4rOutput("lines_chart", height = "100%"),
          uiOutput("lines_note")
        )
      )
    ),

    ## Tab: Estações ----
    nav_panel(
      title = "Estações",
      value = "estacoes",
      icon = bs_icon("pin-map-fill"),

      layout_sidebar(
        sidebar = sidebar(
          title = div(class = "sidebar-title", "Filtros"),
          width = 260,
          selectInput(
            "sta_line",
            "Linha",
            choices = setNames(LINES, unname(line_labels)),
            selected = "1"
          ),
          selectizeInput(
            "sta_station",
            "Estação",
            choices = NULL,
            options = list(placeholder = "Buscar estação...")
          ),
          div(
            class = "pill-group",
            radioButtons(
              "sta_period",
              "Período (série mensal)",
              inline = TRUE,
              choices = PERIOD_CHOICES,
              selected = "inicio"
            )
          ),
          if (HAS_TRENDSERIES) {
            checkboxInput("sta_trend", "Mostrar tendência (STL)", value = FALSE)
          },
          uiOutput("sta_info")
        ),

        uiOutput("sta_kpis"),

        # equal row heights: as plain siblings the two cards size to their
        # content and the monthly chart ends up the shorter of the two
        layout_columns(
          col_widths = 12,
          row_heights = c(1, 1),

          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex align-items-center justify-content-between gap-2",
              container = tags$h2,
              textOutput("sta_monthly_title", inline = TRUE),
              downloadButton(
                "dl_sta_csv",
                tagList(
                  bs_icon("download"),
                  tags$span(class = "visually-hidden", "Baixar CSV")
                ),
                icon = NULL,
                class = "btn-sm btn-link p-1 download-icon",
                title = "Baixar CSV"
              )
            ),
            echarts4rOutput("sta_chart", height = "100%")
          ),

          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex align-items-center justify-content-between gap-2",
              container = tags$h2,
              textOutput("sta_daily_title", inline = TRUE),
              # the year selector lives with the chart it drives, not in the
              # sidebar next to the monthly period filter
              div(
                class = "header-controls",
                div(
                  class = "header-select",
                  selectInput(
                    "sta_year",
                    # the header already reads "Série diária (2025)"
                    tags$span(class = "visually-hidden", "Ano da série diária"),
                    choices = NULL,
                    # plain <select>: a year list needs no search box, and
                    # selectize ignores the .form-select styling
                    selectize = FALSE,
                    width = "110px"
                  )
                ),
                downloadButton(
                  "dl_sta_daily_csv",
                  tagList(
                    bs_icon("download"),
                    tags$span(class = "visually-hidden", "Baixar CSV")
                  ),
                  icon = NULL,
                  class = "btn-sm btn-link p-1 download-icon",
                  title = "Baixar CSV"
                )
              )
            ),
            echarts4rOutput("sta_daily_chart", height = "100%")
          )
        )
      )
    ),

    ## Tab: Mapa ----
    nav_panel(
      title = "Mapa",
      value = "mapa",
      icon = bs_icon("geo-alt-fill"),

      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex align-items-center justify-content-between gap-3 flex-wrap",
          container = tags$h2,
          div(
            "Linhas e estações do Metrô de São Paulo",
            tags$small(
              class = "d-block text-muted fw-normal",
              "clique em uma estação para detalhes e para abrir a série completa"
            )
          ),
          div(
            class = "pill-group",
            radioButtons(
              "map_metric",
              NULL,
              inline = TRUE,
              choices = c(
                "Demanda" = "demanda",
                "Variação anual" = "yoy",
                "vs. 2019" = "vs2019",
                "Rede" = "rede"
              ),
              selected = "demanda"
            )
          )
        ),
        if (!is.null(sf_lines) || !is.null(sf_stations)) {
          tagList(
            if (!is.null(sf_stations_map)) {
              conditionalPanel(
                condition = "input.map_metric == 'demanda'",
                div(
                  class = "map-year-row",
                  sliderInput(
                    "map_year",
                    NULL,
                    min = min(MAP_YEARS),
                    max = max(MAP_YEARS),
                    value = max(MAP_YEARS),
                    step = 1,
                    sep = "",
                    ticks = FALSE,
                    width = "320px",
                    animate = animationOptions(interval = 900)
                  ),
                  tags$span(
                    class = "map-year-hint",
                    "média de dias úteis no ano — use ▶ para animar"
                  )
                )
              )
            },
            leafletOutput("map", height = "100%")
          )
        } else {
          div(
            class = "station-empty",
            div(class = "empty-icon", bs_icon("geo-alt")),
            div(class = "empty-title", "Dados espaciais indisponíveis"),
            div(
              class = "empty-text",
              "O app não conseguiu carregar os dados geográficos de linhas e estações."
            )
          )
        }
      )
    ),

    ## Tab: Download ----
    nav_panel(
      title = "Download",
      value = "download",
      icon = bs_icon("download"),

      tags$h2(
        class = "section-label",
        "Datasets disponíveis"
      ),

      layout_column_wrap(
        width = 1 / 2,
        heights_equal = "row",
        !!!lapply(download_card_configs, make_download_card)
      )
    ),

    ## Tab: Sobre ----
    nav_panel(
      title = "Sobre",
      value = "sobre",
      icon = bs_icon("info-circle-fill"),

      layout_column_wrap(
        width = 1 / 2,

        card(
          card_header("Sobre o pacote metrosp", container = tags$h2),
          card_body(
            tags$p(
              "O ",
              tags$b("metrosp"),
              " é um pacote R que reúne ",
              sprintf(
                "dados de demanda de passageiros do Metrô de São Paulo (%s–%s). ",
                format(DATA_MIN, "%Y"),
                format(DATA_MAX, "%Y")
              )
            ),
            tags$p(
              sprintf(
                "Este explorador cobre de %s a %s. ",
                fmt_month_pt(DATA_MIN),
                fmt_month_pt(DATA_MAX)
              ),
              sprintf(
                "A demanda usa a %s. ",
                DATA_SOURCE_LABEL
              ),
              "A aba Download traz as bases completas em vários formatos."
            ),
            tags$h3(class = "card-subhead", "Links"),
            tags$ul(
              tags$li(tags$a(
                href = "https://github.com/viniciusoike/metrosp",
                target = "_blank",
                "GitHub"
              )),
              tags$li(tags$a(
                href = "https://viniciusoike.github.io/metrosp/",
                target = "_blank",
                "Documentação (pkgdown)"
              )),
              tags$li(tags$a(
                href = "https://viniciusoike.r-universe.dev/metrosp",
                target = "_blank",
                "r-universe"
              )),
              tags$li(tags$a(
                href = "https://github.com/viniciusoike/metrosp/issues",
                target = "_blank",
                "Reportar problema"
              ))
            ),
            tags$h3(class = "card-subhead", "Licença"),
            tags$p(class = "small text-muted", "MIT")
          )
        ),

        card(
          card_header("Fontes de dados", container = tags$h2),
          card_body(
            tags$h3(class = "card-subhead", "Demanda de passageiros"),
            tags$ul(
              class = "small",
              tags$li(
                tags$b("Linhas 1, 2, 3 e 15: "),
                tags$a(
                  href = "https://transparencia.metrosp.com.br/dataset/demanda",
                  target = "_blank",
                  "METRO SP — Portal de Transparência"
                )
              ),
              tags$li(
                tags$b("Linha 4: "),
                "Insper Dataverse (doi:10.60873/FK2/UTGQ0I)"
              ),
              tags$li(
                tags$b("Linha 5: "),
                paste(
                  "METRO SP até jul/2018 e Insper Dataverse a partir de",
                  "ago/2018, conforme a série"
                )
              )
            ),
            tags$h3(class = "card-subhead", "Dados espaciais"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                href = "https://geosampa.prefeitura.sp.gov.br/",
                target = "_blank",
                "GeoSampa — Prefeitura de São Paulo"
              ))
            ),
            tags$h3(class = "card-subhead", "Limitações conhecidas"),
            tags$ul(
              class = "small text-muted",
              tags$li(
                paste(
                  "A soma de passageiros transportados entre linhas conta",
                  "baldeações mais de uma vez; não é um total único da rede."
                )
              ),
              tags$li(
                paste(
                  "Linha 5: a série mensal transportada por estação termina",
                  "em jul/2018; as entradas diárias continuam depois."
                )
              ),
              tags$li(
                paste(
                  "Demanda por estação publicada pelo METRO SP em 2017:",
                  "dados apenas de outubro a dezembro"
                )
              ),
              tags$li(
                sprintf(
                  "A fonte ainda não publicou os meses posteriores a %s",
                  fmt_month_pt(DATA_MAX)
                )
              )
            )
          )
        )
      )
    ),

    nav_spacer(),
    nav_item(
      tags$span(
        class = "source-tag",
        sprintf("Dados até %s", fmt_month_pt(DATA_MAX)),
        sprintf(" · %s", DATA_SOURCE_LABEL),
        " · Fonte: ",
        # metrosp consolidates and processes the raw sources (METRO SP,
        # Insper Dataverse, GeoSampa), so the package is the credited
        # source; the raw sources are detailed in the Sobre tab
        tags$a(
          href = "https://viniciusoike.github.io/metrosp/",
          target = "_blank",
          "{metrosp}"
        )
      )
    ),
    # EKIO credit badge: swap the src to "ekio-badge.svg" for the
    # badge-mark version (both live in www/)
    nav_item(
      tags$img(
        src = "ekio-badge-wordmark.svg",
        alt = "Desenvolvido por EKIO",
        title = "Desenvolvido por EKIO",
        class = "navbar-logo"
      )
    )
  )
}
