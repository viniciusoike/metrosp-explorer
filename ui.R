# UI for the Metro SP explorer ----
# Objects referenced here (line_labels, LINES, metro_theme, DATA_MIN/MAX,
# HAS_TRENDSERIES, make_download_card, …) come from global.R.

function(request) {
  page_navbar(
    id = "main_nav",
    title = tags$span(
      bs_icon("train-front-fill", size = "1.05em", class = "me-2"),
      "Metro SP — Explorador de Dados"
    ),
    theme = metro_theme,
    lang = "pt-BR",
    fillable = FALSE,
    header = tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
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
          dateInput(
            "lines_start",
            "Início da série",
            value = DEFAULT_START,
            min = DATA_MIN,
            max = DATA_MAX,
            language = "pt-BR"
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
          hr(),
          tags$p(
            class = "text-muted small mb-0",
            "Dados mensais. ",
            if (HAS_TRENDSERIES) {
              "Tendência extraída via decomposição STL robusta (s.window = 13)."
            } else {
              "Instale o pacote trendseries para habilitar tendência STL."
            }
          )
        ),

        uiOutput("lines_kpis"),

        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex align-items-center justify-content-between gap-2",
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
          echarts4rOutput("lines_chart", height = "480px"),
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
          dateInput(
            "sta_start",
            "Início da série",
            value = DEFAULT_START,
            min = DATA_MIN,
            max = DATA_MAX,
            language = "pt-BR"
          ),
          if (HAS_TRENDSERIES) {
            checkboxInput("sta_trend", "Mostrar tendência (STL)", value = FALSE)
          },
          hr(),
          selectInput("sta_year", "Ano (série diária)", choices = NULL),
          hr(),
          tags$p(
            class = "text-muted small mb-0",
            "A data de início filtra o gráfico mensal. ",
            "O seletor de ano controla o gráfico diário. ",
            if (HAS_TRENDSERIES) {
              "Tendência STL disponível para dados mensais."
            } else {
              "Instale o pacote trendseries para habilitar tendência STL."
            }
          )
        ),

        uiOutput("sta_kpis"),

        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex align-items-center justify-content-between gap-2",
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
          echarts4rOutput("sta_chart", height = "380px")
        ),

        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex align-items-center justify-content-between gap-2",
            textOutput("sta_daily_title", inline = TRUE),
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
          ),
          echarts4rOutput("sta_daily_chart", height = "320px")
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
          "Linhas e estações do Metrô de São Paulo",
          tags$small(
            class = "ms-2 text-muted",
            "círculos proporcionais à demanda — clique para abrir a estação"
          )
        ),
        if (!is.null(sf_lines) || !is.null(sf_stations)) {
          leafletOutput("map", height = "600px")
        } else {
          div(
            class = "station-empty",
            div(class = "empty-icon", bs_icon("geo-alt")),
            div(class = "empty-title", "Dados espaciais indisponíveis"),
            div(
              class = "empty-text",
              "Não foi possível carregar os dados geográficos de linhas e estações."
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

      div(
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
          card_header("Sobre o pacote metrosp"),
          card_body(
            tags$p(
              "O ",
              tags$b("metrosp"),
              " é um pacote R de dados que disponibiliza ",
              sprintf(
                "informações de demanda de passageiros do Metrô de São Paulo (%s–%s). ",
                format(DATA_MIN, "%Y"),
                format(DATA_MAX, "%Y")
              ),
              "Similar ao ",
              tags$code("nycflights13"),
              ", o pacote contém apenas datasets, ",
              "sem funções voltadas ao usuário."
            ),
            tags$p(
              sprintf(
                "Este explorador cobre o período de %s a %s (a visualização padrão começa em %s), ",
                fmt_month_pt(DATA_MIN),
                fmt_month_pt(DATA_MAX),
                fmt_month_pt(DEFAULT_START)
              ),
              "permitindo visualização rápida e download em múltiplos formatos."
            ),
            tags$h6("Links"),
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
                href = "https://github.com/viniciusoike/metrosp/issues",
                target = "_blank",
                "Reportar problema"
              ))
            ),
            tags$h6("Licença"),
            tags$p(class = "small text-muted", "MIT")
          )
        ),

        card(
          card_header("Fontes de dados"),
          card_body(
            tags$h6("Demanda de passageiros"),
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
                tags$b("Linhas 4 e 5: "),
                "Insper Dataverse (doi:10.60873/FK2/UTGQ0I)"
              )
            ),
            tags$h6("Dados espaciais"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                href = "https://geosampa.prefeitura.sp.gov.br/",
                target = "_blank",
                "GeoSampa — Prefeitura de São Paulo"
              ))
            ),
            tags$h6("Limitações conhecidas"),
            tags$ul(
              class = "small text-muted",
              tags$li("Linhas 4/5 — passageiros transportados não disponíveis"),
              tags$li("Linhas 4/5 — código de estação é NA"),
              tags$li("2017: dados disponíveis apenas de outubro a dezembro"),
              tags$li(
                sprintf(
                  "Meses após %s ainda não publicados pela fonte",
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
        " · Fontes: METRO SP · Insper Dataverse · GeoSampa"
      )
    )
  )
}
