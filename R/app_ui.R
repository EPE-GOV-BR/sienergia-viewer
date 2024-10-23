#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  header <- dashboardHeader(
    title = HTML('<img class=logo-epe src=www/EPE_PRIMARIO4.png width="70" > SIEnergia'),
    titleWidth = 120,
    tags$li(
      class = "dropdown",
      tags$a(id = "page_header", class = "dropdown-toggle", "SIEnergia")
    )
  )

  sidebar <- dashboardSidebar(
    width = 120,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem(div(tags$img(src = "www/sienergia_icon.png", width = "22px", height = "22px"), str_glue("{traduz('SIEnergia')}")), tabName = "introducao_inicio"),
      #menuItem(str_glue("{traduz('O que ?')}"), tabName = "introducao_oque", icon = icon("lightbulb")),
      #menuItem(str_glue("{traduz('Como ?')}"), tabName = "introducao_como", icon = icon("puzzle-piece")),
      #menuItem(str_glue("{traduz('Diagrama')}"), tabName = "introducao_diagrama", icon = icon("diagram-project")),
      # menuItem(str_glue("{traduz('Modelo')}"), tabName = "modelo", icon = icon("table")),
      # menuItem(str_glue("{traduz('Produto')}"), tabName = "producao", icon = icon("chart-column")),
      # menuItem(str_glue("{traduz('Energético')}"), tabName = "energetico", icon = icon("sack-xmark")),
      # menuItem(str_glue("{traduz('Combustível')}"), tabName = "combustivel", icon = icon("gas-pump")),
      # menuItem(str_glue("{traduz('Energia')}"), tabName = "energia", icon = icon("bolt")),
      menuItem("Simulador", tabName = "simulador", icon = icon("calculator"))
      #menuItem(str_glue("{traduz('Glossário')}"), tabName = "glossario", icon = icon("list"))
    )
  )


  body <- dashboardBody(
    withMathJax(),
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName = "introducao_inicio",
        tags$div(
          class = "painel_inicial",
          tags$div(
            style = "display: flex;justify-content: center;",
            tags$div(
              tags$img(
                height = 289,
                width = 701,
                src = "www/sienergia_inicio.png"
              ),
              tags$div(
                style = "background-color: darkgray; text-align: center; color: white; font-weight: bold; font-size:20px;",
                "COPRODUTOS DA AGROPECUÁRIA, INDUSTRIA E SANEAMENTO"
              ),
              HTML("<h1><strong>Um enorme potencial energético disponível</strong></h1>")
            )
          ),
          tags$div(
            tags$div(
              style = "display:flex; justify-content: space-around;",
              tags$img(height = 70, width = 290, src = "www/sienergia_governo.png"),
              tags$text("Versão: 4.1"),
              tags$text("Publicação: 02/06/2023")
            ),
            tags$div(
              style = "height:5px; background-color: #0c2340; margin-top: 10px; margin-bottom: 10px;"
            ),
            tags$div(
              style = "display:flex;justify-content:space-around;",
              tags$img(height = 93, width = 170, src = "www/sienergia_epe.png"),
              tags$text(style = "width: 420px; padding: 6px; font-size: 12px;", texto_inicio),
              tags$text(list(strong("Coordenação Executiva"), h6("Carla da C. Lopes Achão"), strong("Coordenação Técnica"), h6("Daniel Kühner Coelho"), h6("Luciano Basto Oliveira"))),
              tags$text(list(strong("Desenvolvimento"), h6("Bruno Crotman"), h6("Daniel Kühner Coelho"), h6("Gustavo Martins F. de Aquino"))),
              tags$text(list(strong("Equipe Técnica"), h6("Daniel Kühner Coelho")))
            )
          )
        )
      ),
      tabItem(
        tabName = "introducao_oque",
        tags$div(
          class = "painel_introducao",
          tags$img(
            height = 630,
            width = 1120,
            src = "www/sienergia_oque_completo.png"
          )
        )
      ),
      tabItem(
        tabName = "introducao_como",
        tags$div(
          class = "painel_introducao",
          tags$img(
            height = 630,
            width = 1120,
            src = "www/sienergia_como_completo.png"
          )
        )
      ),
      tabItem(
        tabName = "introducao_diagrama",
        tags$div(
          class = "painel_introducao",
          tags$img(
            style = "padding:10px;",
            height = 630,
            width = 1120,
            src = "www/sienergia_rotas(2).png"
          )
        )
      ),
      tabItem(
        tabName = "modelo",
        mod_tela_modelo_ui(
          id = "modelo",
          titulo = "Modelo"
        )
      ),
      tabItem(
        tabName = "producao",
        mod_tela_produto_ui(
          id = "producao",
          titulo = "Produto"
        )
      ),
      tabItem(
        tabName = "energetico",
        mod_tela_energetico_ui(
          id = "energetico",
          titulo = "Energético"
        )
      ),
      tabItem(
        tabName = "combustivel",
        mod_tela_combustivel_ui(
          id = "combustivel",
          titulo = "Combustivel"
        )
      ),
      tabItem(
        tabName = "energia",
        mod_tela_energia_ui(
          id = "energia"
        )
      ),
      tabItem(
        tabName = "simulador",
        mod_tela_simulador_ui("tela_simulador_1")
      ),
      tabItem(
        tabName = "glossario",
        tags$div(
          style = "display: flex; flex-direction: row; height: 95vh; justify-content: space-around;",
          tags$div(
            class = "bloco_glossario",
            HTML("
                  <h1><strong>Legenda</strong></h1>
                  <h3><strong>Fatores</strong></h3>
                    <ul>
                      <li><strong>Rend</strong> - Rendimento ou produtividade (kg/ha).</li>
                      <li><strong>Lot</strong> - Lotação (cbc/ha).</li>
                      <li><strong>FCoprod.</strong> - Fator de correlação entre a produção do produto principal e do coproduto (t/tbs).</li>
                      <li><strong>FColeta</strong> - Fator mássico de Coleta no campo (%).</li>
                      <li><strong>FDensif.</strong> - Fator mássico de Densificação do Energético para o Combustível (%).</li>
                      <li><strong>FMetan.</strong> - Fator de Metanização do Energético para o Combustível (m3 CH4/t).</li>
                      <li><strong>PCI</strong> - Poder calorífico inferior (tep/t).</li>
                      <li><strong>RCald</strong> - Rendimento da Caldeira.</li>
                      <li><strong>RTurb</strong> - Rendimento da Unidade Termelétrica.</li>
                      <li><strong>RTE</strong> - Rendimento Termelétrico (Energia Renovável, 2016) - Ciclo a vapor ou motogerador.</li>
                      <li><strong>FCap</strong> - Fator de capacidade médio de Usinas Termelétricas (PNE 2050).</li>
                    </ul>
                  <h3><strong>Medidas</strong></h3>
                    <ul>
                      <li><strong>Combustível (ktep)</strong> - Conversão de Combustível (mil t) e Combustível (M m3) em tep.</li>
                      <li><strong>Calor (TJ)</strong> - Potencial de geração de calor a partir dos combustíveis pela Rota de Densificação.</li>
                      <li><strong>Energia (TJ)</strong> - Conversão de unidades da energia elétrica potencial (Eletricidade (GWh))</li>
                    </ul>")
          ),
          tags$div(
            class = "bloco_glossario",
            HTML("
                  <h1><strong>Novidades da Versão 4</strong></h1>
                  <h3><strong>Ajustes e novas funcionalidades</strong></h3>
                    <ul>
                      <li>Sistema totalmente reestruturado internamente, com desempenho otimizado.</li>
                      <li>Menu ainda mais intuitivo e amigável.</li>
                      <li>Melhoria nos filtros (segmentações) dos relatórios, agora mais integrados.</li>
                      <li>Mapas em nível municipal.</li>
                      <li>Ajustes no modo de visualização por celular.</li>
                    </ul>
                  <h3><strong>Relatórios Dinâmicos</strong></h3>
                    <ul>
                      <li>Criação de novo relatório, chamado Energia.</li>
                    </ul>
                  <h3><strong>Dados e Informações</strong></h3>
                    <ul>
                      <li>Atualização de todos os dados até 2021.</li>
                      <li>Reformulação do Diagrama do Modelo.</li>
                      <li>Módulo de População.</li>
                      <ul><li>Inclusão de dados de população residente estimada.</li></ul>
                      <li>Módulo de Produtos Florestais.</li>
                      <ul><li>Cálculos de área ocupada por produtos florestais.</li></ul>
                      <li>Módulo da Pecuária.</li>
                      <ul><li>Cálculos de área ocupada pela Pecuária bovina, de corte e leite.</li></ul>
                      <li>Módulo de Saneamento.</li>
                      <ul><li>Inclusão de dados de Efluentes.</li></ul>
                      <li>Módulo Industrial (Alimentos).</li>
                      <ul><li>Estimativas de abates (carcaça) em nível municipal.</li></ul>
                    </ul>")
          )
        )
      )
    )
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      header,
      sidebar,
      body
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )


  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "sienergiaviewer"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
