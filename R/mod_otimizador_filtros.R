#' otimizador_filtros UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_otimizador_filtros_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "filtros",
      shinydashboard::box(
        title = "Opções da simulação",
        width = 12,
        fluidRow(
          column(
            width = 8,
            shiny::p(
              "As referências para os valores padrão dos filtros estão no",
              a(href = "www/simulador.html", target = "_blank", "manual.")
            )
          ),
          column(
            width = 2,
            class = "text-right",
            a(
              href = "www/simulador.html",
              target = "_blank",
              tags$button("Manual do simulador", class = "botao-manual")
            )
          ),
          column(
            width = 2,
            class = "text-right",
            a(
              href = "www/documentacao.pdf",
              target = "_blank",
              tags$button("Documentação do algoritmo", class = "botao-manual")
            )
          )
        ),
        hr(),
        fluidRow(
          style = "overflow: visible;",
          column(
            width = 2,
            shinyWidgets::pickerInput(
              inputId = ns("rotas"),
              label = "Rotas econômicas",
              choices = rotas_nome(),
              selected = rotas_nome(),
              multiple = TRUE,
              options = pickerInput_opcoes(),
              width = "95%"
            ),
            br(),
            br(),
            actionButton(
              inputId = ns("botao_mais_parametros"),
              label = "Parâmetros das rotas econômicas",
              icon = icon("gears"),
              width = "95%"
            ) |>
              bsplus::bs_attach_modal("filtros_rotas"),
          ),
          column(
            width = 2,
            shinyWidgets::pickerInput(
              inputId = ns("regiao_escopo"),
              label = "Regiões de escopo",
              choices = pegar_estados_com_dados(),
              selected = pegar_estados_com_dados()[1],
              multiple = TRUE,
              options = pickerInput_opcoes(),
              width = "95%"
            ),
            br(),
            br(),
            actionButton(
              inputId = ns("botao_parametros_modelo"),
              label = "Parâmetros extras",
              icon = icon("gears"),
              width = "95%"
            ) |>
              bsplus::bs_attach_modal("filtros_modelo")
          ),
          column(
            width = 2,
            shinyWidgets::pickerInput(
              inputId = ns("produtos"),
              label = "Produtos",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              width = "95%",
              options = pickerInput_opcoes(),
            )
          ),
          column(
            width = 2,
            numericInput(
              inputId = ns("num_max_usinas_por_sede"),
              label = "Nº máximo de usinas por sede",
              value = 100,
              min = 1,
              max = 1000,
              width = "95%"
            ), 
            br(),
            numericInput(
              inputId = ns("populacao_minima"),
              label = "População mínima para considerar uma sede",
              value = 100000,
              min = 0,
              width = "95%"
            )
          ),
          column(
            width = 2,
            numericInput(
              inputId = ns("num_c"),
              label = "Nº máximo de sedes testadas",
              value = 100,
              min = 1,
              width = "95%"
            ),
            br(),
            checkboxInput(
              inputId = ns("considerar_sazonalidade"),
              label = "Considerar a sazonalidade da produção agrícola",
              value = TRUE,
              width = "95%"
            )
          ),
          column(
            width = 2,
            tags$label(
              class = "control-label",
              "Limitadores",
              style = "margin-bottom: 0;"
            ),
            checkboxInput(
              inputId = ns("limitar_por_demanda"),
              label = "Limitar por demanda",
              value = TRUE,
              width = "95%"
            ),
            checkboxInput(
              inputId = ns("limitar_tamanho_da_usina"),
              label = "Limitar a produção de energia elétrica pelo tamanho da usina (5MW)",
              value = TRUE,
              width = "95%"
            )
          )
        ),
        bsplus::bs_modal(
          id = "filtros_rotas",
          title = "Parâmetros das rotas econômicas",
          size = "large",
          footer = bsplus::bs_modal_closebutton("Fechar"),
          body = tagList(
            navlistPanel(
              widths = c(3, 9),
              tabPanel(
                title = "Densificação - Energia elétrica - Resíduos agrícolas",
                mod_parametros_dens_ui(ns("parametros_dens_1"))
              ),
              tabPanel(
                title = "Biodigestão - Combustível - Resíduos agrícolas",
                mod_parametros_biocomb_ui(ns("parametros_biocomb_1"))
              ),
              tabPanel(
                title = "Biodigestão - Combustível - Resíduos pecuários",
                mod_parametros_biocomb_pec_ui(ns("parametros_biocomb_pec_1"))
              ),
              tabPanel(
                title = "Biodigestão - Energia elétrica - Resíduos pecuários",
                mod_parametros_bioeletr_pec_ui(ns("parametros_bioeletr_pec_1"))
              ),
              tabPanel(
                title = "Biodigestão - Energia elétrica - Resíduos agrícolas",
                mod_parametros_bioeletr_ui(ns("parametros_bioeletr_1"))
              ),
              tabPanel(
                title = "Co-firing - Resíduos agrícolas",
                mod_parametros_cofiring_ui(ns("parametros_cofiring_1"))
              )
            )
          )
        ),
        bsplus::bs_modal(
          id = "filtros_modelo",
          title = "Parâmetros extras",
          size = "large",
          footer = bsplus::bs_modal_closebutton("Fechar"),
          body = tagList(
            navlistPanel(
              widths = c(3, 9),
              tabPanel(
                title = "Transporte",
                fluidRow(
                  column(
                    width = 6,
                    checkboxInput(
                      inputId = ns("fator_frete_ida_volta"),
                      label = "Considerar frete para ida e volta no cálculo do combustível consumido e distância percorrida",
                      value = TRUE,
                      width = "95%"
                    )
                  )
                )
              ),
              tabPanel(
                title = "Externalidades",
                mod_parametros_externalidades_ui(ns("parametros_externalidades_1"))
              )
            )
          )
        )
      )
    )
  )
}

#' otimizador_filtros Server Functions
#'
#' @noRd
mod_otimizador_filtros_server <- function(id, con, parametros) {
  moduleServer(id, function(input, output, session) {
    # Aqui deve ter uma linha por rota
    parametros_dens <- mod_parametros_dens_server("parametros_dens_1", parametros)
    parametros_biocomb <- mod_parametros_biocomb_server("parametros_biocomb_1", parametros)
    parametros_bioeletr <- mod_parametros_bioeletr_server("parametros_bioeletr_1", parametros)
    parametros_biocomb_pec <- mod_parametros_biocomb_pec_server("parametros_biocomb_pec_1", parametros)
    parametros_bioeletr_pec <- mod_parametros_bioeletr_pec_server("parametros_bioeletr_pec_1", parametros)
    parametros_cofiring <- mod_parametros_cofiring_server("parametros_cofiring_1", parametros)

    # Parâmetros pop-up extras
    parametros_externalidades <- mod_parametros_externalidades_server("parametros_externalidades_1", con)

    df_produtos <- dplyr::tbl(con, "carga_energia_brasil") |>
      dplyr::distinct(producao, produto_codigo, produto_nome) |>
      dplyr::collect() 
    
      
      produtos_possiveis <- df_produtos |> 
      dplyr::group_by(producao) |>
      dplyr::arrange(produto_nome, .by_group = TRUE) |>
      dplyr::summarise(
        produto_filtro = list(as.list(purrr::set_names(produto_codigo, produto_nome)))
      ) |>
      tibble::deframe()

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "produtos",
      choices = produtos_possiveis,
      selected =  df_produtos$produto_codigo
    )

    parametros <- reactive({
      list(
        gerais = list(
          rotas = input$rotas,
          regiao_escopo = input$regiao_escopo,
          num_c = input$num_c,
          num_max_usinas_por_sede = input$num_max_usinas_por_sede,
          limitar_por_demanda = input$limitar_por_demanda,
          limitar_tamanho_da_usina = input$limitar_tamanho_da_usina,
          produtos = input$produtos,
          externalidades = parametros_externalidades(),
          fator_frete_ida_volta = input$fator_frete_ida_volta,
          considerar_sazonalidade = input$considerar_sazonalidade,
          populacao_minima = input$populacao_minima
        ),
        # deve ter uma linha por rota
        dens = parametros_dens(),
        biocomb = parametros_biocomb(),
        bioeletr = parametros_bioeletr(),
        biocomb_pec = parametros_biocomb_pec(),
        bioeletr_pec = parametros_bioeletr_pec(),
        cofiring = parametros_cofiring()
      )
    })


    return(parametros)
  })
}

## To be copied in the UI
# mod_otimizador_filtros_ui("otimizador_filtros_1")

## To be copied in the server
# mod_otimizador_filtros_server("otimizador_filtros_1")
