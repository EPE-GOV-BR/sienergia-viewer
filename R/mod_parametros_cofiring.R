#' parametros_cofiring UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parametros_cofiring_ui <- function(id) {
  ns <- NS(id)
  parametros <- golem::get_golem_options("parametros") |>
    dplyr::filter(rota == "cofiring")
  tagList(
    fluidRow(
      shinydashboard::tabBox(
        width = 12,
        tabPanel(
          title = "Econômicos",
          fluidRow(
            column(
              width = 6,
              shinyWidgets::autonumericInput(
                inputId = ns("receita"),
                label = "Receita (R$/tonelada)",
                value = parametros$receita,
                min = 0,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                currencySymbol = "R$ ",
                width = "95%"
              ),
            ),
            column(
              width = 6,
              sliderInput(
                inputId = ns("taxa"),
                label = "Taxa de desconto anualizada",
                value = parametros$taxa,
                min = 0,
                max = 1,
                step = 0.01
              )
            )
          )
        ),
        tabPanel(
          title = "Usina",
          fluidRow(
            column(
              width = 6,
              shinyWidgets::autonumericInput(
                inputId = ns("alfa"),
                label = "Capex Fixo",
                value = parametros$alfa_custo_usina,
                min = 0,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                currencySymbol = "R$ ",
                width = "95%"
              ),
              shinyWidgets::autonumericInput(
                inputId = ns("beta"),
                label = "Capex adicional (R$/MW)",
                value = parametros$beta_custo_usina,
                min = 0,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                currencySymbol = "R$ ",
                width = "95%"
              ),
              numericInput(
                inputId = ns("vida_util"),
                label = "Vida útil da usina",
                value = parametros$anos_vida_util_usina,
                min = 1,
                step = 1,
                width = "95%"
              )
            ),
            column(
              width = 6,
              sliderInput(
                inputId = ns("perc_opex"),
                label = "Percentual Opex",
                value = parametros$percentual_opex,
                min = 0,
                max = 1,
                step = 0.01,
                width = "95%"
              ),
              sliderInput(
                inputId = ns("eficiencia"),
                label = "Eficiência da usina",
                value = parametros$efic_usina,
                min = 0,
                max = 1,
                step = 0.01,
                width = "95%"
              ),
              sliderInput(
                inputId = ns("fator_disponibilidade"),
                label = "Fator de disponibilidade da usina",
                value = parametros$fator_disponibilidade,
                min = 0,
                max = 1,
                step = 0.01,
                width = "95%"
              )
            )
          )
        ),
        tabPanel(
          title = "Obtenção do resíduo",
          fluidRow(
            column(
              width = 6,
              numericInput(
                inputId = ns("custo_coleta"),
                label = "Custo de coleta (R$/t)",
                value = parametros$custo_coleta_por_t,
                min = 0,
                width = "95%"
              ),
              numericInput(
                inputId = ns("custo_armazenamento"),
                label = "Custo de armazenamento (R$/t/ano)",
                value = parametros$custo_armazenamento_por_t,
                min = 0,
                width = "95%"
              )
            ),
            column(
              width = 6,
              numericInput(
                inputId = ns("custo_carga_descarga"),
                label = "Custo de carga e descarga do resíduo (R$/t)",
                value = parametros$custo_carga_por_t,
                min = 0,
                width = "95%"
              ),
              numericInput(
                inputId = ns("premio"),
                label = "Prêmio para o produtor (R$/t)",
                value = parametros$premio_produtor,
                width = "95%"
              )
            )
          )
        ),
        tabPanel(
          title = "Transporte",
          fluidRow(
            column(
              width = 6,
              numericInput(
                inputId = ns("custo_transporte_fixo"),
                label = "Custo fixo de transporte (R$)",
                value = parametros$custo_fixo_transporte,
                min = 0,
                width = "95%"
              )
            ),
            column(
              width = 6,
              numericInput(
                inputId = ns("custo_transporte_variavel"),
                label = "Custo de transporte (R$/t/km)",
                value = parametros$custo_transporte_por_t_km,
                min = 0,
                width = "95%"
              )
            )
          )
        ),
        tabPanel(
          title = "Demanda",
          fluidRow(
            column(
              width = 6,
              numericInput(
                inputId = ns("porc_demanda_termeletrica"),
                label = "Demanda termelétrica (%)",
                value = parametros$porc_demanda_termeletrica*100,
                 min = 0,
                max = 1,
                width = "95%"
              ),
              numericInput(
                inputId = ns("porc_demanda_cimenteira"),
                label = "Demanda cimenteira (%)",
                value = parametros$porc_demanda_cimenteira*100,
                min = 0,
                max = 1,
                width = "95%"
              )
            ),
            column(
              width = 6,
              numericInput(
                inputId = ns("porc_demanda_siderurgica"),
                label = "Demanda siderúrgica (%)",
                value = parametros$porc_demanda_siderurgica*100,
                min = 0,
                max = 1,
                width = "95%"
              )
            )
          )
        )
      )
    )
  )
}

#' parametros_cofiring Server Functions
#'
#' @noRd
mod_parametros_cofiring_server <- function(id, parametros) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    parametros_cofiring <- parametros |>
      dplyr::filter(rota == "cofiring")

    parametros_cofiring_atualizados <- reactive({

      parametros_cofiring$receita <- input$receita
      parametros_cofiring$premio_produtor <- input$premio
      parametros_cofiring$custo_armazenamento_por_t <- input$custo_armazenamento
      parametros_cofiring$custo_coleta_por_t <- input$custo_coleta
      parametros_cofiring$custo_carga_por_t <- input$custo_carga_descarga
      parametros_cofiring$custo_fixo_transporte <- input$custo_transporte_fixo
      parametros_cofiring$custo_transporte_por_t_km <- input$custo_transporte_variavel
      parametros_cofiring$alfa_custo_usina <- input$alfa
      parametros_cofiring$beta_custo_usina <- input$beta
      parametros_cofiring$percentual_opex <- input$perc_opex
      parametros_cofiring$anos_vida_util_usina <- input$vida_util
      parametros_cofiring$taxa <- input$taxa
      parametros_cofiring$efic_usina <- input$eficiencia
      parametros_cofiring$fator_disponibilidade <- input$fator_disponibilidade
      parametros_cofiring$porc_demanda_termeletrica <- input$porc_demanda_termeletrica/100
      parametros_cofiring$porc_demanda_cimenteira <- input$porc_demanda_cimenteira/100
      parametros_cofiring$porc_demanda_siderurgica <- input$porc_demanda_siderurgica/100
      
      return(parametros_cofiring)
    })


    return(parametros_cofiring_atualizados)
  })
}

## To be copied in the UI
# mod_parametros_cofiring_ui("parametros_cofiring_1")

## To be copied in the server
# mod_parametros_cofiring_server("parametros_cofiring_1")
