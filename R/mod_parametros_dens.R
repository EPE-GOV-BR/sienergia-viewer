#' parametros_dens UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_parametros_dens_ui <- function(id) {
  ns <- NS(id)
  parametros <- golem::get_golem_options("parametros") |>
    dplyr::filter(rota == "dens")
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
                label = "Receita (R$/MWh)",
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
        )
      )
    )
  )
}

#' parametros_dens Server Functions
#'
#' @noRd
mod_parametros_dens_server <- function(id, parametros) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    parametros_dens <- parametros |>
      dplyr::filter(rota == "dens")

    parametros_dens_atualizados <- reactive({
        parametros_dens$receita <- input$receita
        parametros_dens$premio_produtor <- input$premio
        parametros_dens$custo_armazenamento_por_t <- input$custo_armazenamento
        parametros_dens$custo_coleta_por_t <- input$custo_coleta
        parametros_dens$custo_carga_por_t <- input$custo_carga_descarga
        parametros_dens$custo_fixo_transporte <- input$custo_transporte_fixo
        parametros_dens$custo_transporte_por_t_km <- input$custo_transporte_variavel
        parametros_dens$alfa_custo_usina <- input$alfa
        parametros_dens$beta_custo_usina <- input$beta
        parametros_dens$percentual_opex <- input$perc_opex
        parametros_dens$anos_vida_util_usina <- input$vida_util
        parametros_dens$taxa <- input$taxa
        parametros_dens$efic_usina <- input$eficiencia
        parametros_dens$fator_disponibilidade <- input$fator_disponibilidade

        parametros_dens
    })

    return(parametros_dens_atualizados)
  })
}

## To be copied in the UI
# mod_parametros_dens_ui("parametros_dens_1")

## To be copied in the server
# mod_parametros_dens_server("parametros_dens_1")
