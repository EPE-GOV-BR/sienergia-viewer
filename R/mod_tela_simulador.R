#' tela_simulador UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tela_simulador_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "simulador",
    mod_otimizador_filtros_ui(ns("otimizador_filtros_1")),
    mod_otimizador_resultados_ui(ns("otimizador_resultados_1"))
  )
}

#' tela_simulador Server Functions
#'
#' @noRd
mod_tela_simulador_server <- function(id, con, parametros_padrao, motor_otimizacao, tempo_limite_otimizacao, gap_otimizacao) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    parametros <- mod_otimizador_filtros_server(
      "otimizador_filtros_1",
      con,
      parametros_padrao
    )

    mod_otimizador_resultados_server(
      "otimizador_resultados_1",
      con,
      parametros,
      motor_otimizacao,
      tempo_limite_otimizacao,
      gap_otimizacao
    )

  })
}

## To be copied in the UI
# mod_tela_simulador_ui("tela_simulador_1")

## To be copied in the server
# mod_tela_simulador_server("tela_simulador_1")
