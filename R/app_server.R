#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DBI
#' @import RSQLite
#'
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here
  database <- reactiveValues(
    con = connect_database()
  )

  con <- golem::get_golem_options("con")
  parametros_padrao <- golem::get_golem_options("parametros")
  motor_otimizacao <- golem::get_golem_options("motor_otimizacao")
  tempo_limite_otimizacao <- golem::get_golem_options("tempo_limite_otimizacao")
  gap_otimizacao <- golem::get_golem_options("gap_otimizacao")

  cancel.onSessionEnded <- session$onSessionEnded(function() {
    reactive({
      DBI::dbDisconnect(database$con)
    })
  })

  observe({
    selected_menu <- input$sidebar_menu

    atualiza_titulo_pagina(selected_menu)
  })

  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")

  callModule(module = mod_tela_modelo_server, id = "modelo", database = database)
  callModule(module = mod_tela_produto_server, id = "producao", database = database)
  callModule(module = mod_tela_energetico_server, id = "energetico", database = database)
  callModule(module = mod_tela_combustivel_server, id = "combustivel", database = database)
  callModule(module = mod_tela_energia_server, id = "energia", database = database)
  mod_tela_simulador_server(
    "tela_simulador_1",
    con,
    parametros_padrao,
    motor_otimizacao,
    tempo_limite_otimizacao,
    gap_otimizacao
  )
}
