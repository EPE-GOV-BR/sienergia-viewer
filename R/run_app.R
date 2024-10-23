#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @import RSQLite
run_app <- function(motor_otimizacao = "cbc", tempo_limite_otimizacao = 20, gap_otimizacao = 0.01, ...) {
  shinyOptions(
    cache = cachem::cache_disk("./app_cache/cache/")
  )
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    "SIEnergia_dados.sqlite"
  )

  # Deixar os produtos serem buscados aqui pra ir mais rapido?
  
  parametros <- dplyr::tbl(con, "exemplo_parametros") |>
    dplyr::collect()

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(port = 4242, launch.browser = FALSE)
    ),
    golem_opts = list(
      con = con,
      parametros = parametros,
      motor_otimizacao = motor_otimizacao,
      tempo_limite_otimizacao = tempo_limite_otimizacao,
      gap_otimizacao = gap_otimizacao,
      ...)
  )
}
