#' otimizador_resultados UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_otimizador_resultados_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "well resultados",
    fluidRow(
      column(
        width = 4,
        offset = 4,
        class = "text-center",
        br(),
        actionButton(
          ns("otimizar"),
          "Rodar otimizador",
          status = "primary",
          icon = icon("diagram-project"),
          class = "botao-rodar"
        )
      ),
      column(
        offset = 2,
        width = 2,
        shiny::a(
          href = "https://www.pti.org.br/",
          target = "_blank",
          shiny::p("Contribuição Fundação PTI"),
          img(src = "www/pti.png", style = "width: 70%;")
        )
      )
    ), 
    br(),
    div(
      style = "display: none;",
      id = ns("resultados_otimizacao"),
      fluidRow(
        shinydashboard::infoBoxOutput(
          ns("valor_solucao"),
          width = 3
        ),
        shinydashboard::infoBoxOutput(
          ns("num_usinas"),
          width = 3
        ),
        shinydashboard::infoBoxOutput(
          ns("biomassa_aproveitada_agricultura"),
          width = 3
        ),
        shinydashboard::infoBoxOutput(
          ns("biomassa_aproveitada_pecuaria"),
          width = 3
        )
      ),
      hr(),
      navlistPanel(
        widths = c(3, 9),
        header = tagList(
          fluidRow(
            column(
              width = 2,
              actionButton(
                ns("ver_mapas"),
                label = "Ver mapas",
                width = "100px",
                icon = icon("table")
              )
            ),
            column(
              width = 2,
              downloadButton(
                ns("baixar_tabelas"),
                label = "Baixar tabelas",
                width = "100px"
              )
            )
          ),
          br()
        ),
        tabPanel(
          title = "Potência instalada",
          fluidRow(
            column(
              width = 12,
              reactable::reactableOutput(ns("resultados_tabela_potencia"))
            )
          )
        ),
      tabPanel(
          title = "Investimentos",
          fluidRow(
            column(
              width = 12,
              reactable::reactableOutput(ns("resultados_investimentos_por_etapa"))
            )
          )
        ),
        tabPanel(
          title = "Biomassa aproveitada",
          fluidRow(
            column(
              width = 12,
              reactable::reactableOutput(ns("resultados_biomassa_aproveitada"))
            )
          )
        ),
      tabPanel(
          title = "Municípios fornecedores",
          fluidRow(
            column(
              width = 12,
              reactable::reactableOutput(ns("resultados_municipios_fornecedores")),
              br(),
              p("* As distribuidoras de energia elétrica são apresentadas apenas para as rotas que envolvem a geração de energia elétrica.
                A tabela apresenta as distribuidoras que abastecem os municípios sedes correspondentes.")
            )
          )
        ),
        tabPanel(
          title = "Demanda satisfeita (%)",
          reactable::reactableOutput(ns("resultados_demanda_satisfeita_porc"))
        ),
        tabPanel(
          title = "Demanda satisfeita (Energia elétrica)",
          reactable::reactableOutput(ns("resultados_demanda_satisfeita_eletricidade")),
          textOutput(ns("sem_demanda_ee"))
        ),
        tabPanel(
          title = "Demanda satisfeita (Combustível)",
          reactable::reactableOutput(ns("resultados_demanda_satisfeita_combustivel")),
          textOutput(ns("sem_demanda_comb"))
        ),
        tabPanel(
          title = "Demanda satisfeita (Co-firing)",
          reactable::reactableOutput(ns("resultados_demanda_satisfeita_cofiring")),
          textOutput(ns("sem_demanda_cofiring"))
        ),        
        tabPanel(
          title = "Geração de empregos",
          fluidRow(
            column(
              width = 12,
              h2("Rotas Combustível"),
              p("Quantidade de empregos"),
              br(),
              reactable::reactableOutput(ns(
                "resultados_empregos_rotas_combustivel"
              ))
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              h2("Rotas Elétricas"),
              p("Quantidade de empregos"),
              br(),
              reactable::reactableOutput(ns("resultados_empregos_rotas_eletricas"))
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              h2("Rotas Co-firing"),
              p("Quantidade de empregos"),
              br(),
              reactable::reactableOutput(ns("resultados_empregos_rotas_cofiring")),
              textOutput(ns("sem_resultados_empregos_rotas_cofiring"))
            )
          )
        ),
        tabPanel(
          title = "Emissões anuais",
          fluidRow(
            column(
              width = 12,
              h2("Rotas Combustível"),
              p("Emissões anuais em toneladas de CO2 equivalente"),
              br(),
              reactable::reactableOutput(ns(
                "resultados_emissoes_rotas_combustivel"
              ))
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              h2("Rotas Elétricas"),
              p("Emissões anuais em toneladas de CO2 equivalente"),
              br(),
              reactable::reactableOutput(ns("resultados_emissoes_rotas_eletricas"))
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              h2("Rotas Co-firing"),
              p("Emissões anuais em toneladas de CO2 equivalente"),
              br(),
              reactable::reactableOutput(ns("resultados_emissoes_rotas_cofiring")),
              textOutput(ns("sem_resultados_emissoes_rotas_cofiring"))
            )
          )
        ),
        tabPanel(
          title = "Consumo de água anual",
          fluidRow(
            column(
              width = 12,
              h2("Rotas Combustível"),
              p("Consumo de água anual (m³)"),
              br(),
              reactable::reactableOutput(
                ns("resultados_consumo_agua_rotas_combustivel")
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              h2("Rotas Elétricas"),
              p("Consumo de água anual (m³)"),
              br(),
              reactable::reactableOutput(
                ns("resultados_consumo_agua_rotas_eletricas")
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              h2("Rotas Co-firing"),
              p("Consumo de água anual (m³)"),
              br(),
              reactable::reactableOutput(ns("resultados_consumo_agua_rotas_cofiring")),
              textOutput(ns("sem_resultados_consumo_agua_rotas_cofiring"))
            )
          )
        ),
        tabPanel(
          title = "Distância percorrida",
          fluidRow(
            column(
              width = 12,
              reactable::reactableOutput(ns("resultados_total_km_rodados"))
            )
          )
        ),
        tabPanel(
          title = "Combusível consumido",
          fluidRow(
            column(
              width = 12,
              reactable::reactableOutput(ns("resultados_litros_combustivel_consumido"))
            )
          )
        )
      )
    )
    
  )
}

#' otimizador_resultados Server Functions
#'
#' @noRd
mod_otimizador_resultados_server <- function(id, con, parametros, motor_otimizacao, tempo_limite_otimizacao, gap_otimizacao) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pars <- reactive({
      dplyr::bind_rows(
        # Cada rota deve ter uma linha aqui:
        parametros()$dens,
        parametros()$biocomb,
        parametros()$bioeletr,
        parametros()$bioeletr_pec,
        parametros()$biocomb_pec,
        parametros()$cofiring
      )
    })

    observeEvent(input$otimizar, {
      shinyjs::show(id = "resultados_otimizacao")
    })

    solucao <- eventReactive(input$otimizar, {
      validate(
        need(
          !is.null(parametros()$gerais$rotas),
          "Selecione ao menos uma rota."
        )
      )

      validate(
        need(
        !is.null(parametros()$gerais$produtos),
        "Selecione ao menos um produto."
        )
      )
  
      
      
      withProgress(message = "Rodando o modelo", {
        incProgress(0.1)

        inputs <- otimizadorLinear::calcular_inputs(
          con = con,
          parametros = pars(),
          regiao_escopo = parametros()$gerais$regiao_escopo,
          populacao_minima = parametros()$gerais$populacao_minima,
          rotas = parametros()$gerais$rotas,
          produtos_selecionados = parametros()$gerais$produtos,
          backend = c("ram", "disco"),
          sazonalidade = parametros()$gerais$considerar_sazonalidade
        )

        incProgress(0.3)

        if (motor_otimizacao == "cbc") {
          res <- otimizadorLinear::otimizar(
            inputs,
            solver = "cbc",
            num_c = parametros()$gerais$num_c,
            limitar_tamanho_usina = parametros()$gerais$limitar_tamanho_da_usina,
            num_max_usinas_sede = parametros()$gerais$num_max_usinas_por_sede,
            limitar_por_demanda = parametros()$gerais$limitar_por_demanda,
            cbc_args = list(
              sec = tempo_limite_otimizacao,
              ratio = ".01"
            )
          )
        } else if (motor_otimizacao == "gurobi") {
          res <- otimizadorLinear::otimizar(
            inputs,
            solver = "gurobi",
            num_c = parametros()$gerais$num_c,
            limitar_tamanho_usina = parametros()$gerais$limitar_tamanho_da_usina,
            num_max_usinas_sede = parametros()$gerais$num_max_usinas_por_sede,
            limitar_por_demanda = parametros()$gerais$limitar_por_demanda,
            TimeLimit = tempo_limite_otimizacao,
            MIPGap = gap_otimizacao
          )
        } else if (motor_otimizacao == "cplex") {
          res <- otimizadorLinear::otimizar(
            inputs,
            solver = "cplex",
            num_c = parametros()$gerais$num_c,
            limitar_tamanho_usina = parametros()$gerais$limitar_tamanho_da_usina,
            num_max_usinas_sede = parametros()$gerais$num_max_usinas_por_sede,
            limitar_por_demanda = parametros()$gerais$limitar_por_demanda,
            tilim = tempo_limite_otimizacao,
            epgap = gap_otimizacao
          )
        } else if(motor_otimizacao == "glpk") {
          res <- otimizadorLinear::otimizar(
            inputs,
            solver = "glpk",
            num_c = parametros()$gerais$num_c,
            limitar_tamanho_usina = parametros()$gerais$limitar_tamanho_da_usina,
            num_max_usinas_sede = parametros()$gerais$num_max_usinas_por_sede,
            limitar_por_demanda = parametros()$gerais$limitar_por_demanda,
            tm_lim = tempo_limite_otimizacao,
            presolve = TRUE
          )
        } else {
          stop("Motor de otimização não reconhecido.")
        }
      })

      if (sum(res$solucao) == 0) {
        return(NA)
      } else {
        res |>
          otimizadorLinear::arrumar_solucao()
      }
    })


    tabela_resultados_solucao <- reactive({
      req(solucao())


      if (isolate(parametros()$gerais$fator_frete_ida_volta) == TRUE) {
        fatores_frete <- 2
      } else {
        fatores_frete <- 1
      }

      fatores <- isolate(parametros()$gerais$externalidades)
      fatores$fator_ida_volta <- fatores_frete

      resultados <- otimizadorLinear:::preparar_tabelas_resultados(
        regiao_escopo = isolate(parametros()$gerais$regiao_escopo),
        resultado = solucao(),
        con = con,
        parametros = isolate(pars()),
        fatores = fatores
      )

      resultados_final <- resultados |>
        purrr::map_at(
          c(
            "biomassa_aproveitada",
            "demanda_satisfeita",
            "combustivel_aproveitado",
            "tabela_municipios_fornecedores",
            "resultados_potencia",
            "demanda_satisfeita_grupo",
            "energia_ofertada",
            "empregos_emissoes",
            "total_km_rodados",
            "litros_combustivel_consumido",
            "tabela_investimentos"
          ),
          ~ dplyr::left_join(.x, rotas_df(), by = "rota")
        )

      resultados_final
    })


    output$valor_solucao <- shinydashboard::renderInfoBox({
      
      if (!is.list(solucao())) {
        shinyalert::shinyalert(
          title = "Sem solução...",
          text = "Não foi possível encontrar uma solução para os parâmetros selecionados.",
          type = "info"
        )
        return({
        shinydashboard::infoBox(
          title = "Valor da solução",
          subtitle = "",
          value = "Sem solução",
          icon = shiny::icon("dollar-sign")
        )
        })
      } 
    if(is.list(solucao())) {
        valor <- scales::dollar(
          solucao()$valor,
          prefix = "R$ ",
          big.mark = ".",
          decimal.mark = ","
        )

        shinydashboard::infoBox(
          title = "Valor da solução",
          subtitle = "",
          value = valor,
          icon = shiny::icon("dollar-sign")
        )
      }
    })

    output$num_usinas <- shinydashboard::renderInfoBox({
      valor <- tabela_resultados_solucao()$resultados_potencia |>
        dplyr::summarise(num_usinas = sum(num_usinas)) |>
        dplyr::pull(num_usinas) |>
        formatar_numero()

      shinydashboard::infoBox(
        title = "Número de usinas",
        value = valor,
        subtitle = "",
        icon = shiny::icon("industry")
      )
    })

    output$biomassa_aproveitada_agricultura <- shinydashboard::renderInfoBox({
      valor <- tabela_resultados_solucao()$biomassa_aproveitada |>
        dplyr::filter(stringr::str_detect(rota_nome, "agrícolas")) |>
        dplyr::summarise(
          total = sum(unique(total_soma_energetico_disponivel)),
          aproveitado = sum(total_soma_energetico_disponivel * porc_biomassa_aproveitada),
          porc = aproveitado / total
        ) |>
        dplyr::pull(porc) |>
        formatar_porc()
      
      if (is.na(valor)) {
        valor <- "-"
      }
      
      shinydashboard::infoBox(
        title = "Biomassa aproveitada",
        subtitle = "Resíduos agrícolas",
        value = valor,
        icon = shiny::icon("seedling")
      )
    })

    output$biomassa_aproveitada_pecuaria <- shinydashboard::renderInfoBox({
      valor <- tabela_resultados_solucao()$biomassa_aproveitada |>
        dplyr::filter(stringr::str_detect(rota_nome, "pecuários")) |>
        dplyr::summarise(
          total = sum(unique(total_soma_energetico_disponivel)),
          aproveitado = sum(total_soma_energetico_disponivel * porc_biomassa_aproveitada),
          porc = aproveitado / total
        ) |>
        dplyr::pull(porc) |>
        formatar_porc()
      
      if (is.na(valor)) {
        valor <- "-"
      }

      shinydashboard::infoBox(
        title = "Biomassa aproveitada",
        subtitle = "Resíduos pecuários",
        value = valor,
        icon = shiny::icon("cow")
      )
    })

    # tabelas ---------------------------------------------------------------

    # tabela auxiliar
    municipios <- dplyr::tbl(con, "municipios") |>
          dplyr::select(muni_nome, muni_cod) |>
          dplyr::mutate(muni_cod = as.character(muni_cod)) |> 
          dplyr::distinct(muni_cod, .keep_all = TRUE) |> 
          dplyr::collect()

    
    
    tab_potencia <- reactive({
      req(solucao())

      tab <- tabela_resultados_solucao()$resultados_potencia |> 
        dplyr::transmute(
          "Rota" = rota_nome,
          "Município sede" = muni_sede_nome,
          "UF da sede" = uf_sede,
          "Número de municípios" = numero_municipios,
          "Número de usinas" = num_usinas,
          potencia,
          comb_gerado_m3,
          comb_gerado_ton
        )

      if (length(unique(tab$`UF da sede`)) == 1) {
        tab <- tab |>
          dplyr::select(-`UF da sede`)
      }

      tab
    })
    
    
    resultados_investimentos_por_etapa <- reactive({
      req(solucao())

      tabela_resultados_solucao()$tabela_investimentos |>
        dplyr::transmute(
          "Rota" = rota_nome,
          total_inv_receita,
          total_inv_capex,
          total_inv_opex,
          total_inv_aquisicao,
          total_inv_transporte, 
          total_inv_sazonalidade
        ) |> 
        janitor::adorn_totals("col", name = "vpl_rota_economica") |> 
        janitor::adorn_totals("row") 
    })
    
    

    resultados_biomassa_aproveitada <- reactive({
      req(solucao())
      tabela_resultados_solucao()$biomassa_aproveitada |>
        dplyr::transmute(
          "Rota" = rota_nome,
          porc_biomassa_aproveitada
        )
    })
    
    

    resultados_demanda_satisfeita_porc <- reactive({
      req(solucao())
      tabela_resultados_solucao()$demanda_satisfeita |>
        dplyr::transmute(
          "Rota" = rota_nome,
          porc_demanda_satisfeita
        )
    })

    resultados_demanda_satisfeita_eletricidade <- reactive({

      tab <- tabela_resultados_solucao()$demanda_satisfeita_grupo |>
        dplyr::filter(rota %in% c("dens", "bioeletr", "bioeletr_pec"))

      if (nrow(tab) > 0) {
        tab |>
          dplyr::transmute(
            Rota = rota_nome,
            Distribuidora = grupo_demanda,
            demanda_existente,
            demanda_satisfeita,
            porc_demanda_satisfeita
          )
      } else {
        tibble::tibble()
      }
    })

    resultados_demanda_satisfeita_combustivel <- reactive({
      tab <- tabela_resultados_solucao()$demanda_satisfeita_grupo |>
        dplyr::filter(rota %in% c("biocomb", "biocomb_pec"))

      if (nrow(tab) > 0) {

        tab |>
          dplyr::left_join(municipios, by = c("grupo_demanda" = "muni_cod")) |>
          dplyr::transmute(
            Rota = rota_nome,
            "Município" = muni_nome,
            demanda_existente,
            demanda_satisfeita,
            porc_demanda_satisfeita
          )
      } else {
        tibble::tibble()
      }
    })
    
    resultados_demanda_satisfeita_cofiring <- reactive({
         
      tab <- tabela_resultados_solucao()$demanda_satisfeita_grupo |>
        dplyr::filter(rota %in% c("cofiring"))

      if (nrow(tab) > 0) {

        tab |>
          dplyr::left_join(municipios, by = c("grupo_demanda" = "muni_cod")) |>
          dplyr::transmute(
            Rota = rota_nome,
            "Município" = muni_nome,
            demanda_existente,
            demanda_satisfeita,
            porc_demanda_satisfeita
          )
      } else {
        tibble::tibble()
      }
    })

    resultados_municipios_fornecedores <- reactive({
      req(solucao())

      tab <- tabela_resultados_solucao()$tabela_municipios_fornecedores |>
        dplyr::transmute(
          "Rota" = rota_nome,
          "Município fornecedor" = muni_origem_nome,
          "UF do fornecedor" = uf_origem,
          "Município sede" = muni_sede_nome,
          distribuidora_energia_eletrica,
          biomassa_disponivel,
          biomassa_aproveitada,
          porc_energetico_aproveitado,
          combustivel_comercializado
        )

      if (length(unique(tab$`UF do fornecedor`)) == 1) {
        tab <- tab |>
          dplyr::select(-`UF do fornecedor`)
      }

      tab
    })

    resultados_agua_emissoes_empregos_rotas_combustivel <- reactive({
      req(solucao())

      if (!is.null(tabela_resultados_solucao()$agua_emissoes_empregos$rotas_combustivel)) {

        tabela_resultados_solucao()$agua_emissoes_empregos$rotas_combustivel |>
          dplyr::left_join(rotas_df(), by = "rota") |>
          dplyr::relocate(rota_nome, .before = tidyselect::everything()) |>
          dplyr::select(-rota)
      } else {
        tibble::tibble()
      }
    })

    resultados_agua_emissoes_empregos_rotas_eletricas <- reactive({
      req(solucao())

      if (!is.null(tabela_resultados_solucao()$agua_emissoes_empregos$rotas_eletricas)) {
        tabela_resultados_solucao()$agua_emissoes_empregos$rotas_eletricas |>
          dplyr::left_join(rotas_df(), by = "rota") |>
          dplyr::relocate(rota_nome, .before = tidyselect::everything()) |>
          dplyr::select(-rota)
      } else {
        tibble::tibble()
      }
    })

    resultados_agua_emissoes_empregos_rotas_cofiring <- reactive({
      req(solucao())

      if (!is.null(tabela_resultados_solucao()$agua_emissoes_empregos$rotas_cofiring)) {
        tabela_resultados_solucao()$agua_emissoes_empregos$rotas_cofiring |>
          dplyr::left_join(rotas_df(), by = "rota") |>
          dplyr::relocate(rota_nome, .before = tidyselect::everything()) |>
          dplyr::select(-rota)
      } else {
        tibble::tibble()
      }
    })

    resultados_total_km_rodados <- reactive({
      req(solucao())

      tabela_resultados_solucao()$total_km_rodados |>
        dplyr::mutate(rota_nome = dplyr::if_else(rota == "total", "Total", rota_nome)) |>
        dplyr::transmute(
          "Rota" = rota_nome,
          total_energetico,
          total_km_rodados
        )
    })

    resultados_litros_combustivel_consumido <- reactive({
      req(solucao())

      tabela_resultados_solucao()$litros_combustivel_consumido |>
        dplyr::mutate(rota_nome = dplyr::if_else(rota == "total", "Total", rota_nome)) |>
        dplyr::transmute(
          "Rota" = rota_nome,
          total_litros_diesel_consumidos = total_litros_diesel_consumidos
        )
    })

    # tabelas reactable ---------------

    output$resultados_tabela_potencia <- reactable::renderReactable({
      tab_potencia() |>
        tabela_padrao(
          columns = list(
            potencia = reactable::colDef(
              name = "Potência instalada (MW)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            comb_gerado_m3 = reactable::colDef(
              name = "Combustível gerado (m³)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            comb_gerado_ton = reactable::colDef(
              name = "Combustível gerado (ton)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            Rota = reactable::colDef(
              minWidth = 200
            )
          )
        )
    })
    
  output$resultados_investimentos_por_etapa <- reactable::renderReactable({
      resultados_investimentos_por_etapa() |>
        tabela_padrao(
          columns = list(
            Rota = reactable::colDef(
              minWidth = 200
            ),
            total_inv_receita = reactable::colDef(
              name = "Valor presente da receita (R$)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            total_inv_capex = reactable::colDef(
              name = "CAPEX (R$)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            total_inv_opex = reactable::colDef(
              name = "Valor presente do OPEX (R$)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            total_inv_aquisicao = reactable::colDef(
              name = "Valor presente dos custos de aquisição (R$)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            total_inv_transporte = reactable::colDef(
              name = "Valor presente dos custos de transporte (R$)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            total_inv_sazonalidade = reactable::colDef(
              name = "Valor presente dos custos de estoque adicional por sazonalidade (R$)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            vpl_rota_economica = reactable::colDef(
              name = "Valor presente líquido da rota econômica (R$)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
          )
        )
    })
    

    output$resultados_biomassa_aproveitada <- reactable::renderReactable({
      resultados_biomassa_aproveitada() |>
        tabela_padrao(
          columns = list(
            porc_biomassa_aproveitada = reactable::colDef(
              name = "Percentual de biomassa aproveitada",
              format = reactable::colFormat(separators = TRUE, digits = 1, percent = TRUE)
            )
          )
        )
    })

    output$resultados_demanda_satisfeita_porc <- reactable::renderReactable({
      resultados_demanda_satisfeita_porc() |>
        tabela_padrao(
          columns = list(
            porc_demanda_satisfeita = reactable::colDef(
              name = "Percentual de demanda satisfeita",
              format = reactable::colFormat(separators = TRUE, digits = 1, percent = TRUE)
            )
          )
        )
    })

    output$resultados_demanda_satisfeita_eletricidade <- reactable::renderReactable({
      req(nrow(resultados_demanda_satisfeita_eletricidade()) > 0)
      resultados_demanda_satisfeita_eletricidade() |>
        tabela_padrao(
          columns = list(
            porc_demanda_satisfeita = reactable::colDef(
              name = "Percentual de demanda satisfeita",
              format = reactable::colFormat(separators = TRUE, digits = 1, percent = TRUE)
            ),
            demanda_existente = reactable::colDef(
              name = "Demanda anual existente (MJ)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            demanda_satisfeita = reactable::colDef(
              name = "Demanda anual satisfeita (MJ)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
          )
        )
    })



    output$resultados_demanda_satisfeita_combustivel <- reactable::renderReactable({
      req(nrow(resultados_demanda_satisfeita_combustivel()) > 0)
      resultados_demanda_satisfeita_combustivel() |>
        tabela_padrao(
          columns = list(
            porc_demanda_satisfeita = reactable::colDef(
              name = "Percentual de demanda satisfeita",
              format = reactable::colFormat(separators = TRUE, digits = 1, percent = TRUE)
            ),
            demanda_existente = reactable::colDef(
              name = "Demanda anual existente (MJ)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            demanda_satisfeita = reactable::colDef(
              name = "Demanda anual satisfeita (MJ)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
          )
        )
    })

     output$resultados_demanda_satisfeita_cofiring <- reactable::renderReactable({
      req(nrow(resultados_demanda_satisfeita_cofiring()) > 0)
      resultados_demanda_satisfeita_cofiring() |>
        tabela_padrao(
          columns = list(
            porc_demanda_satisfeita = reactable::colDef(
              name = "Percentual de demanda satisfeita",
              format = reactable::colFormat(separators = TRUE, digits = 1, percent = TRUE)
            ),
            demanda_existente = reactable::colDef(
              name = "Demanda anual existente (ton)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            demanda_satisfeita = reactable::colDef(
              name = "Demanda anual satisfeita (ton)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
          )
        )
    })


    output$resultados_municipios_fornecedores <- reactable::renderReactable({
      req(nrow(resultados_municipios_fornecedores()) > 0)
      resultados_municipios_fornecedores() |>
        tabela_padrao(
          columns = list(
            Rota = reactable::colDef(
              minWidth = 200
            ),
            distribuidora_energia_eletrica = reactable::colDef(
              name = "Distribuidora de energia elétrica*",
              minWidth = 200
            ),
            biomassa_disponivel = reactable::colDef(
              name = "Biomassa anual disponível",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            biomassa_aproveitada = reactable::colDef(
              name = "Biomassa anual aproveitada",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            porc_energetico_aproveitado = reactable::colDef(
              name = "Percentual do energético aproveitado",
              format = reactable::colFormat(separators = TRUE, digits = 1, percent = TRUE)
            ),
            combustivel_comercializado = reactable::colDef(
              name = "Combustível anual comercializado (m^3)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
          )
        )
    })

    output$resultados_empregos_rotas_combustivel <- reactable::renderReactable({
      req(nrow(resultados_agua_emissoes_empregos_rotas_combustivel()) > 0)
      resultados_agua_emissoes_empregos_rotas_combustivel() |>
        dplyr::select(
          rota_nome,
          producao,
          num_empregos_rota,
          nivel_remuneracao_mensal
        ) |>
        tabela_padrao(
          columns = list(
            rota_nome = reactable::colDef(
              name = "Rota",
              minWidth = 200
            ),
            producao = reactable::colDef(
              name = "Produção anual (m³)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            num_empregos_rota = reactable::colDef(
              name = "Número de empregos por ano",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            nivel_remuneracao_mensal =  reactable::colDef(
              name = "Nível de remuneração (mensal)",
              format = reactable::colFormat(separators = TRUE, digits = 0), 
              na = "0"
            )
          )
        )
    })

    output$resultados_empregos_rotas_eletricas <- reactable::renderReactable({
      req(nrow(resultados_agua_emissoes_empregos_rotas_eletricas()) > 0)

      resultados_agua_emissoes_empregos_rotas_eletricas() |>
        dplyr::select(
          rota_nome,
          producao,
          num_empregos_rota,
          nivel_remuneracao_mensal
        ) |>
        tabela_padrao(
          columns = list(
            rota_nome = reactable::colDef(
              name = "Rota",
              minWidth = 200
            ),
            producao = reactable::colDef(
              name = "Produção anual (MWh)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            num_empregos_rota = reactable::colDef(
              name = "Número de empregos por ano",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            nivel_remuneracao_mensal =  reactable::colDef(
              name = "Nível de remuneração (mensal)",
              format = reactable::colFormat(separators = TRUE, digits = 0), 
              na = "0"
            )
          )
        )
    })

    output$resultados_empregos_rotas_cofiring <- reactable::renderReactable({
      req(nrow(resultados_agua_emissoes_empregos_rotas_cofiring()) > 0)
      resultados_agua_emissoes_empregos_rotas_cofiring() |>
        dplyr::select(
          rota_nome,
          producao,
          num_empregos_rota,
          nivel_remuneracao_mensal
        ) |>
        tabela_padrao(
          columns = list(
            rota_nome = reactable::colDef(
              name = "Rota",
              minWidth = 200
            ),
            producao = reactable::colDef(
              name = "Produção anual (ton)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            num_empregos_rota = reactable::colDef(
              name = "Número de empregos por ano",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            nivel_remuneracao_mensal =  reactable::colDef(
              name = "Nível de remuneração (mensal)",
              format = reactable::colFormat(separators = TRUE, digits = 0), 
              na = "0"
            )
          )
        )
    })



    output$resultados_emissoes_rotas_combustivel <- reactable::renderReactable({
      req(nrow(resultados_agua_emissoes_empregos_rotas_combustivel()) > 0)
      resultados_agua_emissoes_empregos_rotas_combustivel() |>
        dplyr::select(
          rota_nome,
          producao,
          toneladas_co2_eq_rota
        ) |>
        tabela_padrao(
          columns = list(
            rota_nome = reactable::colDef(
              name = "Rota",
              minWidth = 200
            ),
            producao = reactable::colDef(
              name = "Produção anual (m³)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            toneladas_co2_eq_rota = reactable::colDef(
              name = "Emissões anuais (Toneladas de CO2 eq.)",
              format = reactable::colFormat(separators = TRUE, digits = 1)
            )
          )
        )
    })

    output$resultados_emissoes_rotas_eletricas <- reactable::renderReactable({
      req(nrow(resultados_agua_emissoes_empregos_rotas_eletricas()) > 0)

      resultados_agua_emissoes_empregos_rotas_eletricas() |>
        dplyr::select(
          rota_nome,
          producao,
          toneladas_co2_eq_rota
        ) |>
        tabela_padrao(
          columns = list(
            rota_nome = reactable::colDef(
              name = "Rota",
              minWidth = 200
            ),
            producao = reactable::colDef(
              name = "Produção anual (MWh)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            toneladas_co2_eq_rota = reactable::colDef(
              name = "Emissões anuais (Toneladas de CO2 eq.)",
              format = reactable::colFormat(separators = TRUE, digits = 1)
            )
          )
        )
    })

    output$resultados_emissoes_rotas_cofiring <- reactable::renderReactable({
      req(nrow(resultados_agua_emissoes_empregos_rotas_cofiring()) > 0)

      resultados_agua_emissoes_empregos_rotas_cofiring() |>
        dplyr::select(
          rota_nome,
          producao,
          toneladas_co2_eq_rota
        ) |>
        tabela_padrao(
          columns = list(
            rota_nome = reactable::colDef(
              name = "Rota",
              minWidth = 200
            ),
            producao = reactable::colDef(
              name = "Produção anual (ton)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            toneladas_co2_eq_rota = reactable::colDef(
              name = "Emissões anuais (Toneladas de CO2 eq.)",
              format = reactable::colFormat(separators = TRUE, digits = 1)
            )
          )
        )
    })

    output$resultados_consumo_agua_rotas_combustivel <- reactable::renderReactable({
      req(nrow(resultados_agua_emissoes_empregos_rotas_combustivel()) > 0)

      resultados_agua_emissoes_empregos_rotas_combustivel() |>
        dplyr::select(
          rota_nome,
          producao,
          consumo_agua_m3_rota
        ) |>
        tabela_padrao(
          columns = list(
            rota_nome = reactable::colDef(
              name = "Rota",
              minWidth = 200
            ),
            producao = reactable::colDef(
              name = "Produção anual (m³)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            consumo_agua_m3_rota = reactable::colDef(
              name = "Consumo de água anual (m³)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
          )
        )
    })


    output$resultados_consumo_agua_rotas_eletricas <- reactable::renderReactable({
      req(nrow(resultados_agua_emissoes_empregos_rotas_eletricas()) > 0)
      resultados_agua_emissoes_empregos_rotas_eletricas() |>
        dplyr::select(
          rota_nome,
          producao,
          consumo_agua_m3_rota
        ) |>
        tabela_padrao(
          columns = list(
            rota_nome = reactable::colDef(
              name = "Rota",
              minWidth = 200
            ),
            producao = reactable::colDef(
              name = "Produção anual (MWh)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            consumo_agua_m3_rota = reactable::colDef(
              name = "Consumo de água anual (m³)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
          )
        )
    })


    output$resultados_consumo_agua_rotas_cofiring <- reactable::renderReactable({
      req(nrow(resultados_agua_emissoes_empregos_rotas_cofiring()) > 0)
      resultados_agua_emissoes_empregos_rotas_cofiring() |>
        dplyr::select(
          rota_nome,
          producao,
          consumo_agua_m3_rota
        ) |>
        tabela_padrao(
          columns = list(
            rota_nome = reactable::colDef(
              name = "Rota",
              minWidth = 200
            ),
            producao = reactable::colDef(
              name = "Produção anual (ton)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            consumo_agua_m3_rota = reactable::colDef(
              name = "Consumo de água anual (m³)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
          )
        )
    })

    output$resultados_total_km_rodados <- reactable::renderReactable({
      req(nrow(resultados_total_km_rodados()) > 0)
      resultados_total_km_rodados() |>
        tabela_padrao(
          columns = list(
            Rota = reactable::colDef(
              minWidth = 200
            ),
            total_km_rodados = reactable::colDef(
              name = "Distância anual percorrida (km)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            ),
            total_energetico = reactable::colDef(
              name = "Energético anual (ton)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
          )
        )
    })

    output$resultados_litros_combustivel_consumido <- reactable::renderReactable({
      req(nrow(resultados_litros_combustivel_consumido()) > 0)
      resultados_litros_combustivel_consumido() |>
        tabela_padrao(
          columns = list(
            Rota = reactable::colDef(
              minWidth = 200
            ),
            total_litros_diesel_consumidos = reactable::colDef(
              name = "Diesel consumido anual (L)",
              format = reactable::colFormat(separators = TRUE, digits = 0)
            )
          )
        )
    })

    # textos quando não há resultados ---------------

    output$sem_demanda_ee <- renderText({
      tab <- tabela_resultados_solucao()$demanda_satisfeita_grupo |>
        dplyr::filter(rota %in% c("dens", "bioeletr", "bioeletr_pec"))

      req(nrow(tab) == 0)

      "Não há geração de energia elétrica nesta solução."
    })

    output$sem_demanda_comb <- renderText({
      tab <- tabela_resultados_solucao()$demanda_satisfeita_grupo |>
        dplyr::filter(rota %in% c("biocomb", "biocomb_pec"))

      req(nrow(tab) == 0)

      "Não há geração de biocombustível nesta solução."
    })

    output$sem_demanda_cofiring <- renderText({
      tab <- tabela_resultados_solucao()$demanda_satisfeita_grupo |>
        dplyr::filter(rota %in% c("cofiring"))

      req(nrow(tab) == 0)

      "Não há geração nesta solução."
    })

    output$sem_resultados_consumo_agua_rotas_cofiring <-
      renderText({
        req(nrow(resultados_agua_emissoes_empregos_rotas_cofiring()) == 0)

        "Não há geração nesta solução."
      })

    output$sem_resultados_emissoes_rotas_cofiring <-
      renderText({
        req(nrow(resultados_agua_emissoes_empregos_rotas_cofiring()) == 0)

        "Não há geração nesta solução."
      })

    # download das tabelas ----------

    output$baixar_tabelas <- shiny::downloadHandler(
      filename = function() {
        paste0("tabelas_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        list(
          tab_potencia = tab_potencia(),
          investimentos_etapa = resultados_investimentos_por_etapa(),
          biomassa_aproveitada = resultados_biomassa_aproveitada(),
          demanda_satisfeita_porc = resultados_demanda_satisfeita_porc(),
          demanda_satisfeita_eletricidade = resultados_demanda_satisfeita_eletricidade(),
          demanda_satisfeita_combustivel = resultados_demanda_satisfeita_combustivel(),
          municipios_fornecedores = resultados_municipios_fornecedores(),
          externalidades_rotas_comb = resultados_agua_emissoes_empregos_rotas_combustivel(),
          externalidades_rotas_eletricas = resultados_agua_emissoes_empregos_rotas_eletricas(),
          externalidades_rotas_cofiring = resultados_agua_emissoes_empregos_rotas_cofiring(),
          total_km_rodados = resultados_total_km_rodados(),
          combustivel_consumido_litros = resultados_litros_combustivel_consumido()
        ) |>
          writexl::write_xlsx(path = file)
      }
    )



    # mapas -----------------------------------------------------------------

    observeEvent(input$ver_mapas, {
      par <- isolate(parametros())
      regiao_escopo <- par$gerais$regiao_escopo

      if (length(regiao_escopo) > 1) {
      regiao_escopo <- names(otimizadorLinear::dados_geo)
      } 

      tab_geo <- regiao_escopo |>
        purrr::map(
          ~ sf::st_as_sf(otimizadorLinear::dados_geo[[.x]])
        ) |>
        dplyr::bind_rows()



      # Atenção: aqui devemos nos atentar a criar o conteúdo por rotas.

      sedes_dens <- solucao()$tab_solucao$dens |>
        pegar_sedes(tab_geo)

      sedes_biocomb <- solucao()$tab_solucao$biocomb |>
        pegar_sedes(tab_geo)

      sedes_bioeletr <- solucao()$tab_solucao$bioeletr |>
        pegar_sedes(tab_geo)
      
      sedes_bioeletr_pec <- solucao()$tab_solucao$bioeletr_pec |>
        pegar_sedes(tab_geo)
      
      sedes_biocomb_pec <- solucao()$tab_solucao$biocomb_pec |>
        pegar_sedes(tab_geo)

      sedes_cofiring <- solucao()$tab_solucao$cofiring |>
        pegar_sedes(tab_geo)

      showModal(
        modalDialog(
          easyClose = TRUE,
          footer = modalButton("Fechar"),
          size = "l",
          title = "Mapas",
          navlistPanel(
            widths = c(3, 9),
            if ("dens" %in% par$gerais$rotas) {
              tabPanel(
                title = "Densificação - Energia elétrica - Resíduos agrícolas",
                shinyWidgets::pickerInput(
                  ns("regioes_dens"),
                  paste0("Municípios sede - ", dplyr::pull(dplyr::filter(rotas_df(), rota == "dens"), rota_nome)),
                  choices = sedes_dens,
                  selected = sedes_dens,
                  multiple = TRUE,
                  options = pickerInput_opcoes()
                ),
                br(),
                shiny::plotOutput(ns("mapa_dens")) |>
                  shinycssloaders::withSpinner()
              )
            },
            if ("biocomb" %in% par$gerais$rotas) {
              tabPanel(
                title = "Biodigestão - Combustível - Resíduos agrícolas",
                shinyWidgets::pickerInput(
                  ns("regioes_biocomb"),
                  paste0("Municípios sede - ", dplyr::pull(dplyr::filter(rotas_df(), rota == "biocomb"), rota_nome)),
                  choices = sedes_biocomb,
                  selected = sedes_biocomb,
                  multiple = TRUE,
                  options = pickerInput_opcoes()
                ),
                br(),
                shiny::plotOutput(ns("mapa_biocomb")) |>
                  shinycssloaders::withSpinner()
              )
            },
            if ("biocomb_pec" %in% par$gerais$rotas) {
              tabPanel(
                title = "Biodigestão - Combustível - Resíduos pecuários",
                shinyWidgets::pickerInput(
                  ns("regioes_biocomb_pec"),
                  paste0("Municípios sede - ", dplyr::pull(dplyr::filter(rotas_df(), rota == "biocomb_pec"), rota_nome)),
                  choices = sedes_biocomb_pec,
                  selected = sedes_biocomb_pec,
                  multiple = TRUE,
                  options = pickerInput_opcoes()
                ),
                br(),
                shiny::plotOutput(ns("mapa_biocomb_pec")) |>
                  shinycssloaders::withSpinner()
              )
            },
            if ("bioeletr" %in% par$gerais$rotas) {
              tabPanel(
                title = "Biodigestão - Energia elétrica - Resíduos agrícolas",
                shinyWidgets::pickerInput(
                  ns("regioes_bioeletr"),
                  paste0("Municípios sede - ", dplyr::pull(dplyr::filter(rotas_df(), rota == "bioeletr"), rota_nome)),
                  choices = sedes_bioeletr,
                  selected = sedes_bioeletr,
                  multiple = TRUE,
                  options = pickerInput_opcoes()
                ),
                br(),
                shiny::plotOutput(ns("mapa_bioeletr")) |>
                  shinycssloaders::withSpinner()
              )
            },
           if ("bioeletr_pec" %in% par$gerais$rotas) {
              tabPanel(
                title = "Biodigestão - Energia elétrica - Resíduos pecuários",
                shinyWidgets::pickerInput(
                  ns("regioes_bioeletr_pec"),
                  paste0("Municípios sede - ", dplyr::pull(dplyr::filter(rotas_df(), rota == "bioeletr_pec"), rota_nome)),
                  choices = sedes_bioeletr_pec,
                  selected = sedes_bioeletr_pec,
                  multiple = TRUE,
                  options = pickerInput_opcoes()
                ),
                br(),
                shiny::plotOutput(ns("mapa_bioeletr_pec")) |>
                  shinycssloaders::withSpinner()
              )
            },
            if ("cofiring" %in% par$gerais$rotas) {
              tabPanel(
                title = "Co-firing  - Resíduos agrícolas",
                shinyWidgets::pickerInput(
                  ns("regioes_cofiring"),
                  paste0("Municípios sede - ", dplyr::pull(dplyr::filter(rotas_df(), rota == "cofiring"), rota_nome)),
                  choices = sedes_cofiring,
                  selected = sedes_cofiring,
                  multiple = TRUE,
                  options = pickerInput_opcoes()
                ),
                br(),
                shiny::plotOutput(ns("mapa_cofiring")) |>
                  shinycssloaders::withSpinner()
              )
            }
          )
        )
      )
    })

    output$mapa_dens <- shiny::renderPlot({
      req(solucao())
      regiao_escopo <- isolate(parametros())$gerais$regiao_escopo
      otimizadorLinear::gg_regioes(
        solucao()$tab_solucao$dens,
        regiao_escopo = regiao_escopo,
        sedes = input$regioes_dens,
        title = "Municípios fornecedores - Densificação - Energia elétrica - Resíduos agrícolas"
      )
    })

    output$mapa_biocomb <- shiny::renderPlot({
      req(solucao())
      regiao_escopo <- isolate(parametros())$gerais$regiao_escopo
      otimizadorLinear::gg_regioes(
        solucao()$tab_solucao$biocomb,
        regiao_escopo = regiao_escopo,
        sedes = input$regioes_biocomb,
        title = "Municípios fornecedores - Biodigestão - Combustível - Resíduos agrícolas"
      )
    })
    
    output$mapa_biocomb_pec <- shiny::renderPlot({
      req(solucao())
      regiao_escopo <- isolate(parametros())$gerais$regiao_escopo
      otimizadorLinear::gg_regioes(
        solucao()$tab_solucao$biocomb_pec,
        regiao_escopo = regiao_escopo,
        sedes = input$regioes_biocomb_pec,
        title = "Municípios fornecedores - Biodigestão - Combustível - Resíduos pecuários"
      )
    })

    output$mapa_bioeletr <- shiny::renderPlot({
      req(solucao())
      regiao_escopo <- isolate(parametros())$gerais$regiao_escopo
      otimizadorLinear::gg_regioes(
        solucao()$tab_solucao$bioeletr,
        regiao_escopo = regiao_escopo,
        sedes = input$regioes_bioeletr,
        title = "Municípios fornecedores - Biodigestão - Energia elétrica - Resíduos agrícolas"
      )
    })
    
    output$mapa_bioeletr_pec <- shiny::renderPlot({
      req(solucao())
      regiao_escopo <- isolate(parametros())$gerais$regiao_escopo
      otimizadorLinear::gg_regioes(
        solucao()$tab_solucao$bioeletr_pec,
        regiao_escopo = regiao_escopo,
        sedes = input$regioes_bioeletr_pec,
        title = "Municípios fornecedores - Biodigestão - Energia elétrica - Resíduos pecuários"
      )
    })

    output$mapa_cofiring <- shiny::renderPlot({
      req(solucao())
      regiao_escopo <- isolate(parametros())$gerais$regiao_escopo
      otimizadorLinear::gg_regioes(
        solucao()$tab_solucao$cofiring,
        regiao_escopo = regiao_escopo,
        sedes = input$regioes_cofiring,
        title = "Municípios fornecedores - Co-firing - Resíduos agrícolas"
      )
    })
  })
}

pegar_sedes <- function(tab, tab_geo) {
  if (!is.null(tab)) {
    tab |>
      dplyr::distinct(sede_cod) |>
      dplyr::left_join(
        tab_geo |>
          sf::st_drop_geometry() |>
          dplyr::select(name_muni, code_muni),
        by = c("sede_cod" = "code_muni")
      ) |>
      dplyr::select(name_muni, sede_cod) |>
      tibble::deframe()
  } else {
    structure(numeric(0), names = character(0))
  }
}

## To be copied in the UI
# mod_otimizador_resultados_ui("otimizador_resultados_1")

## To be copied in the server
# mod_otimizador_resultados_server("otimizador_resultados_1")
