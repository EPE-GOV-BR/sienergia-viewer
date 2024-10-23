#' tela_produto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#'
#'
#'
#' @importFrom shiny NS tagList
mod_tela_modelo_ui <- function(id, titulo){

  ## Filtros ----
  faixa_titulos <- cria_filtros_com_rota(id)


  ## Well panel ----
  well_panel_conteudo <- cria_well_panel_modelo(id)


  ## Formato final ----
  ns <- NS(id)
  return(
    tagList(
      tags$div(faixa_titulos,class = "faixa_titulo"),
      tags$div(well_panel_conteudo, class = "conteudo-util")
    )
  )

}

#' tela_produto Server Function
#'
#' @noRd
mod_tela_modelo_server <- function(input, output, session, database){
  ## Configura as opções de todos os filtros ----
  observe({
    atualiza_filtro_data(database, "fato_modelo")
  })
  observe({
    atualiza_filtro_n1_secao(database, session, input, "fato_modelo")
  })
  observe({
    atualiza_filtro_n2_divisao(database, session, input, "fato_modelo")
  })
  observe({
    atualiza_filtro_n3_grupo(database, session, input, "fato_modelo")
  })
  observe({
    atualiza_filtro_n5_subclasse(database, session, input, "fato_modelo")
  })
  observe({
    atualiza_filtro_municipio(database, session, input)
  })

  ## Filtragem da produto_flat ----
  fato_produto_flat_filtrada <- reactive({

    inicio_periodo = input$filtro_periodo[1] * 10000
    fim_periodo = (input$filtro_periodo[2] + 1) * 10000

    cd_municipio_local <- municipio_selecionado()

    cd_subclasse_local <- subclasse_selecionado()

    dados_filtrados <- tbl(database$con, "fato_produto_mu") %>%
      filter(
        cd_data >= inicio_periodo,
        cd_data < fim_periodo,
        cd_municipio %in% cd_municipio_local,
        cd_subclasse %in% cd_subclasse_local
      ) %>%
      collect() %>%
      left_join(
        collect(tbl(database$con, "dim_produto") %>% select(cd_produto, produto, produto_2)),
        by = "cd_produto"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_subclasse") %>% select(cd_subclasse, n1_secao, n2_divisao, n3_grupo, n5_subclasse)),
        by = "cd_subclasse"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_municipio") %>% select(cd_municipio, cd_microrregiao, regiao, sigla_uf, municipio)),
        by = c("cd_municipio")
      ) %>%
      mutate(
        across(
          .cols = c(qtd_produzida_t, efetivo_rebanho, area_utilizada),
          .fns = ~(.x/1000)
        )
      )

    dados_filtrados

  })

  ## Filtragem da energetico_flat ----
  fato_energetico_flat_filtrada <- reactive({

    inicio_periodo = input$filtro_periodo[1] * 10000
    fim_periodo = (input$filtro_periodo[2] + 1) * 10000

    cd_municipio_local <- municipio_selecionado()

    cd_subclasse_local <- subclasse_selecionado()

    dados_filtrados <- tbl(database$con, "fato_energetico") %>%
      filter(
        cd_data >= inicio_periodo,
        cd_data < fim_periodo,
        cd_municipio %in% cd_municipio_local,
        cd_subclasse %in% cd_subclasse_local
      ) %>%
      collect() %>%
      left_join(
        collect(tbl(database$con, "dim_subclasse") %>% select(cd_subclasse, n1_secao, n2_divisao, n3_grupo, n5_subclasse)),
        by = "cd_subclasse"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_municipio") %>% select(cd_municipio, cd_microrregiao, regiao, sigla_uf, municipio)),
        by = c("cd_municipio")
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_energetico") %>% select(-c(cd_subclasse, cd_rota, uso_modelo))),
        by = "cd_energetico"
      )

    dados_filtrados

  })


  ## Filtragem da combustivel_flat ----
  fato_combustivel_flat_filtrada <- reactive({

    inicio_periodo = input$filtro_periodo[1] * 10000
    fim_periodo = (input$filtro_periodo[2] + 1) * 10000

    porcentagem_densificacao <- case_when(input$rotas == "Densif." ~ 1, input$rotas == "Biod." ~ 0, input$rotas == "Mescladas" ~ 0.5)
    porcentagem_digestao <- 1 - porcentagem_densificacao

    cd_municipio_local <- municipio_selecionado()

    cd_subclasse_local <- subclasse_selecionado()

    dados_filtrados <- tbl(database$con, "fato_combustivel") %>%
      filter(
        cd_data >= inicio_periodo,
        cd_data < fim_periodo,
        cd_rota != 14,
        !(is.na(qtd_combustivel)&is.na(volume_combustivel)),
        cd_municipio %in% cd_municipio_local,
        cd_subclasse %in% cd_subclasse_local
      ) %>%
      collect() %>%
      left_join(
        collect(tbl(database$con, "dim_combustivel")),
        by = "cd_combustivel"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_subclasse") %>% select(cd_subclasse, n1_secao, n2_divisao, n3_grupo, n5_subclasse)),
        by = "cd_subclasse"
      ) %>%
      left_join(
        collect(tbl(database$con, "dim_municipio") %>% select(cd_municipio, cd_microrregiao, regiao, sigla_uf, municipio)),
        by = c("cd_municipio")
      ) %>%
      mutate(
        qtd_combustivel = if_else(cd_rota == 11, qtd_combustivel/1000, (qtd_combustivel*porcentagem_densificacao)/1000),
        volume_combustivel = if_else(cd_rota == 12, volume_combustivel/1000, (volume_combustivel*porcentagem_digestao)/1000),
        combustivel_ktep = qtd_combustivel*poder_calorifico_inf + volume_combustivel*poder_calorifico_inf,
        calor_ktep = combustivel_ktep*rend_caldeira,
        eletric_gwh = combustivel_ktep*rend_termoelet*11.63,
        pot_mw = eletric_gwh*1000/(8760*fator_cap)
      )

    dados_filtrados
  })


  ## Filtragem pelas ufs ----
  ufs_selecionadas <- reactive({
    sigla_ufs_selecionadas(input = input, connection = database$con)
  })

  municipio_selecionado <- reactive({
    cd_municipio_selecionado(input, database$con, ufs_selecionadas())
  })

  subclasse_selecionado <- reactive({
    cd_subclasse_selecionado(input, database$con)
  })



  ## Agrupamentos das flat ----
  # agrupamento para tabela produto
  fato_produto_flat_filtrada_agrupada_produto <-  reactive({

    agrupa_filtrados_produto_rend_lot(
      dados = fato_produto_flat_filtrada(),
      campos = c("produto", "n1_secao", "n2_divisao", "n3_grupo")
    )

  })

  # agrupamento para tabela energetico
  fato_energetico_flat_filtrada_agrupada_energetico <-  reactive({

    agrupamento_dados(
      dados = fato_energetico_flat_filtrada(),
      campos_agrupar = c('cd_energetico', 'energetico', 'n1_secao', 'n2_divisao', 'n3_grupo', 'cd_rota'),
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp')
    )

  })

  # agrupamento para a tabela combustivel
  fato_combustivel_flat_filtrada_agrupada_combustivel <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('combustivel', 'n1_secao', 'n2_divisao', 'n3_grupo'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep', 'calor_ktep', 'eletric_gwh', 'pot_mw')
    )


  })

  # agrupa os dados filtrados por região
  fato_combustivel_flat_filtrada_agrupada_regiao <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('regiao'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # agrupa os dados filtrados por microregião
  fato_combustivel_flat_filtrada_agrupada_microrregiao <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('cd_microrregiao'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # agrupa os dados filtrados por uf
  fato_combustivel_flat_filtrada_agrupada_uf <- reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('sigla_uf'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })

  # agrupa os dados filtrados por município
  fato_combustivel_flat_filtrada_agrupada_municipio <-  reactive({

    agrupamento_dados(
      dados = fato_combustivel_flat_filtrada(),
      campos_agrupar = c('cd_municipio', 'municipio'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep')
    )

  })


  ## Tabelas ----
  # produto
  output$tabela_produto <- reactable::renderReactable({
    req(input$filtro_periodo != 0, cancelOutput = TRUE)

    tictoc::tic('Modelo - fato_produto - collect()')
    dados_agregados <- fato_produto_flat_filtrada_agrupada_produto() %>% ungroup()
    tictoc::toc()

    totals_sums <- c(sum(dados_agregados$qtd_produzida_t), sum(dados_agregados$efetivo_rebanho), sum(dados_agregados$area_utilizada))

    tictoc::tic('Modelo - tabela_produto - beautiful()')
    dados_tabela <- beautiful_table(
      dados_brutos = dados_agregados,
      campos_agrupar = c('n1_secao', 'n2_divisao', 'n3_grupo', 'produto'),
      campos_somar = c('qtd_produzida_t', 'efetivo_rebanho', 'area_utilizada'),
      campos_informacao = c('rendimento', 'lotacao')
    )
    dados_tabela[dados_tabela == 0] <- NA
    tictoc::toc()

    lista_colunas <- list(
      geral = reactable::colDef(name = "Setores", footer = "Total", minWidth = 100, align = 'left'),
      qtd_produzida_t = reactable::colDef(minWidth = 60, footer = format(totals_sums[1], big.mark='.', decimal.mark = ','), header = reactable::JS('function(column) {return "Produção" + `<div(mil t)</div>`}')),
      efetivo_rebanho = reactable::colDef(minWidth = 58, footer = format(totals_sums[2], big.mark='.', decimal.mark = ','), header = reactable::JS('function(column) {return "Rebanho" + `<div>(mil cbç)</div>`}')),
      area_utilizada = reactable::colDef(minWidth = 52, footer = format(totals_sums[3], big.mark='.', decimal.mark = ','), header = reactable::JS('function(column) {return "Área" + `<div>(mil ha)</div>`}')),
      rendimento = reactable::colDef(format = reactable::colFormat(digits = 0), minWidth = 45, header = reactable::JS('function(column) {return "Rend." + `<div>(kg/ha)</div>`}')),
      lotacao = reactable::colDef(format = reactable::colFormat(digits = 2), minWidth = 50,header = reactable::JS('function(column) {return "Lotação" + `<div>(cbç/ha)</div>`}'))
    )

    tictoc::tic("Modelo - tabela_produto - reactable()")
    tabela <- cria_tabela_reactable(
      dados_tabela = dados_tabela,
      columns_list = lista_colunas
    )
    tictoc::toc()

    tabela
  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$cd_municipio
    )

  # energético
  output$tabela_energetico <- reactable::renderReactable({
    req(input$filtro_periodo != 0, cancelOutput = TRUE)

    tictoc::tic("Modelo - fato_energetico_flat_filtrada - (collect)")
    dados_agregados <- fato_energetico_flat_filtrada_agrupada_energetico() %>%
      ungroup() %>%
      left_join(
        collect(tbl(database$con, "dim_rota")),
        by = c("cd_rota")
      ) %>%
      left_join(
        collect(tbl(database$con, "fator_produto_energetico") %>% select(cd_energetico, ipr, fcol) %>% distinct()),
        by = "cd_energetico"
      ) %>%
      left_join(
        collect(tbl(database$con, "fator_energetico_combustivel") %>% select(cd_energetico, fdens) %>% filter(!is.na(fdens))),
        by = "cd_energetico"
      ) %>%
      left_join(
        collect(tbl(database$con, "fator_energetico_combustivel") %>% select(cd_energetico, fmetanizacao) %>% filter(!is.na(fmetanizacao))),
        by = "cd_energetico"
      ) %>%
      mutate(
        qtd_energetico = qtd_energetico/1000,
        qtd_energetico_disp = qtd_energetico_disp/1000
      )
    tictoc::toc()

    totals_sums <- c(sum(dados_agregados$qtd_energetico), sum(dados_agregados$qtd_energetico_disp))

    dados_tabela <- beautiful_table(
      dados_brutos = dados_agregados,
      campos_agrupar = c('n1_secao', 'n2_divisao', 'n3_grupo', 'energetico'),
      campos_somar = c('qtd_energetico', 'qtd_energetico_disp'),
      campos_informacao = c('ipr', 'fcol', 'fdens', 'fmetanizacao', 'rotas_possiveis'),
      output_order = c('ipr', 'qtd_energetico', 'fcol', 'qtd_energetico_disp', 'fdens', 'fmetanizacao', 'rotas_possiveis')
    )
    dados_tabela[dados_tabela == 0] <- NA

    lista_colunas <- list(
      geral = reactable::colDef(name = "Setores", footer = "Total", minWidth = 100, align = 'left'),
      ipr = reactable::colDef(name = "FCoprod.", format = reactable::colFormat(digits = 2), minWidth = 55),
      qtd_energetico = reactable::colDef(minWidth = 62, footer = format(totals_sums[1], big.mark='.', decimal.mark = ','),header = reactable::JS('function(column) {return "Coproduto" + `<div>(mil t)</div>`}')),
      fcol = reactable::colDef(name = "FColeta", format =reactable::colFormat(digits = 2), minWidth = 48),
      qtd_energetico_disp = reactable::colDef(minWidth = 62, footer = format(totals_sums[2], big.mark='.', decimal.mark = ','),header = reactable::JS('function(column) {return "Energético" + `<div>(mil t)</div>`}')),
      fdens = reactable::colDef(name = "FDensif", format = reactable::colFormat(digits = 2), minWidth = 48),
      fmetanizacao = reactable::colDef(name = "FMetan", format = reactable::colFormat(digits = 0), minWidth = 48),
      rotas_possiveis = reactable::colDef(name = "Rotas", minWidth = 70)
    )

    tictoc::tic("Modelo - tabela_energetico - reactable()")
    tabela <-  cria_tabela_reactable(
      dados_tabela = dados_tabela,
      columns_list = lista_colunas,
      background_color = "#BF8A15"
    )

    tictoc::toc()

    tabela
  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$cd_municipio
    )

  # combustivel
  output$tabela_combustivel <- reactable::renderReactable({
    req(input$filtro_periodo != 0, cancelOutput = TRUE)

    tictoc::tic("Modelo - combustivel_flat_filtrada - (collect)")
    dados_agregados <- fato_combustivel_flat_filtrada_agrupada_combustivel() %>%
      ungroup() %>%
      left_join(
        collect(tbl(database$con, "dim_combustivel")),
        by = "combustivel"
      )
    tictoc::toc()

    totals_sums <- c(sum(dados_agregados$qtd_combustivel), sum(dados_agregados$volume_combustivel), sum(dados_agregados$combustivel_ktep),
                     sum(dados_agregados$calor_ktep), sum(dados_agregados$eletric_gwh), sum(dados_agregados$pot_mw))

    dados_tabela <- beautiful_table(
      dados_brutos = dados_agregados,
      campos_agrupar = c('n1_secao', 'n2_divisao', 'n3_grupo', 'combustivel'),
      campos_somar = c('qtd_combustivel', 'volume_combustivel', 'combustivel_ktep', 'calor_ktep', 'eletric_gwh', 'pot_mw'),
      campos_informacao = c('poder_calorifico_inf', 'rend_caldeira', 'rend_turbina', 'rend_termoelet', 'fator_cap'),
      output_order = c('qtd_combustivel','volume_combustivel','poder_calorifico_inf','combustivel_ktep','rend_caldeira','calor_ktep','rend_turbina', 'rend_termoelet','eletric_gwh', 'fator_cap', 'pot_mw')
    )
    dados_tabela[dados_tabela == 0] <- NA

    lista_colunas <- list(
      geral = reactable::colDef(name = "Setores", minWidth = 100, footer = "Total", align = 'left'),
      qtd_combustivel = reactable::colDef(minWidth = 50, footer = format(totals_sums[1], big.mark='.', decimal.mark = ','), header = reactable::JS('function(column) {return "Comb." + `<div>(mil t)</div>`}')),
      volume_combustivel = reactable::colDef(minWidth = 50, footer = format(totals_sums[2], big.mark='.', decimal.mark = ','), header = reactable::JS('function(column) {return "Comb." + `<div>(mi m³)</div>`}')),
      poder_calorifico_inf = reactable::colDef(name = "PCI", minWidth = 50, format = reactable::colFormat(digits = 2)),
      combustivel_ktep = reactable::colDef(minWidth = 50, footer = format(totals_sums[3], big.mark='.', decimal.mark = ','), header = reactable::JS('function(column) {return "Comb." + `<div>(ktep)</div>`}')),
      rend_caldeira = reactable::colDef(name = "RCald", minWidth = 40, format = reactable::colFormat(digits = 2)),
      calor_ktep= reactable::colDef(minWidth = 50, footer = format(totals_sums[4], big.mark='.', decimal.mark = ','), header = reactable::JS('function(column) {return "Calor" + `<div>(ktep)</div>`}')),
      rend_turbina = reactable::colDef(name = "RTurb", minWidth = 40, format = reactable::colFormat(digits = 2)),
      rend_termoelet = reactable::colDef(name = "RTE", minWidth = 40, format = reactable::colFormat(digits = 2)),
      eletric_gwh = reactable::colDef(minWidth = 50, footer = format(totals_sums[5], big.mark='.', decimal.mark = ','), header = reactable::JS('function(column) {return "Eletric." + `<div>(GWh)</div>`}')),
      fator_cap = reactable::colDef(name = "Fcap", minWidth = 40, format = reactable::colFormat(digits = 2)),
      pot_mw= reactable::colDef(aggregate = "sum", minWidth = 50, footer =  format(totals_sums[6], big.mark='.', decimal.mark = ','), header = reactable::JS('function(column) {return "Pot." + `<div>(MW)</div>`}'))
    )

    tictoc::tic("Modelo - tabela_combustivel - reactable")
    tabela <-  cria_tabela_reactable(
      dados_tabela = dados_tabela,
      columns_list = lista_colunas,
      background_color = "#BF714D"
    )
    tictoc::toc()

    tabela
  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$cd_municipio
    )


  ## Mosaicos ----
  # mosaico de energia por regiao
  output$mosaico_regioes <- renderGirafe({
    req(input$filtro_periodo != 0, cancelOutput = TRUE)

    dados_grafico <- fato_combustivel_flat_filtrada_agrupada_regiao() %>%
      group_by(
        regiao
      ) %>%
      summarise(
        combustivel_ktep = sum(combustivel_ktep, na.rm = TRUE)
      )

    tictoc::tic("Modelo - mosaico - regiao")
    saida <- gera_grafico_mosaico(dados = dados_grafico,
                                  area = "combustivel_ktep",
                                  categorizacao = "regiao",
                                  tooltip = "Energia (tep)",
                                  height = 2.25)

    tictoc::toc()
    saida

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$cd_municipio
    )


  ## Mapas ----
  # mapa brasil por uf
  output$mapa_brasil <- renderGirafe({
    req(input$filtro_periodo != 0, cancelOutput = TRUE)

    ufs_selecionadas_local <- ufs_selecionadas()

    dados_por_uf <- fato_combustivel_flat_filtrada_agrupada_uf()

    combustivel_por_uf_geo <- sienergiaviewer::poligonos_estados_simples %>%
      left_join(
        dados_por_uf,
        by = c("abbrev_state" = "sigla_uf")
      )

    tictoc::tic("Modelo - mapa - uf")
    saida <- gera_mapa_ufs(dados = combustivel_por_uf_geo,
                           ufs_selecionadas = ufs_selecionadas_local,
                           fill = "combustivel_ktep",
                           tooltip = 'Comb. (ktep)',
                           titulo = "Combustível (ktep) por UF")

    tictoc::toc()
    saida

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$cd_municipio
    )

  # mapa das microregiões sobre a área selecionada (Brasil ou UF específica)
  output$mapa_microrregioes <- renderGirafe({

    cd_municipio_local <- input$cd_municipio

    if(length(cd_municipio_local) > 0 | length(ufs_selecionadas()) == 1){
      dados_mapa <- fato_combustivel_flat_filtrada_agrupada_municipio()
      dados_com_geo <- sienergiaviewer::poligonos_municipios_simples %>%
        inner_join(
          dados_mapa,
          by = c("code_muni" = "cd_municipio")
        )

      tictoc::tic("Modelo - mapa - municipios")
      saida <- gera_mapa_municipios(dados = dados_com_geo,
                                code_muni_local = cd_municipio_local,
                                fill = "combustivel_ktep",
                                tooltip = 'Comb.(ktep)')
      tictoc::toc()

    } else {
      dados_mapa <- fato_combustivel_flat_filtrada_agrupada_microrregiao()
      dados_com_geo <- sienergiaviewer::poligonos_microrregiao_simples %>%
        inner_join(
          dados_mapa,
          by = c("code_micro" = "cd_microrregiao")
        )

      tictoc::tic("Modelo - mapa - microrregiao")
      saida <- gera_mapa_micros(dados = dados_com_geo,
                                    code_muni_local = cd_municipio_local,
                                    fill = "combustivel_ktep")
      tictoc::toc()
    }

    saida

  }) %>%
    bindCache(
      input$rotas,
      input$setor,
      input$subsetor,
      input$subsubsetor,
      input$n5_subclasse,
      input$filtro_periodo,
      input$mapa_brasil_selected,
      input$mosaico_regioes_selected,
      input$cd_municipio
    )


  ## Download Handlers ----
  output$download_produto <- downloadHandler(
    filename = 'tbl_produto.csv',
    content = function(file){
      write.csv2(fato_produto_flat_filtrada_agrupada_produto(), file)
    }
  )

  output$download_energetico <- downloadHandler(
    filename = 'tbl_energetico.csv',
    content = function(file){
      write.csv2(fato_energetico_flat_filtrada_agrupada_energetico(), file)
    }
  )

  output$download_combustivel <- downloadHandler(
    filename = 'tbl_combustivel.csv',
    content = function(file){
      write.csv2(fato_combustivel_flat_filtrada_agrupada_combustivel(), file)
    }
  )
}

## To be copied in the UI
# mod_tela_produto_ui("tela_produto_ui")

## To be copied in the server
# callModule(mod_tela_produto_server, "tela_produto_ui")

