#' Cria Filtros sem Rota
#'
#' Esta função cria uma faixa de filtros, sem conter filtro de rota, utilizada no aplicativo Shiny.
#' Dentro do SIEnergia Viewer é usada nos paineis de produto e energético visto que esses não são afetados pelo filtro de rotas.
#' Note que essa função apenas faz o esqueleto dos filtros e não os preenchem com valores.
#'
#' @param id O ID usado para nomear os elementos de entrada evitando conflito entre modulos.
#'
#' @return Um objeto `html` a ser usado na UI do aplicativo shiny.
#'
#' @import shiny
#' @importFrom htmltools div
#' @importFrom shinyWidgets checkboxGroupButtons radioGroupButtons
cria_filtros_sem_rota <- function(id){

  filtro_periodo <- sliderInput(
    inputId = NS(id, "filtro_periodo"),
    label = NULL,
    min = 0,
    max = 0,
    value = c(0,0),
    step = 1,
    sep = ''
  )

  filtro_municipio <- selectizeInput(
    inputId = NS(id, 'cd_municipio'),
    label = 'Municipio',
    choices = c('Carrega' = NULL),
    multiple = TRUE,
    options = list(maxOptions = 3)
  )

  filtro_setor <- checkboxGroupButtons(
    inputId = NS(id,"setor"),
    label = "Setor nível 1",
    choices = c("Carrega"),
    selected = ""
  )

  filtro_subsetor <- checkboxGroupButtons(
    inputId = NS(id,"subsetor"),
    label = "Setor nível 2",
    choices = c("Carrega"),
    selected = ""
  )


  filtro_subsubsetor <- checkboxGroupButtons(
    inputId = NS(id,"subsubsetor"),
    label = "Setor nível 3",
    choices = c("Carrega"),
    selected = ""
  )

  filtro_n5 <- checkboxGroupButtons(
    inputId = NS(id,"n5_subclasse"),
    label = "Setor nível 5",
    choices = c("Carrega"),
    selected = ""
  )

  saida <-
    splitLayout(
      cellWidths = c("18%", "57%", "25%"),
      cellArgs = list(style = "padding: 6px"),
      verticalLayout(
        filtro_setor,
        filtro_subsetor
      ),
      verticalLayout(
        filtro_subsubsetor,
        filtro_n5
      ),
      div(
        filtro_municipio,
        filtro_periodo
      )
    )

  return(saida)
}


#' Cria Filtros com Rota
#'
#' Esta função cria uma faixa de filtros, contendo o filtro de rota, utilizada no aplicativo Shiny.
#' Dentro do SIEnergia Viewer é usada nos paineis de modelo, combustível e energia visto que esses são afetados pelo filtro de rotas.
#' Note que essa função apenas faz o esqueleto dos filtros e não os preenchem com valores.
#'
#' @param id O ID usado para nomear os elementos de entrada evitando conflito entre modulos.
#'
#' @return Um objeto `html` a ser usado na UI do aplicativo shiny.
#'
#' @import shiny
#' @importFrom htmltools div
#' @importFrom shinyWidgets checkboxGroupButtons
cria_filtros_com_rota <- function(id){

  filtro_periodo <- sliderInput(
    inputId = NS(id, "filtro_periodo"),
    label = NULL,
    min = 0,
    max = 0,
    value = c(0,0),
    step = 1,
    sep = ''
  )

  filtro_municipio <- selectizeInput(
    inputId = NS(id, 'cd_municipio'),
    label = 'Municipio',
    choices = c('Carrega' = NULL),
    multiple = TRUE,
    options = list(maxOptions = 3)
  )

  filtro_setor <- checkboxGroupButtons(
    inputId = NS(id,"setor"),
    label = "Setor nível 1",
    choices = c("Carrega"),
    selected = ""
  )

  filtro_subsetor <- checkboxGroupButtons(
    inputId = NS(id,"subsetor"),
    label = "Setor nível 2",
    choices = c("Carrega"),
    selected = ""
  )


  filtro_subsubsetor <- checkboxGroupButtons(
    inputId = NS(id,"subsubsetor"),
    label = "Setor nível 3",
    choices = c("Carrega"),
    selected = ""
  )

  filtro_n5 <- checkboxGroupButtons(
    inputId = NS(id,"n5_subclasse"),
    label = "Setor nível 5",
    choices = c("Carrega"),
    selected = ""
  )

  filtro_rotas <- shinyWidgets::radioGroupButtons(
    inputId = NS(id,"rotas"),
    label = "Rotas Possíveis",
    choiceNames = c("Densificação",
                    "Rotas Mescladas",
                    "Biodigestão"
    ),
    choiceValues = c("Densif.",
                     "Mescladas",
                     "Biod."
    ),
    selected = "Mescladas",
    direction = "vertical"
  )

  saida <-
    splitLayout(
      cellWidths = c("18%", "40%", "17%","25%" ),
      cellArgs = list(style = "padding: 6px"),
      verticalLayout(
        filtro_setor,
        filtro_subsetor
      ),
      verticalLayout(
        filtro_subsubsetor,
        filtro_n5
      ),
      verticalLayout(
        tags$div(filtro_rotas,class = "alinhar")
      ),
      div(
        filtro_municipio,
        filtro_periodo
      )
    )

  return(saida)
}


#' Cria Well Panel Geral
#'
#' Esta função cria o corpo principal dos paineis, exceto modelo, do aplicativo Shiny, contendo os gráficos, tabelas e botões de download.
#'
#' @param id O ID usado para nomear os elementos de saída.
#'
#' @return Um objeto `html` a ser usado na UI do aplicativo shiny.
#'
#' @import shiny
#' @importFrom htmltools div
#' @importFrom shinyWidgets checkboxGroupButtons
cria_well_panel_geral <- function(id){
  # gráfico/mapa
  linha_evolucao <- girafeOutput(NS(id,"linha_evolucao"))
  mosaico_setor <- girafeOutput(NS(id,"mosaico_setor"))
  barras_subsetor <- girafeOutput(NS(id,"barras_subsetor"))
  mosaico_produto <- girafeOutput(NS(id,"mosaico_produto"))
  mosaico_regioes <- girafeOutput(NS(id,"mosaico_regioes"))
  mapa_brasil <- girafeOutput(NS(id,"mapa_brasil"))
  mapa_micros <- girafeOutput(NS(id,"mapa_microrregioes"))

  # tabela/resumo
  tabela_evolucao <- reactable::reactableOutput(NS(id, "tabela_evolucao"), height = "20vh")
  tabela_secao <- reactable::reactableOutput(NS(id,"tabela_modulo"), height = "21vh")
  tabela_municipio <- reactable::reactableOutput(NS(id,"tabela_municipio"), height = "29vh")

  # downloads
  download_temporal <- downloadButton(NS(id,"download_temporal"), label = NULL, class = 'download')
  download_setorial <- downloadButton(NS(id,"download_setorial"), label = NULL, class = 'download')
  download_espacial <- downloadButton(NS(id,"download_espacial"), label = NULL, class = 'download')

  informacoes <- htmlOutput(NS(id,"info"))


  ## Estrutura auxiliar ----
  info_produto <- tags$div(
    class = "info_produto",
    tags$div(informacoes),
    tags$div(mosaico_produto, class = "mosaico_produto")
  )

  regiao_uf <- verticalLayout(
    tags$div(mosaico_regioes, class = "mosaico_regioes"),
    tags$div(mapa_brasil, class = "mapa_uf")
  )

  regiao_mapa <- verticalLayout(
    splitLayout(
      cellWidths = c("40%", "60%"),
      regiao_uf,
      tags$div(mapa_micros, class = "mapa_micro")
    )
  )

  bloco_temporal <- div(class = "bloco_painel", splitLayout(
    cellWidths = c("38%", "60%",'2%'),
    tags$div(linha_evolucao, class = "linhas"),
    tabela_evolucao,
    download_temporal
  ))

  bloco_setorial <- div(class = "bloco_painel",splitLayout(
    cellWidths = c("9%", "29%", "60%", '2%'),
    tags$div(mosaico_setor, class = "mosaico_setor"),
    tags$div(barras_subsetor, class = "barras"),
    tabela_secao,
    download_setorial
  ))

  bloco_espacial <- div(class = "bloco_painel", splitLayout(
    cellWidths = c("40%", "58%", '2%'),
    regiao_mapa,
    tabela_municipio,
    download_espacial
  ))

  bloco_conteudo <- verticalLayout(
    bloco_temporal,
    bloco_setorial,
    bloco_espacial
  )

  # agrupamento de todas as tabelas e gráficos
  well_panel_conteudo <- wellPanel(
    splitLayout(
      cellWidths = c("10%","90%"),
      info_produto,
      bloco_conteudo
    )
  )

  well_panel_conteudo
}


#' Cria Well Panel Modelo
#'
#' Esta função cria o corpo principal do painel modelo do aplicativo Shiny, contendo os gráficos, tabelas e botões de download.
#'
#' @param id O ID usado para nomear os elementos de saída.
#'
#' @return Um objeto `html` a ser usado na UI do aplicativo shiny.
#'
#' @import shiny
#' @importFrom htmltools div
#' @importFrom shinyWidgets checkboxGroupButtons
cria_well_panel_modelo <- function(id){
  # gráfico/mapa
  mosaico_regioes <- girafeOutput(NS(id,"mosaico_regioes"))
  mapa_brasil <- girafeOutput(NS(id,"mapa_brasil"))
  mapa_micros <- girafeOutput(NS(id,"mapa_microrregioes"))

  # tabelas
  tabela_produto <- verticalLayout(
    tags$div("Produção Agrícola", style = style_title),
    reactable::reactableOutput(NS(id,"tabela_produto"), height = "33vh"),
  )
  tabela_energetico <- verticalLayout(
    tags$div("Produção Energética", style = style_title),
    reactable::reactableOutput(NS(id,"tabela_energetico"), height = "33vh"),
  )
  tabela_combustivel <- verticalLayout(
    tags$div("Produção de combustível", style = style_title),
    reactable::reactableOutput(NS(id,"tabela_combustivel"), height = "33vh")
  )

  #downloads
  download_produto <- downloadButton(NS(id,"download_produto"), label = NULL, class = 'download_modelo')
  download_energetico <- downloadButton(NS(id,"download_energetico"), label = NULL, class = 'download_modelo')
  download_combustivel <- downloadButton(NS(id,"download_combustivel"), label = NULL, class = 'download_modelo')

  ## Estrutura auxiliar ----
  regiao_uf <- div(
    tags$div(mosaico_regioes, class = "mosaico_regioes"),
    tags$div(mapa_brasil, class = "mapa_uf")
  )

  regiao_mapa <- div(
    style = "padding-top:30px;",
    splitLayout(
      cellWidths = c("40%", "60%"),
      regiao_uf,
      tags$div(mapa_micros, class = "mapa_micro")
    )
  )

  bloco_superior <- splitLayout(
    cellWidths = c("43%",'2%',"53%",'2%'),
    cellArgs = list(style = "padding: 2px"),
    tabela_produto,
    download_produto,
    tabela_energetico,
    download_energetico
  )

  bloco_inferior <- splitLayout(
    cellWidths = c("40%", "58%", '2%'),
    regiao_mapa,
    tabela_combustivel,
    download_combustivel
  )

  well_panel_conteudo <- wellPanel(
    style = "background-color: #FFFFFF;",
    tags$div(
      bloco_superior,
      bloco_inferior
    )
  )

  well_panel_conteudo
}







