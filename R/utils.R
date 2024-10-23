

library(tidyverse)
library(shinydashboard)
library(dashboardthemes)
library(patchwork)
library(ggiraph)
library(readxl)
library(RColorBrewer)
library(scales)
library(latex2exp)
library(colorspace)
library(reactable)
library(writexl)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(janitor)
library(ggmap)
library(geobr)
library(sf)
library(gghighlight)
library(feather)
library(here)
library(labelled)
library(sidrar)
library(lubridate)

#### TRADUÇÃO ####

str_to_title_pt <- function(x){
  str_to_title(x) %>%
    str_replace_all(" De ", " de ") %>%
    str_replace_all(" Da ", " da ")

}

portugues <- "port"

ingles <- "ing"

lingua <- portugues

dicionario_original <- read_excel("dicionario/dicionario.xlsx") %>%
  mutate_all(str_trim)

dicionario_minusculo <- dicionario_original %>%
  mutate_all(str_to_lower)

dicionario_title <- dicionario_original %>%
  mutate_all(str_to_title_pt)

dicionario_maiusculo <- dicionario_original %>%
  mutate_all(str_to_upper)

dicionario_sentence <- dicionario_original %>%
  mutate_all(str_to_sentence)

dicionario <- bind_rows(
  dicionario_original,
  dicionario_minusculo,
  dicionario_title,
  dicionario_maiusculo,
  dicionario_sentence
) %>%
  group_by(port) %>%
  summarise_all(first) %>%
  mutate(
    base = port
  )

traduz <- function(port_param){


  port_param %>%
    enframe(value = "antes_traducao") %>%
    mutate(
      linha = row_number(),
    ) %>%
    left_join(dicionario,
              by = c("antes_traducao" = "base")
    ) %>%
    group_by(
      antes_traducao,
      linha
    ) %>%
    summarise_all(
      first
    ) %>%
    mutate(
      resultado = .data[[lingua]]
    ) %>%
    mutate(
      resultado = if_else(is.na(resultado), antes_traducao, resultado)
    ) %>%
    arrange(
      linha
    ) %>%
    pull(resultado)

}

#### TEMAS ####
paleta_epe <- c(
  "#001F66",
  "#FF8000",
  "#780116",
  "#034732",
  "#AF9164"
)

extra_paleta_2 <- brewer.pal(n = 90,  name = 'Set1' )
extra_paleta_1 <- brewer.pal(n = 90,  name = 'Accent' )
extra_paleta_3 <- brewer.pal(n = 90,  name = 'Set3' )
extra_paleta_4 <- brewer.pal(n = 90,  name = 'Pastel2' )
extra_paleta_5 <- brewer.pal(n = 90,  name = 'Dark2' )
extra_paleta_6 <- brewer.pal(n = 90,  name = 'Pastel1' )
extra_paleta_7 <- brewer.pal(n = 90,  name = 'Set2' )
extra_paleta_8 <- brewer.pal(n = 90,  name = 'Paired' )

paleta_todas <- c(paleta_epe, extra_paleta_1, extra_paleta_2, extra_paleta_3, extra_paleta_4, extra_paleta_5, extra_paleta_6, extra_paleta_7, extra_paleta_8 )

paleta <- paleta_todas

cores_fontes <- c(
  "darkgreen",
  "chocolate4",
  "dimgray",
  "black",
  "navy",
  "darkgreen",
  "deeppink2",
  "firebrick1",
  "burlywood3",
  "aquamarine4",
  "#3F1708",
  "orange4",
  "darkgoldenrod1",
  "darkorchid4"
)

cores_fontes_escuras <- darken(cores_fontes, amount = 0.4 )

paleta_fontes <- c(
  "RENOVÁVEL" = cores_fontes[1],
  "NÃO RENOVÁVEL"= cores_fontes[2],
  "CARVÃO METALÚRGICO" = cores_fontes[3],
  "CARVÃO VAPOR" = cores_fontes[4],
  "ENERGIA HIDRÁULICA " = cores_fontes[5],
  "EÓLICA" = cores_fontes[6],
  "GÁS NATURAL" = cores_fontes[7],
  "LENHA" = cores_fontes[8],
  "OUTRAS NÃO RENOVÁVEIS" = cores_fontes[9],
  "OUTRAS RENOVÁVEIS" = cores_fontes[10],
  "PETRÓLEO" = cores_fontes[11],
  "PRODUTOS DA CANA" = cores_fontes[12],
  "SOLAR" = cores_fontes[13],
  "URÂNIO (U3O8)" = cores_fontes[14]

)

paleta_fontes_escura <- c(
  "RENOVÁVEL" = cores_fontes_escuras[1],
  "NÃO RENOVÁVEL" = cores_fontes_escuras[2],
  "CARVÃO METALÚRGICO" = cores_fontes_escuras[3],
  "CARVÃO VAPOR" = cores_fontes_escuras[4],
  "ENERGIA HIDRÁULICA " = cores_fontes_escuras[5],
  "EÓLICA" = cores_fontes_escuras[6],
  "GÁS NATURAL" = cores_fontes_escuras[7],
  "LENHA" = cores_fontes_escuras[8],
  "OUTRAS NÃO RENOVÁVEIS" = cores_fontes_escuras[9],
  "OUTRAS RENOVÁVEIS" = cores_fontes_escuras[10],
  "PETRÓLEO" = cores_fontes_escuras[11],
  "PRODUTOS DA CANA" = cores_fontes_escuras[12],
  "SOLAR" = cores_fontes_escuras[13],
  "URÂNIO (U3O8)" = cores_fontes_escuras[14]
)

paleta_fontes_escura_title <- paleta_fontes_escura

names(paleta_fontes_escura_title) <- str_to_title_pt(names(paleta_fontes_escura))

paleta_fontes_escura <- c(paleta_fontes_escura, paleta_fontes_escura_title)

paleta_fontes_title <- paleta_fontes

names(paleta_fontes_title) <- str_to_title_pt(names(paleta_fontes))

paleta_fontes <- c(paleta_fontes, paleta_fontes_title)

# theme_custom <- shinyDashboardThemes(
#
#   ### general
#   appFontFamily = "Helvetica",
#   appFontColor = "#001F66"
#   ,primaryFontColor = "rgb(15,15,15)"
#   ,infoFontColor = "rgb(15,15,15)"
#   ,successFontColor = "rgb(15,15,15)"
#   ,warningFontColor = "rgb(15,15,15)"
#   ,dangerFontColor = "rgb(15,15,15)"
#   ,bodyBackColor = "rgb(240,240,240)"
#
#   ### header
#   ,logoBackColor = "#001F66"
#
#   ,headerButtonBackColor = "#001F66"
#   ,headerButtonIconColor = "rgb(220,220,220)"
#   ,headerButtonBackColorHover = "rgb(100,100,100)"
#   ,headerButtonIconColorHover = "rgb(60,60,60)"
#
#   ,headerBackColor = "#001F66"
#   ,headerBoxShadowColor = "#dfdfdf"
#   ,headerBoxShadowSize = "3px 5px 5px"
#
#   ### sidebar
#   ,sidebarBackColor = "rgb(255,255,255)"
#   ,sidebarPadding = 0
#
#   ,sidebarMenuBackColor = "transparent"
#   ,sidebarMenuPadding = 0
#   ,sidebarMenuBorderRadius = 0
#
#   ,sidebarShadowRadius = "3px 5px 5px"
#   ,sidebarShadowColor = "#dfdfdf"
#
#   ,sidebarUserTextColor = "rgb(115,115,115)"
#
#   ,sidebarSearchBackColor = "rgb(240,240,240)"
#   ,sidebarSearchIconColor = "rgb(100,100,100)"
#   ,sidebarSearchBorderColor = "rgb(220,220,220)"
#
#   ,sidebarTabTextColor = "#001F66"
#   ,sidebarTabTextSize = 11
#   ,sidebarTabBorderStyle = "none"
#   ,sidebarTabBorderColor = "none"
#   ,sidebarTabBorderWidth = 0
#
#   ,sidebarTabBackColorSelected = "rgb(230,230,230)"
#   ,sidebarTabTextColorSelected = "rgb(0,0,0)"
#   ,sidebarTabRadiusSelected = "0px"
#
#   ,sidebarTabBackColorHover = "rgb(245,245,245)"
#   ,sidebarTabTextColorHover = "rgb(0,0,0)"
#   ,sidebarTabBorderStyleHover = "none solid none none"
#   ,sidebarTabBorderColorHover = "rgb(200,200,200)"
#   ,sidebarTabBorderWidthHover = 4
#   ,sidebarTabRadiusHover = "0px"
#
#   ,boxBackColor = "rgb(255, 255, 255)"
#   ,boxBorderRadius = 5
#   ,boxShadowSize = "none"
#   ,boxShadowColor = ""
#   ,boxTitleSize = 18
#   ,boxDefaultColor = "rgb(255,255,255)"
#   ,boxPrimaryColor = "rgb(255, 255, 255)"
#   ,boxInfoColor = "rgb(180,180,180)"
#   ,boxSuccessColor = "rgb(112,173,71)"
#   ,boxWarningColor = "rgb(237,125,49)"
#   ,boxDangerColor = "rgb(232,76,34)"
#
#   ,tabBoxTabColor = "rgb(255,255,255)"
#   ,tabBoxTabTextSize = 6
#   ,tabBoxTabTextColor = "rgb(100,100,100)"
#   ,tabBoxTabTextColorSelected = "rgb(45,45,45)"
#   ,tabBoxBackColor = "rgb(255,255,255)"
#   ,tabBoxHighlightColor = "rgb(200,200,200)"
#   ,tabBoxBorderRadius = 5
#
#
#   ,textboxBackColor = "rgb(255,255,255)"
#   ,textboxBorderColor = "rgb(118,118,118)"
#   ,textboxBorderRadius = 5
#   ,textboxBackColorSelect = "rgb(245,245,245)"
#   ,textboxBorderColorSelect = "rgb(108,108,108)"
#
#   ### tables
#   ,tableBackColor = "rgb(248,248,248)"
#   ,tableBorderColor = "rgb(238,238,238)"
#   ,tableBorderTopSize = 1
#   ,tableBorderRowSize = 1
#
# )
#

size_linha <-  1.5
alpha_linha <-  0.7

shape_ponto <-  21
size_ponto <-  1.75
stroke_ponto <-  0.5
alpha_ponto <-  0.8


tema_detalhes <- theme(
  legend.position = "top",
  axis.title = element_text(size = 14),
  text = element_text(family = "Helvetica", colour = paleta[1], face = "bold"),
  axis.text.x.top = element_text(family = "Helvetica", colour = paleta[1], face = "bold", size = 14),
  axis.text = element_text(family = "Helvetica", colour = paleta[1], face = "bold", size = 14),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  axis.line.x = element_line(size = 0.1, colour = paleta[1] ),
  axis.line.y = element_line(size = 0.1, colour = paleta[1] ),
  legend.text = element_text(family = "Helvetica", colour = paleta[1], face = "bold", size = 13),
  legend.title = element_text(family = "Helvetica", colour = paleta[1], face = "bold", size = 16)

)

width_svg = 20
height_svg = 4.5

style_title <- "font-size: 13px; text-align: left; font-weight: bold; padding-left: 10px; height: 20px;"

#### Módulos Shiny ####

# This is a new style of module construction that will appear in shiny 1.5.0.
# For now, we're adding a simple shim that you can use on your own, and delete
# when shiny 1.5.0 is out.
moduleServer <- function(id, module) {
  callModule(module, id)
}


#### LEITURA DADOS ####

# Variaveis auxiliares

ano_pne <- 2050

texto_inicio <- 'Empresa pública, vinculada ao Ministério de Minas e Energia, instituída nos termos da Lei nº 10.847, de 15 de março de 2004, a EPE tem por finalidade prestar serviços na área de estudos e pesquisas destinadas a suvsidiar o planejamento do setor energético, tais como energia elétrica, petróleo e gás natural e seus derivados, carvão mineral, fontes energéticas renováveis e eficiência energética, dentre outras.'


#### FUNÇÕES AUXILIARES ####

#' Cria um tema personalizado para cabeçalhos e rodapés de tabelas Reactable
#'
#' Esta função cria um tema personalizado para os cabeçalhos e rodapés de tabelas Reactable, definindo o estilo dos cabeçalhos e rodapés.
#'
#' @param color A cor de fundo dos cabeçalhos e rodapés da tabela.
#'
#' @return Um objeto de tema personalizado para tabelas Reactable.
tema_header_footer_reactable <- function(color){
  reactable::reactableTheme(
    headerStyle = list(
      display = "flex",
      justifyContent = "center",
      backgroundColor = color,
      color = "rgb(240,255,255)"
    ),
    footerStyle = list(
      backgroundColor = color,
      color = "rgb(240,255,255)"
    )
  )
}

#' Função auxiliar à tabela evolução
#'
#' Na tabela evolução de cada painel temos uma coluna para cada cultura.
#' Devido a natureza do Reactable não podemos gerar as colunas reativamente e por isso temos essa função.
#' A função apenas gera uma instancia de cada 'n3_grupo' para que a Reactable possa gerar a tabela sem dar o erro de falta de dados.
#'
#' @param ano ano a ser feita a instanciação, deve ser o ultimo ano a ser mostrado na tabela
#'
#' @return tibble com instanciação de todos os 'n3_grupo'
#'
#' @importFrom tibble tribble
padronizacao_tabela_evolucao <- function(ano = 2021){
  tribble(
    ~ano, ~n3_grupo, ~filler,
    ano, 'L. Permanentes', NA_real_,
    ano, 'L. Temporárias', NA_real_,
    ano, 'Pecuária', NA_real_,
    ano, 'Silvicultura', NA_real_,
    ano, 'Extrativismo', NA_real_,
    ano, 'Coleta', NA_real_,
    ano, 'Esgoto', NA_real_,
    ano, 'Abate', NA_real_,
    ano, 'Conserva', NA_real_,
    ano, 'Moagem', NA_real_,
    ano, 'Recuperação', NA_real_
  )
}

#' População por município
#'
#' Esta função retorna uma tabela com informações sobre a população de cada município,
#' incluindo dados demográficos e campos adicionais fornecidos.
#'
#' @param dados Um data frame contendo informações sobre os municípios.
#' @param connection Uma conexão com o banco de dados que contém a tabela da população.
#' @param inicio_periodo A data de início do período desejado para filtrar os dados da população.
#' @param fim_periodo A data de término do período desejado para filtrar os dados da população.
#' @param campos_informacao Um vetor com os nomes dos campos de informação adicionais a serem incluídos na saída.
#'
#' @return Um data frame com informações sobre a população por município, incluindo os campos de informação especificados.
#'
#' @import dplyr
#' @import DBI
populacao_tabela_municipio <- function(dados, connection, inicio_periodo, fim_periodo, campos_informacao){

  dados %>%
    ungroup() %>%
    left_join(
      collect(tbl(connection, 'fato_populacao')) %>%
        filter(cd_data >= inicio_periodo, cd_data < fim_periodo) %>%
        group_by(cd_municipio) %>% summarise(populacao = sum(populacao), .groups = 'drop'),
      by = c('cd_municipio')
    ) %>%
    select(c(municipio, sigla_uf, populacao, campos_informacao))

}

#' Sigla Abreviada
#'
#' Esta função reescreve um número adicionando uma sigla abreviada correspondente, indicando a ordem de magnitude.
#'
#' @param x O número a ser convertido em sigla abreviada.
#' @param accuracy O número de dígitos decimais a serem arredondados (padrão: 3).
#'
#' @return Uma string contendo o numero com a sigla de magnitude.
#'
#' @examples "sigla_abrev(10000) == '10 mil'"
sigla_abrev <- function(x, accuracy = 3){
  ordem <- pmax(floor(log(abs(x), 1000)), 0)

  paste(round(x / (1000 ^ ordem), digits = accuracy),
        c('', ' mil', ' Mi', ' Bi')[ordem + 1])
}


#' Texto para Tooltip
#'
#' Esta função cria o texto para exibição no tooltip de um gráfico, com base nos dados, na categoria e nas informações fornecidas.
#'
#' @param dados_grafico Um conjunto de dados contendo as informações para o gráfico.
#' @param categoria A coluna que representa a categoria para agrupar os dados.
#' @param info A coluna que contém as informações a serem exibidas no tooltip.
#'
#' @return Um conjunto de dados com uma coluna adicional contendo o texto formatado para o tooltip.
#'
#' @import dplyr
texto_tooltip <- function(dados_grafico, categoria, info){

  texto <- dados_grafico %>%
    summarise(
      texto = list(paste(.data[[categoria]], sigla_abrev(.data[[info]]), sep = " = "))
    ) %>%
    rowwise() %>%
    mutate(
      texto = list(append(ano,texto))
    ) %>%
    ungroup()

  dados_grafico <- left_join(x = dados_grafico, y = texto, by = "ano") %>%
    rowwise() %>%
    mutate(
      texto = str_flatten(texto, collapse = "<br/>")
    ) %>%
    group_by(.data[[categoria]])

  dados_grafico

}




