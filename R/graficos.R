#' Gerar Gráfico de Mosaico
#'
#' Esta função gera um gráfico de mosaico a partir dos dados fornecidos. 
#' O gráfico de mosaico exibe a distribuição de uma variável em áreas retangulares proporcionais ao valor dessa variável. 
#' O tamanho dos retângulos é determinado pela área especificada nos dados.
#'
#' @param dados Um dataframe contendo os dados a serem plotados no gráfico de mosaico.
#' @param area O nome da coluna que representa a área dos retângulos no gráfico de mosaico.
#' @param categorizacao O nome da coluna que será usada para categorizar os retângulos no gráfico de mosaico.
#' @param tooltip Uma string contendo o texto que será exibido como tooltip para cada retângulo. Pode conter placeholders como "{area}" para representar o valor da área.
#' @param height A altura do gráfico de mosaico. Se não for especificada, a altura padrão será usada.
#' @param porcentagem Um valor lógico indicando se a área deve ser exibida como porcentagem. Se for TRUE, a área será multiplicada por 100.
#' 
#' @return Um objeto girafe contendo o gráfico de mosaico.
#' 
#' @import dplyr
#' @import treemapify
#' @import ggplot2
#' @import ggiraph
#' @import scales
gera_grafico_mosaico <- function(dados, area, categorizacao,  tooltip = NULL, height = NULL, porcentagem = FALSE){
  
  if(length(dados[[1]]) == 0 | sum(dados[[area]], na.rm = TRUE)<=0){
    return(ggplot()+theme_void())
  }
  #browser()
  dados <- dados %>%
    rowwise() %>% 
    mutate(
      "{area}" := if_else(is.na(.data[[area]]), 0, .data[[area]]),
      "{area}" := if_else(porcentagem == TRUE, .data[[area]]*100, .data[[area]]),
    )
  
  treemap <- treemapify(dados, area = area)
  treemap <- left_join(treemap, dados, by = setdiff(names(dados), area))
  
  paleta_grafico <- paleta[1:nrow(treemap)]

  names(paleta_grafico) <- treemap[["produto"]]
  
  grafico <- ggplot(treemap,
                    aes(
                      area = .data[[area]],
                      fill = .data[[categorizacao]],
                      xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax,
                      tooltip = paste(.data[[categorizacao]],
                                      paste("<br/>",tooltip,"="),
                                      sigla_abrev(.data[[area]], accuracy = if_else(porcentagem == TRUE, 1, 3)),
                                      if_else(porcentagem == TRUE, "%","")
                                      ),
                      label = .data[[categorizacao]]
                    )
    ) +
    geom_treemap() +
    geom_treemap_text(
      grow = TRUE,
      reflow = TRUE,
      place = "topleft",
      colour = "white"
    ) +
    geom_rect_interactive(aes(data_id = .data[[categorizacao]]),alpha = 0.1) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    ) +
    scale_fill_manual(
      values = paleta_grafico
    )
  
  saida <- girafe(
    ggobj = grafico,
    height_svg = if_else(is.null(height), height_svg, height),
    options = list(
      opts_selection(type = "single", css = girafe_css(css = "stroke:blue", line = "stroke:blue")),
      opts_tooltip(),
      opts_hover(css = "")
    )
  )
  
  saida
}


#' Gerar Mapa das Unidades Federativas (UFs)
#'
#' Esta função gera um mapa interativo das Unidades Federativas (UFs) a partir dos dados fornecidos. 
#' O mapa exibe as microrregiões coloridas de acordo com a variável especificada em `fill`.
#'
#' @param dados Um objeto sf contendo os dados geográficos das UFs.
#' @param ufs_selecionadas Um vetor contendo as siglas das UFs a serem exibidas no mapa. Se não for especificado, todas as UFs presentes nos dados serão exibidas.
#' @param fill O nome da coluna que será usada para colorir as UFs no mapa.
#' @param tooltip Uma string contendo o texto que será exibido como tooltip para cada UF.
#' @param titulo O título do mapa.
#' @param bbox Uma lista com as coordenadas do bounding box do mapa. Se não for especificado, o bounding box será calculado automaticamente com base nas UFs selecionadas.
#' 
#' @return Um objeto girafe contendo o mapa interativo das UFs.
#' 
#' @import ggplot2
#' @import ggiraph
#' @importFrom sf st_bbox
#' @importFrom scales label_number
#' @importFrom gghighlight gghighlight
gera_mapa_ufs <- function(dados, ufs_selecionadas = NULL, fill, tooltip, titulo, bbox = NULL){
  
  bbox <- st_bbox(dados %>% filter(abbrev_state %in% ufs_selecionadas))
  
  #dados <- mutate(across )
  #browser()
  grafico <- ggplot(dados) +
    geom_sf_interactive(
      aes(
        fill = .data[[fill]],
        tooltip = paste(.data[['abbrev_state']],
                        paste("<br/>",tooltip,"="),
                        sigla_abrev(.data[[fill]], accuracy = 3)
        ),
        data_id = abbrev_state
      ),
      color = "white"
    ) +
    scale_fill_gradient(low = "#FFB319", high = paleta_epe[1], labels = scales::label_number(accuracy = 0.1, scale_cut =  scales::cut_long_scale())) +
    theme_void() +
    theme(
      legend.position = "null"
    ) +
    gghighlight(
      use_group_by = FALSE,
      abbrev_state %in% ufs_selecionadas | length(ufs_selecionadas) == 0
    ) +
    coord_sf(
      xlim = c((bbox['xmin'] - abs(bbox['xmin']*0.06)), (bbox['xmax'] + abs(bbox['xmax']*0.06))), 
      ylim = c((bbox['ymin'] - abs(bbox['ymin']*0.16)), (bbox['ymax'] + abs(bbox['ymax']*0.16)))
    )
  
  saida <- girafe(
    code = {print(grafico)},
    options = list(
      opts_selection(type = "single", css = girafe_css(css = "stroke:blue", line = "stroke:blue")),
      opts_tooltip(),
      opts_hover(css = "")
    )
  )
  
  saida
}


#' Gerar Mapa das Microrregiões
#'
#' Esta função gera um mapa das microrregiões a partir dos dados fornecidos. 
#' O mapa exibe as microrregiões coloridas de acordo com a variável especificada em `fill`.
#'
#' @param dados Um objeto sf contendo os dados geográficos das microrregiões.
#' @param code_muni_local Um vetor com os códigos dos municípios que determinam as microrregiões a serem exibidas no mapa. Se não for fornecido, todas as microrregiões serão exibidas.
#' @param fill O nome da coluna que será usada para preencher as microrregiões com cores diferentes.
#' @param titulo O título do mapa.
#'
#' @return Um objeto girafe contendo o mapa das microrregiões.
#'
#' @import ggplot2
#' @import ggiraph
#' @importFrom sf st_bbox
#' @importFrom scales label_number
#' @importFrom gghighlight gghighlight
gera_mapa_micros <- function(dados, code_muni_local = NULL, fill, titulo = NULL){
  
  #browser()
  grafico <- ggplot(dados) +
    geom_sf_interactive(
      aes(
        fill = .data[[fill]]
      ),
      size = 0.2,
      color = "white"
    ) +
    scale_fill_gradient(low = "#FFB319", high = paleta_epe[1], labels = scales::label_number(accuracy = 0.1, scale_cut = scales::cut_long_scale())) +
    theme_void() +
    theme(
      legend.position = "null"
    )
    
  #browser()
  if(!is.null(code_muni_local)){
    bbox <- st_bbox(dados %>% filter(code_muni %in% code_muni_local))
    grafico <- grafico +
      coord_sf(
        xlim = c(bbox['xmin'], bbox['xmax']),
        ylim = c(bbox['ymin'], bbox['ymax'])
      ) +
      gghighlight(
        use_group_by = FALSE,
        code_muni %in% code_muni_local
      )
  }
  
  saida <- girafe(
    code = {print(grafico)},
    options = list(
      opts_selection(type = "single", css = girafe_css(css = "stroke:blue", line = "stroke:blue")),
      opts_tooltip(),
      opts_hover(css = "")
    )
  )
  
  saida
}


#' Gerar Mapa dos Municipios
#'
#' Esta função gera um mapa dos municipios a partir dos dados fornecidos. 
#' O mapa exibe os municipios coloridas de acordo com a variável especificada em `fill`.
#'
#' @param dados Um objeto sf contendo os dados geográficos dos mmunicipios.
#' @param code_muni_local Um vetor com os códigos dos municípios que determinam os municipios a serem exibidas no mapa. Se não for fornecido, todas os municipios serão exibidos.
#' @param fill O nome da coluna que será usada para preencher as municipios com cores diferentes.
#' @param titulo O título do mapa.
#' @param tooltip Uma string contendo o texto que será exibido como tooltip para cada municipio.
#'
#' @return Um objeto girafe contendo o mapa dos municipios
#'
#' @import ggplot2
#' @import ggiraph
#' @importFrom sf st_bbox
#' @importFrom scales label_number
#' @importFrom gghighlight gghighlight
gera_mapa_municipios <- function(dados, code_muni_local = NULL, fill, titulo = NULL, tooltip = NULL){
  
  grafico <- ggplot(dados) +
    geom_sf_interactive(
      aes(
        fill = .data[[fill]],
        tooltip = paste(.data[['municipio']],
                        paste("<br/>",tooltip,"="),
                        sigla_abrev(.data[[fill]], accuracy = 3)
        )
      ),
      size = 0.2,
      color = "white"
    ) +
    scale_fill_gradient(low = "#FFB319", high = paleta_epe[1], labels = scales::label_number(accuracy = 0.1, scale_cut = scales::cut_long_scale())) +
    theme_void() +
    theme(
      legend.position = "null"
    )
  
  
  if(!is.null(code_muni_local)){
    bbox <- st_bbox(dados %>% filter(code_muni %in% code_muni_local))
    grafico <- grafico +
      coord_sf(
        xlim = c(bbox['xmin'], bbox['xmax']),
        ylim = c(bbox['ymin'], bbox['ymax'])
      ) +
      gghighlight(
        use_group_by = FALSE,
        code_muni %in% code_muni_local
      )
  }
  
  saida <- girafe(
    code = {print(grafico)},
    options = list(
      opts_selection(type = "single", css = girafe_css(css = "stroke:blue", line = "stroke:blue")),
      opts_tooltip(),
      opts_hover(css = "")
    )
  )
  
  saida
}


#' Geração de gráfico de barras interativo
#'
#' Esta função gera um gráfico de barras interativo com base nos dados fornecidos,
#' onde cada barra representa uma categoria e a altura da barra representa a contagem
#' de uma variável específica.
#'
#' @param dados Um data frame contendo os dados a serem utilizados no gráfico.
#' @param campo_contagem O nome da coluna que contém a variável a ser quantificada para cada categoria.
#' @param y_label Um rótulo opcional para o eixo Y do gráfico. Se não for fornecido, será usado o rótulo padrão.
#' @param tooltip Uma string que define o conteúdo do tooltip exibido quando o usuário passa o mouse sobre as barras.
#'
#' @return Um gráfico de barras interativo.
#'
#' @import dplyr
#' @import ggplot2
#' @import ggiraph
#' @importFrom scales label_number
gera_grafico_barra <- function(dados, campo_contagem, y_label = NULL, tooltip){
  #browser()
  if(length(dados[[1]]) == 0 | sum(dados[[campo_contagem]], na.rm = TRUE)<=0){
    return(ggplot()+theme_void())
  } 
  
  paleta_grafico <- paleta[1:nrow(dados)]
  
  names(paleta_grafico) <- dados[["n3_grupo"]]
  
  grafico <- ggplot(dados, 
                    aes( x = n2_divisao,
                         y = .data[[campo_contagem]],
                         fill = n3_grupo,
                         tooltip = paste(n3_grupo,"<br/>", tooltip, " = ", sigla_abrev(.data[[campo_contagem]])),
                         data_id = n3_grupo
                    )
  ) +
    geom_bar_interactive(stat = "identity") +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1, scale_cut = scales::cut_long_scale())
    ) +
    theme_minimal() +
    labs(x = element_blank(), y = element_blank(), color = "Setor 3") +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 6, face = "bold"),
      legend.position = "none"
    ) +
    scale_fill_manual(values = paleta_grafico)
  
  saida <- 
    girafe(
      code = {print(grafico)},
      width_svg = 3,
      height_svg = 1.5,
      options = list(
        opts_selection(type = "single", css = girafe_css(css = "stroke:blue", line = "stroke:blue")),
        opts_tooltip(),
        opts_hover(css = "")
      )
    )
}


#' Geração de gráfico de linha interativo
#'
#' Esta função gera um gráfico de linha interativo com base nos dados fornecidos,
#' onde cada linha representa uma categoria e os pontos representam os valores dessa categoria ao longo dos anos.
#'
#' @param dados Um data frame contendo os dados a serem utilizados no gráfico.
#' @param campo_contagem O nome da coluna que contém os valores a serem plotados ao longo dos anos.
#' @param y_label Um rótulo opcional para o eixo Y do gráfico. Se não for fornecido, será usado o rótulo padrão.
#'
#' @return Um gráfico de linha interativo.
#'
#' @import dplyr
#' @import ggplot2
#' @import ggiraph
#' @importFrom scales label_number
gera_grafico_linha <- function(dados, campo_contagem, y_label){
  
  dados_grafico <- dados %>%
    group_by(ano) %>% 
    texto_tooltip(categoria = "n3_grupo", info = campo_contagem)
  
  if(length(dados_grafico[[1]]) == 0){
    return(ggplot()+theme_void())
  }
  
  paleta_grafico <- paleta[1:nrow(dados)]
  
  names(paleta_grafico) <- dados[["n3_grupo"]]
  
  grafico <-
    ggplot(dados_grafico, aes(x = ano, y = .data[[campo_contagem]], data_id = ano)) +
    geom_point_interactive(
      aes(
        color = n3_grupo,
        tooltip = texto
      ),
      shape = shape_ponto,
      size = 0.3,
      stroke = 0,
      alpha = 1
    ) +
    geom_line(
      aes(
        color = n3_grupo
      ),
      size = 0.3,
      alpha = alpha_linha
    ) +
    theme_minimal() +
    guides(
      color = guide_legend(override.aes = list(size = 0.5))
    ) +
    theme(
      axis.text = element_text(size = 5, face = "bold"),
      axis.title = element_blank(),
      legend.text = element_text(size = 4, face = "bold"),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = grid::unit(0.05, "lines"),
      legend.box.spacing = unit(0, "pt"),
      legend.margin=margin(0,0,0,0)
    ) +
    labs(x = element_blank(), y = element_blank()) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.1, scale_cut = scales::cut_long_scale())) +
    scale_fill_manual(values = paleta_grafico)
  
  saida <- 
    girafe(
      code = {print(grafico)},
      width_svg = 2.75,
      height_svg = 1,
      options = list(
        opts_selection(type = "single", css = ""),
        opts_tooltip(),
        opts_hover()
      )
    )
}








