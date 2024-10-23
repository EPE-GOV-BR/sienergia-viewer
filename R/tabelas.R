#' Cria uma tabela Reactable
#'
#' Essa função cria uma tabela Reactable com base nos dados fornecidos.
#' Ela aceita um conjunto de dados dados_tabela como entrada e também oferece a opção de personalizar as colunas e grupos de colunas da tabela, fornecendo as listas columns_list e groups_list, respectivamente.
#' Além disso, é possível definir a cor de fundo dos cabeçalhos da tabela usando o parâmetro background_color (padrão: "rgb(12, 35, 64)").
#' A função também permite ativar a paginação e a ordenação das colunas por meio dos parâmetros pagination e sortable.
#'
#' @param dados_tabela Um conjunto de dados contendo os dados da tabela.
#' @param columns_list Uma lista de colunas personalizadas para a tabela (opcional).
#' @param groups_list Uma lista de grupos de colunas para a tabela (opcional).
#' @param background_color A cor de fundo dos cabeçalhos da tabela (padrão: "rgb(12, 35, 64)").
#' @param pagination Um valor lógico indicando se a paginação deve ser ativada (padrão: FALSE).
#' @param sortable Um valor lógico indicando se a ordenação das colunas deve ser ativada (padrão: FALSE).
#'
#' @return Uma tabela Reactable com as configurações fornecidas.
#'
#' @import dplyr
#' @import reactable
cria_tabela_reactable <- function(
    dados_tabela,
    columns_list = NULL,
    groups_list = NULL,
    background_color = "rgb(12, 35, 64)",
    pagination = FALSE,
    sortable = FALSE){

  tabela <-  dados_tabela %>%
    reactable::reactable(
      defaultColDef = reactable::colDef(
        align = "right",
        html = TRUE,
        format = reactable::colFormat(digits = 0, separators = TRUE)
      ),
      defaultColGroup = reactable::colGroup(
        align = "center",
        headerStyle = list(backgroundColor = background_color, color = "rgb(240,255,255)")
      ),
      columns = columns_list,
      columnGroups = groups_list,
      pagination = pagination,
      sortable = sortable,
      defaultPageSize = 15,
      showPageInfo = FALSE,
      outlined = TRUE,
      bordered = TRUE,
      striped = TRUE,
      compact = TRUE,
      showSortIcon = FALSE,
      width = "auto",
      theme = tema_header_footer_reactable(background_color)
    )

  tabela
}

