pegar_estados_com_dados <- function() {
  c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
  "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", 
  "RO", "RR", "SC", "SP", "SE", "TO")
}

rotas_nome <- function() {
  c(
    "Densificação - Energia elétrica - Resíduos agrícolas" = "dens",
    "Biodigestão - Combustível - Resíduos agrícolas" = "biocomb",
    "Biodigestão - Energia elétrica - Resíduos agrícolas" = "bioeletr",
    "Biodigestão - Energia elétrica - Resíduos pecuários" = "bioeletr_pec",
    "Biodigestão - Combustível - Resíduos pecuários" = "biocomb_pec",
    "Co-firing - Resíduos agrícolas" = "cofiring"
  )
}

rotas_df <- function() {
  tibble::enframe(rotas_nome(), name = "rota_nome", value = "rota")
}


tabela_padrao <- function(tab, ...) {
  tab |> 
    reactable::reactable(
      striped = TRUE,
      # filterable = TRUE,
      searchable = TRUE,
      sortable = TRUE,
      language = reactable_traducao(),
      theme = reactable::reactableTheme(
        headerStyle = list(
          backgroundColor = "#0c2340",
          color = "white",
          fontWeight = "bold"
        )
      ),
      ...
    )
}

formatar_numero <- function(x, acc = 1) {
  scales::number(x, accuracy = acc, big.mark = ".", decimal.mark = ",")
}

formatar_porc <- function(x, acc = 0.1) {
  scales::percent(x, accuracy = acc, big.mark = ".", decimal.mark = ",")
}

reactable_traducao <- function() {
  reactable::reactableLang(
    searchPlaceholder = "Procurar",
    searchLabel = "",
    noData = "Nenhuma informação encontrada",
    pageNext = "Próxima",
    pagePrevious = "Anterior",
    pageNumbers = "{page} de {pages}",
    pageInfo = "{rowStart}\u2013{rowEnd} de {rows} registros",
    pageSizeOptions = "Mostrar {rows}",
    pageNextLabel = "Próxima página",
    pagePreviousLabel = "Página anterior",
    pageNumberLabel = "Página {page}",
    pageJumpLabel = "Ir para a página",
    pageSizeOptionsLabel = "Registros por página",
    detailsExpandLabel = "Expandir detalhes"
  )
}

pickerInput_opcoes <- function() {
  shinyWidgets::pickerOptions(
    actionsBox = TRUE,
    liveSearch = TRUE, 
    countSelectedText = ,
    selectAllText = "Selecionar todos",
    deselectAllText = "Limpar seleção",
   `none-selected-text` = "Nenhuma opção selecionada",
    `selected-text-format`= "count",
    `count-selected-text` = "{0} opções selecionadas"
  )
}
