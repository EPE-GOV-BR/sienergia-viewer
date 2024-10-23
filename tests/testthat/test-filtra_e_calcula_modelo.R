

test_that("filtra e calcula modelo works", {
  
  database <- list()
  
  database$con <- connect_database()
  database$tbl_flat_produto <- cria_tbl_flat_produto(database$con)
  database$tbl_flat_energetico <- cria_tbl_flat_energetico(database$con)
  database$tbl_flat_combustivel <- cria_tbl_flat_combustivel(database$con)
  database$tbl_flat_produto_uf <- cria_tbl_flat_produto_uf(database$con)
  database$tbl_flat_modelo <- cria_tbl_flat_combustivel_modelo(database$con)

  
  resultado <- filtra_e_calcula_modelo(
    database = database,
    produtos_selecionados = NULL,
    setores_selecionados = NULL,
    subsetores_selecionados = NULL,
    subsubsetores_selecionados = NULL,
    inicio_periodo = 20190000,
    fim_periodo = 20200000,
    ufs_selecionadas_local = NULL,
    porcentagem_combustao = 50,
    rotas_selecionadas = c("Combustão", "Biodigestão", "Rotas Mescladas"),
    minimo_diario = 50
  )
  
})
