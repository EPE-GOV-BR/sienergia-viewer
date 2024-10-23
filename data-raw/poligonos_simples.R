# Disponibiliza os polígonos simples dos municípios, estados e microrregiões
# como uma base do pacote

library(readr)
poligonos_municipios_simples <-
  read_rds("dados_poligonos_brasil/poligonos_municipios_simples.rds")

poligonos_estados_simples <-
  read_rds("dados_poligonos_brasil/poligonos_estados_simples.rds")

poligonos_microrregiao_simples <-
  read_rds("dados_poligonos_brasil/poligonos_microrregiao_simples.rds")


sf::st_crs(poligonos_municipios_simples) <- 4326

sf::st_crs(poligonos_estados_simples) <- 4326

sf::st_crs(poligonos_microrregiao_simples) <- 4326


usethis::use_data(poligonos_municipios_simples, overwrite = TRUE)

usethis::use_data(poligonos_estados_simples, overwrite = TRUE)

usethis::use_data(poligonos_microrregiao_simples, overwrite = TRUE)