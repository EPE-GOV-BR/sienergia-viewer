# Código para fazer o zip usado no deploy ------------
# ATENÇÃO!
# É importante sempre atualizar os pacotes auxiliares (otimizadorDados e otimizadorLinear) 
# quando for atualizar o sienergiaViewer no servidor.


# Fazer um pull para atualizar o sienergiaViewer localmente
system("git pull")

# Checar se os dados estão na pasta correta -------

## dados_poligonos_brasil/ ----

fs::dir_create("dados_poligonos_brasil")

piggyback::pb_download(
  file = "dados_poligonos_brasil.zip", 
  repo = "curso-r/sienergiaviewer", 
  show_progress = TRUE, 
  tag = "dados-poligonos",
  dest = "."
)

# descompactar pasta
unzip("dados_poligonos_brasil.zip", exdir = "dados_poligonos_brasil/")

# remover zip
file.remove("dados_poligonos_brasil.zip")

# Arquivo do banco de dados ----

if(!file.exists("SIEnergia_dados.sqlite")){
  piggyback::pb_download(
    file = "SIEnergia_dados.sqlite", 
    repo = "curso-r/sienergiaviewer", 
    show_progress = TRUE, 
    tag = "dados",
    dest = "."
  )
}

# Criar arquivo zip
xfun::in_dir("./", {
  zip::zip(
    zipfile = glue::glue("deploy-sienergiaViewer.zip"),
    files = c(
      "app.R",
     # "SIEnergia_dados.sqlite",
      list.files("dados_poligonos_brasil", full.names = TRUE)
    ),
    recurse = FALSE,
    compression = "zip"
  )
})



# Subir para o releases do GitHub

piggyback::pb_upload(
  file = "deploy-sienergiaViewer.zip",
  show_progress = TRUE,
  repo = "curso-r/sienergiaviewer",
  tag = "zip-deploy"
)

# Deletar arquivo zip local
fs::file_delete("deploy-sienergiaViewer.zip")