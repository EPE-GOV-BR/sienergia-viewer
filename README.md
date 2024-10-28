# sienergiaviewer

<!-- badges: start -->
<!-- badges: end -->

Esse pacote contém o Dashboard do SIEnergia com o Módulo Simulador.

## Instalação

Siga os seguintes passos para executar o simulador do SIEnergia.

1.	Clone o seguinte repositório para a sua máquina local: https://github.com/EPE-GOV-BR/sienergia-otimizador 
2.	Instale o pacote “otimizadorLinear” com devtools::install()
3.	Instale o solver gratuito GLPK. Um tutorial pode ser encontrado em: https://youtu.be/iSsIfTYz5Pw?si=fVdcnuvapMi9aeiN 
4.	Instale o pacote “Rglpk” no R. Isso pode ser feito através do comando install.packages("Rglpk")
5.	Clone este repositório (sienergia viewer) para a sua máquina local.
6.	Instale o pacote “otimizadorViewer” com devtools::install()
7.	Baixe a base de dados em: https://www.epe.gov.br/pt/publicacoes-dados-abertos/publicacoes/sienergia 
8.	Descompacte as duas partes da base de dados e cole-a na raiz do projeto do sienergia-viewer.
9.	Abra o arquivo app.R na raiz do projeto sienergia-viewer.
10.	Execute o comando shiny::runApp()
11.	Caso a aplicação não abra automaticamente, cole em seu navegador o endereço apresentado no console do R, no formato “http://...” 

