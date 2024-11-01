# otimizadorViewer

<!-- badges: start -->
<!-- badges: end -->

O pacote otimizadorViewer em R é parte do Módulo de Otimização Energética do SIENERGIA - Sistema de Informações para Energia da EPE. Este Módulo do SIENERGIA estima receitas potenciais da conversão de biomassa residual agropecuária em bioeletricidade e biocombustíveis gasosos e sólidos, indicando alocações viáveis e capacidades de usinas por meio de modelos de otimização linear. As análises são realizadas em nível municipal, característica central do SIENERGIA, e levam em conta custos logísticos de coleta, armazenamento e transporte da biomassa e custos industriais de instalação e operação das usinas, além de restrições de demanda elétrica em baixa tensão na área de concessão passível de atendimento via Micro e Minigeração Distribuída, de vendas municipais de combustível e de combustível sólido para indústrias. Essa abordagem integrada contribui para um planejamento estratégico mais detalhado e eficiente do uso energético da biomassa, revelando as oportunidades viáveis e seus benefícios sociais e ambientais.

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

