#' parametros_externalidades UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_parametros_externalidades_ui <- function(id){
  ns <- NS(id)
tagList(
    fluidRow(
      shinydashboard::tabBox(
        width = 12,
        tabPanel(
          title = "Empregos",
          fluidRow(
            column(
              width = 6,
              shinyWidgets::autonumericInput(
                inputId = ns("fator_empregabilidade_mwh"),
                label = "Fator de empregabilidade por MWh",
                value = NULL,
                min = 0,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                width = "95%",
                decimalPlaces = 10
              ),
              shinyWidgets::autonumericInput(
                inputId = ns("fator_empregabilidade_m3"),
                label = "Fator de empregabilidade por m³",
                value = NULL,
                min = 0,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                width = "95%",
                decimalPlaces = 10
              ),
            ),
            column(
              width = 6,
              shinyWidgets::autonumericInput(
                inputId = ns("fator_empregabilidade_ton"),
                label = "Fator de empregabilidade por tonelada",
                value = NULL,
                min = 0,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                width = "95%",
                decimalPlaces = 10
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              shinyWidgets::autonumericInput(
                inputId = ns("fator_pessoal"),
                label = "Fator de remuneração pessoal",
                value = NULL,
                min = 0,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                width = "95%",
                decimalPlaces = 2
              )
            ),
          ),
          br(),
          fluidRow(
            column(
              width = 12,
              shiny::markdown(
                "Fatores calculados a partir de dados obtidos em [IRENA](https://www.irena.org/Data/View-data-by-topic/Benefits/Renewable-Energy-Employment-by-Country)."
              )
            )
          )
        ),
        tabPanel(
          title = "Consumo de água",
          fluidRow(
            column(
              width = 6,
              shinyWidgets::autonumericInput(
                inputId = ns("fator_consumo_mwh"),
                label = "Fator de consumo de água por MWh",
                value = NULL,
                min = 0,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                width = "95%",
                decimalPlaces = 10
              ),
              shinyWidgets::autonumericInput(
                inputId = ns("fator_consumo_m3"),
                label = "Fator de consumo de água por m³",
                value = NULL,
                min = 0,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                width = "95%",
                decimalPlaces = 10
              ),
            ),
            column(
              width = 6,
              shinyWidgets::autonumericInput(
                inputId = ns("fator_consumo_ton"),
                label = "Fator de consumo de água por tonelada",
                value = NULL,
                min = 0,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                width = "95%",
                decimalPlaces = 10
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 12,
              shiny::markdown("Fatores calculados a partir de dados obtidos em: [Conde (2013, p. 63)](https://www.ppe.ufrj.br/images/publica%C3%A7%C3%B5es/mestrado/Marcos_Ribeiro_Conde.pdf).")
            )
          )
        ),
    tabPanel(
          title = "Emissões",
          fluidRow(
            column(
              width = 6,
              shinyWidgets::autonumericInput(
                inputId = ns("fator_emissoes_mwh"),
                label = "Fator de emissões por MWh",
                value = NULL,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                width = "95%",
                decimalPlaces = 10
              ),
              shinyWidgets::autonumericInput(
                inputId = ns("fator_emissoes_m3"),
                label = "Fator de emissões por m³",
                value = NULL,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                width = "95%",
                decimalPlaces = 10
              ),
            ),
            column(
              width = 6,
              shinyWidgets::autonumericInput(
                inputId = ns("fator_emissoes_ton"),
                label = "Fator de emissões por tonelada",
                value = NULL,
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                width = "95%",
                decimalPlaces = 10
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 12,
              shiny::markdown("Fatores calculados a partir de dados obtidos em [IRENA](https://www.irena.org/Data/View-data-by-topic/Climate-Change/Avoided-Emissions-Calculator).")
            )
          )
        )   
      )
    )
  )
}
    
#' parametros_externalidades Server Functions
#'
#' @noRd 
mod_parametros_externalidades_server <- function(id, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # options(scipen = 9999)
    fatores_externalidades <- dplyr::tbl(con, "fatores_externalidades") |> 
      dplyr::collect()
    
        
    shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "fator_pessoal",
      value = unique(fatores_externalidades$fator_pessoal)[1]
    )
    
    f_e_bioenergia <- fatores_externalidades |> 
      dplyr::filter(fonte_energia == "bioenergia")
    
      f_e_briquete <- fatores_externalidades |> 
      dplyr::filter(fonte_energia == "briquete")
      
      f_e_biocombustivel <- fatores_externalidades |> 
      dplyr::filter(fonte_energia == "biocombustivel")
      
      
   
      # Atualizando os inputs - MWh
    shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "fator_empregabilidade_mwh",
      value = f_e_bioenergia$empregos_por_un
    )

    
    shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "fator_consumo_mwh",
      value = f_e_bioenergia$consumo_agua_m3_por_un
    )
    
      shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "fator_emissoes_mwh",
      value = f_e_bioenergia$toneladas_co2_eq_por_un
    ) 
      
        # Atualizando os inputs - M3
    shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "fator_empregabilidade_m3",
      value = f_e_biocombustivel$empregos_por_un
    )
    
    shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "fator_consumo_m3",
      value = f_e_biocombustivel$consumo_agua_m3_por_un
    )
    
      shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "fator_emissoes_m3",
      value = f_e_biocombustivel$toneladas_co2_eq_por_un
    ) 
    

    # Atualizando os inputs - Tonelada
    shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "fator_empregabilidade_ton",
      value = f_e_briquete$empregos_por_un
    )

    shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "fator_consumo_ton",
      value = f_e_briquete$consumo_agua_m3_por_un
    )
    
      shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "fator_emissoes_ton",
      value = f_e_briquete$toneladas_co2_eq_por_un
    )
      
      
    

    parametros_externalidades <- reactive({
      # req(input$fator_empregabilidade_mwh)
      # req(input$fator_empregabilidade_m3)
      # req(input$fator_empregabilidade_ton)
      
      # O valor do input não está vindo!! pq?
      
      parametros_externalidades <- list(
        fator_empregabilidade_mwh = input$fator_empregabilidade_mwh,
        fator_empregabilidade_m3 = input$fator_empregabilidade_m3,
        fator_empregabilidade_ton = input$fator_empregabilidade_ton,
        
        fator_pessoal = input$fator_pessoal,
        
        fator_consumo_mwh = input$fator_consumo_mwh,
        fator_consumo_m3 = input$fator_consumo_m3,
        fator_consumo_ton = input$fator_consumo_ton,
        
        fator_emissoes_mwh = input$fator_emissoes_mwh,
        fator_emissoes_m3 = input$fator_emissoes_m3,
        fator_emissoes_ton = input$fator_emissoes_ton
      )
      return(parametros_externalidades)
    })

     return(parametros_externalidades)
  })
}
    
## To be copied in the UI
# mod_parametros_externalidades_ui("parametros_externalidades_1")
    
## To be copied in the server
# mod_parametros_externalidades_server("parametros_externalidades_1")
