library(shiny)

ui <- fluidPage(
  actionButton(
    "botao",
    label = "Clique",
    icon = icon("plus")
  ) |> bsplus::bs_attach_modal("filtros_rotas"),
  bsplus::bs_modal(
    id = "filtros_rotas",
    title = "Parâmetros das rotas",
    size = "large",
    body = tagList(
      navlistPanel(
        tabPanel(
          title = "Teste",
          textInput(
            inputId = "teste",
            label = "Teste",
            value = "Teste!!"
          )
        )
      )
    )
  ),
  textOutput("texto")
)

server <- function(input, output, session) {

  

  output$texto <- renderText({
    input$teste
  })
}


shinyApp(ui, server, options = list(launch.browser = FALSE))
