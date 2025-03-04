library(shiny)
library(knitr)
library(markdown)
library(webshot)

ui <- shinyUI(
  fluidPage(
    uiOutput('markdown')
  )
)
server <- function(input, output) {
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit('C:/Users/bende/Documents/R/play/useR_2025_Eclipse_Presentation/rmd/abstract_submission.rmd', quiet = TRUE)))
  })
}

shinyApp(ui, server)