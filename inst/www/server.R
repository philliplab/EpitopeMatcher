library(shiny)
library(EpitopeMatcher)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$file_details <- renderPrint(print(input$patient_hla$datapath))
  output$patient_hla <- renderTable(read_patient_hla(input$patient_hla$datapath))
})
