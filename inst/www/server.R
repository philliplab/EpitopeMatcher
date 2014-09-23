library(shiny)
library(EpitopeMatcher)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$patient_hla_file_details <- renderPrint(print(input$patient_hla$datapath))
  output$lanl_hla_file_details <- renderPrint(print(input$lanl_hla$datapath))
  output$query_alignment_file_details <- renderPrint(print(input$query_alignment$datapath))
  output$patient_hla <- renderTable(read_patient_hla(input$patient_hla$datapath))
  output$lanl_hla <- renderTable(read_lanl_hla(input$lanl_hla$datapath))
  output$query_alignment <- renderPrint(print(read_query_alignment(input$query_alignment$datapath)))
})
