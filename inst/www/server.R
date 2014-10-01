library(shiny)
library(EpitopeMatcher)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  output$patient_hla_file_details <- renderPrint(print(input$patient_hla$datapath))
  output$lanl_hla_file_details <- renderPrint(print(input$lanl_hla$datapath))
  output$query_alignment_file_details <- renderPrint(print(input$query_alignment$datapath))

  output$patient_hla <- renderTable(read_patient_hla(input$patient_hla$datapath))
  output$lanl_hla <- renderTable(read_lanl_hla(input$lanl_hla$datapath))
  output$query_alignment <- renderPrint(print(read_query_alignment(input$query_alignment$datapath)))

  epitope_scores <- reactive({
    ph <- try(read_patient_hla(input$patient_hla$datapath))
    if (class(ph) == 'try-error') {return(list(msg = 'Invalid Patient HLA file'))}
    ln <- try(read_lanl_hla(input$lanl_hla$datapath))
    if (class(ln) == 'try-error') {return(list(msg = 'Invalid LANL HLA file'))}
    qa <- try(read_query_alignment(input$query_alignment$datapath))
    if (class(qa) == 'try-error') {return(list(msg = 'Invalid query alignment'))}
    score_sequence_epitopes(qa, ph, ln)
  })
                             
  output$epitope_score_status <- renderText(epitope_scores()$msg)
  output$epitope_score_results <- renderTable(epitope_scores()$results)
  output$epitope_score_error_log <- renderTable(epitope_scores()$error_log)

  output$help_url <- renderText({
    if (tools:::httpdPort == 0L) 
      tools::startDynamicHelp()
    if (tools:::httpdPort <= 0L)
      return("help system could not be started")
    else
      return(paste0("http://", session$clientData$url_hostname, ":", tools:::httpdPort, 
        "/library/EpitopeMatcher/html/00Index.html"))
  })

  output$download_results <- downloadHandler(
    filename = function() {'results.csv'},
    content = function(file){
      write.csv(epitope_scores()$results, file, row.names = FALSE)
    }
  )

  output$download_error_log <- downloadHandler(
    filename = function() {'error_log.csv'},
    content = function(file){
      write.csv(epitope_scores()$error_log, file, row.names = FALSE)
    }
  )
})
