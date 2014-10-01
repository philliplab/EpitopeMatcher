library(shiny)
library(EpitopeMatcher)

# Need a mini design for handling the input
# Only read in data once
# In this read, also handle failures and populate a msg variable

shinyServer(function(input, output, session) {

  read_data <- reactive({
    data_sets <- list()
    data_sets[['success']] <- TRUE
    ph <- try(read_patient_hla(input$patient_hla$datapath))
    if (class(ph) == 'try-error') {
      data_sets[['ph']] <- list(msg = attr(ph, 'condition')$message, 
                                data_set = NULL)
      data_sets[['success']] <- FALSE
    } else {
      data_sets[['ph']] <- list(msg = 'File read successfully',
                                data_set = ph)
    }

    ln <- try(read_lanl_hla(input$lanl_hla$datapath))
    if (class(ln) == 'try-error') {
      data_sets[['ln']] <- list(msg = attr(ln, 'condition')$message, 
                                data_set = NULL)
      data_sets[['success']] <- FALSE
    } else {
      data_sets[['ln']] <- list(msg = 'File read successfully',
                                data_set = ln)
    }


    qa <- try(read_query_alignment(input$query_alignment$datapath))
    if (class(qa) == 'try-error') {
      data_sets[['qa']] <- list(msg = attr(qa, 'condition')$message, 
                                data_set = NULL)
      data_sets[['success']] <- FALSE
    } else {
      data_sets[['qa']] <- list(msg = 'File read successfully',
                                data_set = qa)
    }

    return(data_sets)
  })

  output$patient_hla_file_details <- renderPrint(print(read_data()$ph$msg))
  output$lanl_hla_file_details <- renderPrint(print(read_data()$ln$msg))
  output$query_alignment_file_details <- renderPrint(print(read_data()$qa$msg))

  output$patient_hla <- renderTable(read_data()$ph$data_set)
  output$lanl_hla <- renderTable(read_data()$ln$data_set)
  output$query_alignment <- renderPrint(read_data()$qa$data_set)

  epitope_scores <- reactive({
    input$goButton
    isolate({
      if (!read_data()$success){
        return(list(msg = 'No Scores Computed yet'))
      }
      ph <- read_data()$ph$data_set
      ln <- read_data()$ln$data_set
      qa <- read_data()$qa$data_set
      return(score_sequence_epitopes(qa, ph, ln))
    })
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
