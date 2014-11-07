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

  load_substitutionMatrix <- reactive({
    if (input$substitutionMatrix == 'BETWEEN10'){
      data(BETWEEN10)
      return(BETWEEN10)
    }
    if (input$substitutionMatrix == 'BETWEEN38'){
      data(BETWEEN38)
      return(BETWEEN38)
    }
    if (input$substitutionMatrix == 'WITHIN5'){
      data(WITHIN5)
      return(WITHIN5)
    }
    return(input$substitutionMatrix)
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

      # create progress object and ensure that it gets closed.
      progress <- shiny::Progress$new()
      progress$set(message = "Computing: ", value = 0)
      on.exit(progress$close())

      # Create a closure to update progress.
      # Each time this is called:
      # - If `value` is NULL, it will move the progress bar 1/100 of the remaining
      #   distance. If non-NULL, it will set the progress to that value.
      # - It also accepts optional detail text.
      update_progress_bar <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 100
        }
        progress$set(value = value, detail = detail)
      }

      ph <- read_data()$ph$data_set
      ln <- read_data()$ln$data_set
      qa <- read_data()$qa$data_set
      return(match_epitopes(qa, ph, ln, update_progress_bar = update_progress_bar,
                            substitutionMatrix = load_substitutionMatrix()))
    })
  })
                             
  output$epitope_score_status <- renderText(epitope_scores()$msg)
  output$epitope_score_results <- renderTable(epitope_scores()$results)
  output$epitopes_not_in_seq <- renderTable(epitope_scores()$error_log$epitopes_not_in_seq)
  output$no_hla_details <- renderTable(epitope_scores()$error_log$no_hla_details)

  output$help_url <- renderUI({
    help_port <- 5437
    status <- .Call(tools:::startHTTPD, "0.0.0.0", help_port)
    if (status != 0L) {
      return(HTML("<strong>Help system could not be started! Contact package maintainer.<strong>"))
    } else {
      help_link <- paste0('<a href="http://', session$clientData$url_hostname, ':', help_port, 
                          '/library/EpitopeMatcher/html/00Index.html">Click for Detailed Package Help</a>')
      return(HTML(paste(h3('Help for EpitopeMatcher web app'),
                        p("Upload your input files using the menu on the left hand side. The files
                          are required to be in a specific format. You can see more details about
                          the formats in the detailed help of the R package which is available in
                          the link below. The help of the functions read_lanl_hla, 
                          read_patient_hla, and read_query_alignment (should) contain more
                          information about the required input formats."),
                        p("EpitopeMatcher can also be used inside an interactive R session. See
                          the help page for the 'match_epitopes' function for more details/"),
                        p("More information about the output formats can be found in the help page
                          for the score_epitope function."),
                        help_link,
                        p("If the link above does not work, you can access the help by starting
                          an R session and typing help(package = 'EpitopeMatcher') into the
                          prompt and hitting enter"),
                        sep = '\n\n')))
    }
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
