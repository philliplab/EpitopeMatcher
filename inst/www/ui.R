library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Epitope Matcher"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("patient_hla", label = h3("Patient HLA genotypes")),
      fileInput("lanl_hla", label = h3("LANL HLA genotype database")),
      fileInput("query_alignment", label = h3("Query Alignment")),
      downloadButton('download_results', 'Download Results'),
      downloadButton('download_error_log', 'Download Error Log')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("File Details",
                 h5("Patient HLA File:"),
                 textOutput("patient_hla_file_details"),
                 h5("LANL HLA File:"),
                 textOutput("lanl_hla_file_details"),
                 h5("Query Alignment File:"),
                 textOutput("query_alignment_file_details"),
                 h5("Epitope score computation status:"),
                 textOutput("epitope_score_status")
                 ),
        tabPanel("Patient HLA File", tableOutput("patient_hla")),
        tabPanel("LANL HLA File", tableOutput("lanl_hla")),
        tabPanel("Query Alignment", verbatimTextOutput("query_alignment")),
        tabPanel("Results", tableOutput("epitope_score_results")),
        tabPanel("Error Log", tableOutput("epitope_score_error_log")),
        tabPanel("Help", textOutput("help_url"))
        )
    )
  )
))
