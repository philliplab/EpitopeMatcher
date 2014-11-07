library(shiny)

shinyUI(fluidPage(

  tags$link(rel = "stylesheet", type = "text/css", href="no_wrap_tables.css"),

  titlePanel("Epitope Matcher"),

  sidebarLayout(
    sidebarPanel(
      fileInput("patient_hla", label = h3("Patient HLA genotypes")),
      fileInput("lanl_hla", label = h3("LANL HLA genotype database")),
      fileInput("query_alignment", label = h3("Query Alignment (FASTA)")),
      selectInput("substitutionMatrix", label = h3("Substitution Matrix"),
                  choices = c('None', 'BLOSUM45', 'BLOSUM50', 'BLOSUM62', 'BLOSUM80', 'BLOSUM100',
                              'PAM30', 'PAM40', 'PAM70', 'PAM120', 'PAM250', 'BETWEEN10',
                              'BETWEEN38', 'WITHIN5'),
                  selected = 'None'),
      actionButton('goButton', 'Compute Scores'),
      downloadButton('download_results', 'Download Results'),
      downloadButton('download_error_log', 'Download Error Log')
    ),

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
        tabPanel("Epitopes Not Found", tableOutput("epitopes_not_in_seq")),
        tabPanel("No Details About HLA", tableOutput("no_hla_details")),
        tabPanel("Help", htmlOutput("help_url"))
        )
    )
  )
))
