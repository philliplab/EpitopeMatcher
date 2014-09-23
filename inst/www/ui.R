library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Hello Shiny!"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("patient_hla", label = h3("Patient HLA genotypes")),
      fileInput("lanl_hla", label = h3("LANL HLA genotype database")),
      fileInput("query_alignment", label = h3("Query Alignment"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("File Details",
                 textOutput("patient_hla_file_details"),
                 textOutput("lanl_hla_file_details"),
                 textOutput("query_alignment_file_details")
                 ),
        tabPanel("Patient HLA File", tableOutput("patient_hla")),
        tabPanel("LANL HLA File", tableOutput("lanl_hla")),
        tabPanel("Query Alignment", verbatimTextOutput("query_alignment"))
        )
    )
  )
))
