library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Hello Shiny!"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("patient_hla", label = h3("Patient HLA genotypes"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("file_details"),
      tableOutput("patient_hla")
    )
  )
))
