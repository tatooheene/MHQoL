library(shiny)
library(tidyverse)
library(DT)
library(mhqol)

################################################################
#                       USER INTERFACE                         #
################################################################
ui <- fluidPage(
  navbarPage(title = "MHQoL"),


# First panel to recalculate dimensions into scores or utilities ----------

  tabPanel(title = "The MHQoL Cooker 👨‍🍳",


    sidebarPanel(
        fileInput("file", "Choose a file (CSV, Excel, RDS)",
                accept = c(".csv", ".xlsx", ".rds")),


        textOutput("warning_message"),

        h4("Example data"),
        p("MHQoL example data scores:", a(img(src="images/icon-excel.png", height = 24, width = 24), href="example-data/example_data_scores.xlsx", target="_blank"), style="margin-bottom:0"),
        hr(),

        radioButtons("output_decision",
                     label = "Output",
                     choices = c("Scores", "Utilities")),

        selectInput("country_decision",
                     label = "Country",
                    choices = "Netherlands",
                    selected = "Netherlands"),

        radioButtons("NA_decision",
                     label = "Take NA's into account",
                     choices = c("TRUE", "FALSE"),
                     selected = "TRUE"),

        radioButtons("invalid_decision",
                     label = "Take invalid cols into account",
                     choices = c("TRUE", "FALSE"),
                     selected = "FALSE")
      ),

    mainPanel(DTOutput("data_output"))
    )



  )

server <- function(input, output, session){



  options(shiny.sanitize.errors = FALSE)

  addResourcePath('example-data', "~/mhqol/inst/extdata") # Dit later aanpassen





  # First panel to recalculate dimensions into scores or utilities ----------


  uploaded_data <- reactive({
    req(input$file)


    file_path <- input$file$datapath

    # Read the file based on its extension
    data <- tryCatch({
      if (grepl("\\.csv$", input$file$name)) {
        read_csv(file_path)
      } else if (grepl("\\.xlsx$", input$file$name)) {
        readxl::read_excel(file_path)
      } else if (grepl("\\.rds$", input$file$name)) {
        readRDS(file_path)
      } else {
        return(NULL)
      }
      },error = function(e) return(NULL)  # Return NULL if there's an error
    )

    data <- data |>
      dplyr::select("ID", "Group", "SI", "IN", "MO", "RE","DA","PH", "FU")

    descriptives <- data |>
      dplyr::select("ID", "Group")




    # Recalculate data into scores/utilities based on the input
    data_mhqol <- mhqol::mhqol(dimensions = data[, 3:9],
                         country = input$country_decision,
                         metric = "total",
                         ignore.invalid = input$invalid_decision,
                         ignore.NA = input$NA_decision,
                         retain_old_variables = TRUE)

    data <- cbind(descriptives, data_mhqol)


    return(data)

  })



    # Warning message if the file is invalid
    output$warning_message <- renderText({
      if (is.null(uploaded_data())) {
        return("⚠️ Please upload a valid dataframe (CSV, Excel, or RDS).")
      }
      return(NULL)  # No warning if file is valid
    })




    # Render the processed table
    output$data_output <- renderDT({
      req(uploaded_data())
      datatable(uploaded_data(), options = list(pageLength = 15))
    })

}

# Run the application
shinyApp(ui = ui, server = server)
