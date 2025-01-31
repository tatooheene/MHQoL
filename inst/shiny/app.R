library(shiny)
library(tidyverse)
library(DT)
library(writexl)
library(mhqol)

################################################################
#                       USER INTERFACE                         #
################################################################
ui <-navbarPage(title = "MHQoL",


# First panel to recalculate dimensions into scores or utilities ----------

  tabPanel(title = "The MHQoL Cooker 👨‍🍳",


    sidebarPanel(

      # Centered action button at the top
      div(style = "text-align: center; margin-bottom: 20px;",
          actionButton("create_plate", "Create the Plate 🍽", class = "btn-primary btn-lg")),


        fileInput("file", "Choose a file (CSV, Excel, RDS)",
                accept = c(".csv", ".xlsx", ".rds")),


        textOutput("warning_message"),

        h4("Example data"),
        p("MHQoL example data scores:", a(img(src="images/icon-excel.png", height = 24, width = 24), href="example-data/example_data_scores.xlsx", target="_blank"), style="margin-bottom:0"),
        p("MHQoL example data text:", a(img(src="images/icon-excel.png", height = 24, width = 24), href="example-data/example_data_text.xlsx", target="_blank"), style="margin-bottom:0"),
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

    mainPanel(DTOutput("data_output"),

    # Download buttons
    uiOutput("download_buttons")
    )
  ),

tabPanel(title = "The reversed MHQoL Cooker 🔄")




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

    # If input$output_decision = Scores

    if(input$output_decision == "Scores"){
    data_mhqol <- mhqol::mhqol_LSS(dimensions = data[, 3:9],
                                   metric = "total",
                                   ignore.invalid = input$invalid_decision,
                                   ignore.NA = input$NA_decision)



    } else if(input$output_decision == "Utilities"){
    data_mhqol <- mhqol::mhqol(dimensions = data[, 3:9],
                               metric = "total",
                               country = input$country_decision,
                               ignore.invalid = input$invalid_decision,
                               ignore.NA = input$NA_decision)

    data_mhqol <- data_mhqol |>
      dplyr::mutate(utility = round(utility, 3))

    }


    # If input$ouput_decision = "Utilities"

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

    # Conditionally show the download buttons when the table is rendered
    output$download_buttons <- renderUI({
      req(uploaded_data())  # Ensure data exists before showing buttons

      tagList(
        downloadButton("download_rds", "Download as RDS"),
        downloadButton("download_excel", "Download as Excel")
      )
    })

    # Download handler for RDS
    output$download_rds <- downloadHandler(
      filename = function() { "cooked_data.rds" },
      content = function(file) {
        saveRDS(uploaded_data(), file)
      }
    )

    # Download handler for Excel
    output$download_excel <- downloadHandler(
      filename = function() { "cooked_data.xlsx" },
      content = function(file) {
        writexl::write_xlsx(uploaded_data(), file)
      }
    )


}

# Run the application
shinyApp(ui = ui, server = server)
