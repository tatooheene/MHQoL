library(shiny)
library(tidyverse)
library(DT)
library(writexl)
library(mhqol)
library(fmsb)

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
                    selected = "Netherlands")),

    mainPanel(
      DTOutput("data_output"),

    # Download buttons
    uiOutput("download_buttons")
    )
  ),

tabPanel(title = "The reversed MHQoL Cooker 🔄",
         sidebarPanel(

           # Centered action button at the top
           fileInput("rev_file", "Choose a file (CSV, Excel, RDS)",
                     accept = c(".csv", ".xlsx", ".rds")),


           textOutput("warning_message_rev"),

           h4("Example data"),
           p("MHQoL example data scores:", a(img(src="images/icon-excel.png", height = 24, width = 24), href="example-data/example_data_scores.xlsx", target="_blank"), style="margin-bottom:0"),
           p("MHQoL example data utilities:", a(img(src="images/icon-excel.png", height = 24, width = 24), href="example-data/example_data_utilities.xlsx", target="_blank"), style="margin-bottom:0"),
           hr(),

           radioButtons("input_decision",
                        label = "Input",
                        choices = c("Scores", "Utilities")),


           selectInput("country_decision_rev",
                       label = "Country",
                       choices = "Netherlands",
                       selected = "Netherlands")),

         mainPanel(DTOutput("data_output_rev"),

                   # Download buttons
                   uiOutput("download_buttons_rev")
         )
)
)





server <- function(input, output, session){



  options(shiny.sanitize.errors = FALSE)

  addResourcePath('example-data', here::here("inst/extdata")) # Dit later aanpassen





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
                                   ignore.invalid = FALSE,
                                   ignore.NA = TRUE)



    } else if(input$output_decision == "Utilities"){
    data_mhqol <- mhqol::mhqol(dimensions = data[, 3:9],
                               metric = "total",
                               country = input$country_decision,
                               ignore.invalid = FALSE,
                               ignore.NA = TRUE)

    data_mhqol <- data_mhqol |>
      dplyr::mutate(utility = round(utility, 3))

    }


    # If input$output_decision = "Utilities"

    data <- cbind(descriptives, data_mhqol)


    return(data)

  })

  #  Metric for output calculations
  selected_metric <- reactive({

    selected_metric <- ifelse(input$output_decision == "Scores", "LSS", "utility")

    return(selected_metric)

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



# Second panel to recalculate scores or utilities into dimensions ---------

    rev_data <- reactive({
      req(input$rev_file)


      file_path_rev<- input$rev_file$datapath

      # Read the file based on its extension
      data_rev <- tryCatch({
        if (grepl("\\.csv$", input$rev_file$name)) {
          read_csv(file_path_rev)
        } else if (grepl("\\.xlsx$", input$rev_file$name)) {
          readxl::read_excel(file_path_rev)
        } else if (grepl("\\.rds$", input$rev_file$name)) {
          readRDS(file_path_rev)
        } else {
          return(NULL)
        }
      },error = function(e) return(NULL)  # Return NULL if there's an error
      )

      data_rev <- data_rev |>
        dplyr::select(c("ID", "Group",
                      contains("SI"),
                      contains("IN"),
                      contains("MO"),
                      contains("RE"),
                      contains("DA"),
                      contains("PH"),
                      contains("FU")))

      descriptives_rev <- data_rev |>
        dplyr::select("ID", "Group")




      # Recalculate data into scores/utilities based on the input

      # If input$input_decision = Scores



      if(input$input_decision == "Scores"){
        data_mhqol_rev <- mhqol::mhqol_scores_to_states(scores = data_rev[, 3:9],
                                       retain_old_variables = FALSE)



      } else if(input$input_decision == "Utilities"){
        data_mhqol_rev <- mhqol::mhqol_utilities_to_states(utilities = data_rev[, 3:9],
                                                           country = input$country_decision_rev,
                                                           retain_old_variables = FALSE)

      }


      # If input$output_decision = "Utilities"

      data_rev <- cbind(descriptives_rev, data_mhqol_rev)


      return(data_rev)

    })




    # Warning message if the file is invalid
    output$warning_message_rev <- renderText({
      if (is.null(rev_data())) {
        return("⚠️ Please upload a valid dataframe (CSV, Excel, or RDS).")
      }
      return(NULL)  # No warning if file is valid
    })




    # Render the processed table
    output$data_output_rev <- renderDT({
      req(rev_data())
      datatable(rev_data(), options = list(pageLength = 15))
    })

    # Conditionally show the download buttons when the table is rendered
    output$download_buttons_rev <- renderUI({
      req(rev_data())  # Ensure data exists before showing buttons

      tagList(
        downloadButton("download_rds", "Download as RDS"),
        downloadButton("download_excel", "Download as Excel")
      )
    })

    # Download handler for RDS
    output$download_rds_rev <- downloadHandler(
      filename = function() { "rev_cooked_data.rds" },
      content = function(file_rev) {
        saveRDS(rev_data(), file_rev)
      }
    )

    # Download handler for Excel
    output$download_excel <- downloadHandler(
      filename = function() { "rev_cooked_data.xlsx" },
      content = function(file_rev) {
        writexl::write_xlsx(rev_data(), file_rev)
      }
    )



# For the dinner plate ----------------------------------------------------

    # Get summary stats
    get_summary_stats <- function(selected_var, selected_group) {
      data <- uploaded_data()
      req(data, selected_var, selected_group)


      metric_col <- selected_metric()


      if (selected_var == "Overall" & selected_group == "None") {
        stats <- data %>%
          summarise(Mean = mean(.data[[metric_col]], na.rm = TRUE),
                    SD = sd(.data[[metric_col]], na.rm = TRUE))
      } else if (selected_group != "None" & selected_var == "Overall") {
          stats <- data %>%
            group_by(.data[[selected_group]]) %>%
            summarise(
              Mean = mean(.data[[metric_col]], na.rm = TRUE),
              SD = sd(.data[[metric_col]], na.rm = TRUE)
            )
        } else if (selected_group == "None" & selected_var != "Overall") {
          stats <- data %>%
            summarise(
              Mean = mean(c_across(starts_with(selected_var)), na.rm = TRUE),
              SD = sd(c_across(starts_with(selected_var)), na.rm = TRUE)
            )
    } else if(selected_group != "None" & selected_var != "Overall"){
      stats <- data %>%
        group_by(.data[[selected_group]]) %>%
        summarise(
          Mean = mean(c_across(starts_with(selected_var)), na.rm = TRUE),
          SD = sd(c_across(starts_with(selected_var)), na.rm = TRUE)
        )

        }

      stats %>%
        mutate(across(where(is.numeric), ~ round(.x, input$round_digi)))
    }

    # Show modal when button is clicked
    observeEvent(input$create_plate, {

      showModal(
        modalDialog(
          title = "MHQOL Plate 🍽 (Summary Statistics)",

          # Tabs inside the modal
          tabsetPanel(

            tabPanel("Select Dimension",
                     h4("Choose a Dimension:"),
                     selectInput("dimension_input", "Select a Dimension:",
                                 choices = c("Overall", "SI", "IN", "MO", "RE", "DA", "PH", "FU"),
                                 selected = "Overall"),
                     h4("Split by group?"),
                     selectInput("group_input", "Choose Grouping Variable:", choices = c("None", "Group"), selected = "None")),


            tabPanel("Summary Statistics",
                     h4("Averages & Standard Deviations"),
                     numericInput("round_digi", "Show decimal places:", value = 2),
                     DTOutput("summary_table")),

            tabPanel("Histogram",
                     h4("Distribution of a Selected Dimension or Overall"),
                     sliderInput("bin_width", "Bin Width:", min = 0.1, max = 10, value = 0.5, step = 0.1),
                     plotOutput("histogram_plot"),
                     downloadButton("downloadHist", "Download Histogram as PNG")),

            tabPanel("Density chart",
                     h4("Density of a Selected Dimension or Overall"),
                     plotOutput("density_plot"),
                     downloadButton("downloadDens", "Download Density plot as PNG")),

            tabPanel("Line chart",
                     h4("Line plot of Overall"),
                     plotOutput("line_plot"),
                     downloadButton("downloadLine", "Download Line plot as PNG")),

            tabPanel("Radar chart",
                     h4("Radar of the selected dimension or Overall"),
                     plotOutput("radar_chart"),
                     downloadButton("downloadRadar", "Download Radar Chart as PNG"))
          ),

          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })

    # Output summary statistics
    output$summary_table <- renderDT({

      req(input$dimension_input, input$group_input)

      stats <- get_summary_stats(input$dimension_input, input$group_input)

      datatable(stats, options = list(pageLength = 10))
    })

    # Histogram



    drawHist <- function(){
      req(input$dimension_input)
      data <- uploaded_data()



      metric_col <- selected_metric()


      if(input$group_input == "None" & input$dimension_input == "Overall"){
          ggplot(data, aes(x = .data[[metric_col]], y = ..count../sum(..count..))) +
          geom_histogram(binwidth = input$bin_width, fill = "blue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Histogram of", input$dimension_input),
               x = input$dimension_input,
               y = "Percentage") +
          scale_y_continuous(labels = scales::percent)

      }else if(input$group_input == "Group" & input$dimension_input == "Overall"){
          ggplot(data, aes(x = .data[[metric_col]], y = ..count../sum(..count..))) +
          geom_histogram(binwidth = input$bin_width, fill = "blue", alpha = 0.7) +
          facet_wrap(~Group, ncol = 2) +
          theme_minimal() +
          labs(title = paste("Histogram of", input$dimension_input),
               x = input$dimension_input,
               y = "Percentage") +
          scale_y_continuous(labels = scales::percent)

        }else if(input$group_input == "None" & input$dimension_input != "overall"){
        data_long <- data %>%
          pivot_longer(
            cols = starts_with(input$dimension_input),
            names_to = "variable",
            values_to = "value"
          )

        ggplot(data_long, aes(x = value, y = ..count../sum(..count..))) +
          geom_histogram(binwidth = input$bin_width, fill = "blue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Histogram of", input$dimension_input),
               x = input$dimension_input,
               y = "Percentage") +
          scale_y_continuous(labels = scales::percent)

      }else if(input$group_input == "Group"){
        data_long <- data %>%
          pivot_longer(
            cols = starts_with(input$dimension_input),
            names_to = "variable",
            values_to = "value"
          )
          ggplot(data_long, aes(x = value, y = ..count../sum(..count..))) +
          geom_histogram(binwidth = input$bin_width, fill = "blue", alpha = 0.7) +
          facet_wrap(~Group, ncol = 2) +
          theme_minimal() +
          labs(title = paste("Histogram of", input$dimension_input),
               x = input$dimension_input,
               y = "Percentage") +
          scale_y_continuous(labels = scales::percent)
        }
      }

      output$histogram_plot <- renderPlot({
        drawHist()
      })

        output$downloadHist <- downloadHandler(
          filename = function() {
            paste("histogram_chart_", Sys.Date(), ".png", sep = "")
          },
          content = function(file) {
            png(file, width = 800, height = 800)
            print(drawHist())
            dev.off()
          }
        )


    # Density Plot


    drawDens <- function(){
      req(input$dimension_input)
      data <- uploaded_data()

      metric_col <- selected_metric()

      facet_wrap(~Group, ncol = 2)


      if(input$group_input == "None" & input$dimension_input == "Overall"){
      ggplot(data, aes(x = .data[[metric_col]])) +
        geom_density(fill = "blue", alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Density Plot of", input$dimension_input),
             x = input$dimension_input, y = "Density")
      }else if(input$group_input == "Group" & input$dimension_input == "Overall"){
        ggplot(data, aes(x = .data[[metric_col]])) +
          geom_density(fill = "blue", alpha = 0.5) +
          facet_wrap(~Group, ncol = 2) +
          theme_minimal() +
          labs(title = paste("Density Plot of", input$dimension_input),
               x = input$dimension_input, y = "Density")
      }else if(input$group_input == "None" & input$dimension_input != "Overall"){
        data_long <- data %>%
          pivot_longer(
            cols = starts_with(input$dimension_input),
            names_to = "variable",
            values_to = "value"
          )

        ggplot(data_long, aes(x = value)) +
          geom_density(fill = "blue", alpha = 0.5) +
          theme_minimal() +
          labs(title = paste("Density Plot of", input$dimension_input),
              x = input$dimension_input, y = "Density")

      }else if(input$group_input == "Group" & input$dimension_input != "Overall"){
        data_long <- data %>%
          pivot_longer(
            cols = starts_with(input$dimension_input),
            names_to = "variable",
            values_to = "value"
          )

        ggplot(data_long, aes(x = value)) +
          geom_density(fill = "blue", alpha = 0.5) +
          facet_wrap(~Group, ncol = 2) +
          theme_minimal() +
          labs(title = paste("Density Plot of", input$dimension_input),
              x = input$dimension_input, y = "Density")
      }
    }

    output$density_plot <- renderPlot({
      drawDens()
    })

    output$downloadDens <- downloadHandler(
      filename = function() {
        paste("density_chart_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file, width = 800, height = 800)
        print(drawDens())
        dev.off()
      }
    )




    # Line diagram (Show all lines)
    drawLine <- function(){
      req(input$dimension_input)
      data <- uploaded_data()

      metric_col <- selected_metric()

      data_long <- data %>%
        pivot_longer(
          cols = c(starts_with("SI"),
                   starts_with("IN"),
                   starts_with("MO"),
                   starts_with("RE"),
                   starts_with("DA"),
                   starts_with("PH"),
                   starts_with("FU")),
          names_to = "variable",
          values_to = "value")

      if(input$group_input == "None" & input$dimension_input == "Overall"){
        data_average <- data_long %>%
          group_by(variable) %>%
          summarize(avg_value = mean(value, na.rm = TRUE), .groups = "drop")

        data_average <- data_average %>%
          mutate(variable = factor(variable, levels = c('SI',
                                                        "IN",
                                                        "MO",
                                                        "RE",
                                                        "DA",
                                                        "PH",
                                                        "FU"))) %>%
          arrange(variable)


        data_average <- data_average %>%
          mutate(cum_avg = cummean(avg_value))


        ggplot(data_average, aes(x = variable, y = cum_avg, group = 1)) +
          geom_line(size = 1) +
          geom_point(size = 2) +
          theme_minimal() +
          labs(title = paste("Line plot of overall"),
               x = "Dimensions",
               y = "Cumulative mean")
      }else if(input$group_input == "Group" & input$dimension_input == "Overall"){
        data_average <- data_long %>%
          group_by(Group, variable) %>%
          summarize(avg_value = mean(value, na.rm = TRUE), .groups = "drop")

        data_average <- data_average %>%
          mutate(variable = factor(variable, levels = c('SI',
                                                        "IN",
                                                        "MO",
                                                        "RE",
                                                        "DA",
                                                        "PH",
                                                        "FU"))) %>%
          arrange(variable)


        data_average <- data_average %>%
          group_by(Group) %>%
          mutate(cum_avg = cummean(avg_value)) %>%
          ungroup()

        ggplot(data_average, aes(x = variable, y = cum_avg, color = Group, group = Group)) +
          geom_line(size = 1) +
          geom_point(size = 2) +
          theme_minimal() +
          labs(
            title = paste("Line plot of", input$dimension_input),
            x = input$dimension_input,
            y = "Line"
          )
      }
    }

    output$line_plot <- renderPlot({
      drawLine()
    })

    output$downloadLine <- downloadHandler(
      filename = function() {
        paste("line_chart_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file, width = 800, height = 800)
        print(drawLine())
        dev.off()
      }
    )







    # Radar Chart (Comparing Multiple Dimensions)
    drawRadar <- function(){
      req(uploaded_data())

      metric_col <- selected_metric()
      data <- uploaded_data()


      if(input$group_input == "None" & input$dimension_input == "Overall"){

        library(fmsb)

        # Calculate averages for selected columns
        averages <- data %>%
          select(
            starts_with("SI"),
            starts_with("IN"),
            starts_with("MO"),
            starts_with("RE"),
            starts_with("DA"),
            starts_with("PH"),
            starts_with("FU")
          ) %>%
          summarise(across(everything(), mean, na.rm = TRUE))

        # Calculate maximum values for selected columns
        max_values <- data %>%
          select(
            starts_with("SI"),
            starts_with("IN"),
            starts_with("MO"),
            starts_with("RE"),
            starts_with("DA"),
            starts_with("PH"),
            starts_with("FU")
          ) %>%
          summarise(across(everything(), max, na.rm = TRUE))

        # Calculate minimum values for selected columns (use a different variable name)
        min_values <- data %>%
          select(
            starts_with("SI"),
            starts_with("IN"),
            starts_with("MO"),
            starts_with("RE"),
            starts_with("DA"),
            starts_with("PH"),
            starts_with("FU")
          ) %>%
          summarise(across(everything(), min, na.rm = TRUE))

        # Combine into a single data frame for the radar chart
        radar_data <- rbind(max_values, min_values, averages)
        rownames(radar_data) <- c("Max", "Min", "Average")

        # Plot the radar chart
        radarchart(radar_data,
                   axistype = 1,
                   pcol = rgb(0.2, 0.5, 0.5, 0.9),
                   pfcol = rgb(0.2, 0.5, 0.5, 0.5),
                   plwd = 4,
                   cglcol = "grey", cglty = 1,
                   axislabcol = "grey",
                   caxislabels = seq(min(as.numeric(min_values)), max(as.numeric(max_values)), length.out = 5),
                   cglwd = 0.8,
                   vlcex = 0.8)
      }else if(input$group_input == "Group" & input$dimension_input == "Overall"){
        # Calculate averages for GroupA
        groupA <- data %>%
          filter(Group == "Group A") %>%
          select(
            starts_with("SI"),
            starts_with("IN"),
            starts_with("MO"),
            starts_with("RE"),
            starts_with("DA"),
            starts_with("PH"),
            starts_with("FU")
          ) %>%
          summarise(across(everything(), mean, na.rm = TRUE))

        # Calculate averages for GroupB
        groupB <- data %>%
          filter(Group == "Group B") %>%
          select(
            starts_with("SI"),
            starts_with("IN"),
            starts_with("MO"),
            starts_with("RE"),
            starts_with("DA"),
            starts_with("PH"),
            starts_with("FU")
          ) %>%
          summarise(across(everything(), mean, na.rm = TRUE))

        # Calculate overall maximum values for selected columns
        max_values <- data %>%
          select(
            starts_with("SI"),
            starts_with("IN"),
            starts_with("MO"),
            starts_with("RE"),
            starts_with("DA"),
            starts_with("PH"),
            starts_with("FU")
          ) %>%
          summarise(across(everything(), max, na.rm = TRUE))

        # Calculate overall minimum values for selected columns
        min_values <- data %>%
          select(
            starts_with("SI"),
            starts_with("IN"),
            starts_with("MO"),
            starts_with("RE"),
            starts_with("DA"),
            starts_with("PH"),
            starts_with("FU")
          ) %>%
          summarise(across(everything(), min, na.rm = TRUE))

        # Combine the rows in the order: Max, Min, GroupA, GroupB
        radar_data <- rbind(max_values, min_values, groupA, groupB)

        # Set row names for clarity (first two rows are reserved for scaling)
        rownames(radar_data) <- c("Max", "Min", "GroupA", "GroupB")

        library(fmsb)

        radarchart(radar_data,
                   axistype = 1,
                   # Provide colors only for the groups (ignoring the first two rows)
                   pcol = c("blue", "red"),
                   pfcol = c(rgb(0, 0, 1, 0.4), rgb(1, 0, 0, 0.4)),
                   plwd = c(3, 3),
                   cglcol = "grey", cglty = 1,
                   axislabcol = "grey",
                   caxislabels = seq(min(as.numeric(min_values)), max(as.numeric(max_values)), length.out = 5),
                   cglwd = 0.8,
                   vlcex = 0.8)

        legend("topright",
               legend = c("GroupA", "GroupB"),
               col = c("blue", "red"),
               lwd = 3,
               bty = "n")
      }
    }

    output$radar_chart <- renderPlot({
      drawRadar()
    })

    output$downloadRadar <- downloadHandler(
      filename = function() {
        paste("radar_chart_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        png(file, width = 800, height = 800)
        print(drawRadar())
        dev.off()
      }
    )


}



# Run the application
shinyApp(ui = ui, server = server)
