library(shiny)
library(DT)
library(dplyr)
library(readr)
library(here)
library(shinyWidgets)

# === Load preprocessed comparison data ===
data_file <- here::here("Haralson_County_Final", "data", "FQHC_evaluation_data.csv")
comparison_df <- read_csv(data_file, show_col_types = FALSE)

# Ensure 'Measure' column exists
if (!"Measure" %in% names(comparison_df)) {
  comparison_df$Measure <- comparison_df$Disparity
}

# Add editable FQHC columns if not present
if (!"FQHC Progress (%)" %in% names(comparison_df)) {
  comparison_df$`FQHC Progress (%)` <- 0
}

# === UI ===
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
  body { background-color: #4D7952; color: white; }

  .selectize-input {
    background-color: #d9d9d9 !important;
    color: black !important;
    border-radius: 4px;
  }

  .selectize-dropdown, .selectize-dropdown-content {
    background-color: #f0f0f0 !important;
    color: black !important;
  }

  table.dataTable thead {
    background-color: #4D7952;
    color: white;
  }

  table.dataTable tbody {
    background-color: #C3D3C6;
    color: black;
  }

  table.dataTable tbody tr:hover {
    background-color: #8BB4C7 !important;
  }

  .dataTables_wrapper .dataTables_paginate .paginate_button {
    color: black !important;
    background: none !important;
    border: none !important;
  }

  .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
    color: #000 !important;
    text-decoration: underline;
  }

  .dataTables_wrapper .dataTables_paginate .paginate_button.current {
    background-color: #03787d !important;
    color: white !important;
    font-weight: bold;
  }

  .shiny-input-container input:focus,
  .shiny-input-container select:focus {
    outline: 2px solid #fcd116;
    box-shadow: 0 0 10px #fcd116;
  }

  .shiny-input-container label[for='disparity_filter'] {
    color: #d3d3d3 !important;
  }
"))
    
  ),
  
  titlePanel("\U0001F4CA Haralson Health Collective Disparities KPI Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("disparity_filter", "Filter by Disparity:",
                  choices = c("All", unique(comparison_df$Disparity)), selected = "All"),
      helpText("Compare Haralson County's rates with GA, US, and track your own FQHC performance.")
    ),
    mainPanel(
      h4("Comparative KPI Table with Progress Bars"),
      DTOutput("comparison_dt"),
      br(),
      uiOutput("progress_bars"),
      br(),
      h4("Key Takeaways"),
      p("This dashboard tracks the progress of your FQHC toward achieving health equity goals. Use it to assess disparities, adjust internal targets, and guide improvement efforts."),
      br(),
      p(strong("Why this matters:"),
        "This tool helps rural health leaders track disparities, guide care coordination, and align with Healthy People 2030.")
    )
  )
)

# === Server ===
server <- function(input, output, session) {
  # Reactive comparison_df so updates propagate
  reactive_df <- reactiveVal(comparison_df)
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- reactive_df()
    if (input$disparity_filter == "All") {
      df
    } else {
      df %>% filter(Disparity == input$disparity_filter)
    }
  })
  
  # Render editable DT table
  output$comparison_dt <- renderDT({
    datatable(
      filtered_data(),
      editable = list(target = 'cell'),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 't<"dt-footer"ip>'
      ),
      class = 'stripe hover cell-border row-border compact'
    )
  }, server = FALSE)
  
  # Render progress bars
  output$progress_bars <- renderUI({
    df <- filtered_data()
    tagList(
      lapply(1:nrow(df), function(i) {
        progress <- df$`FQHC Progress (%)`[i]
        target <- df$`HP2030 Target`[i]
        gap <- target - progress
        color <- ifelse(is.na(progress), "gray",
                        ifelse(gap <= 5, "success",
                               ifelse(gap <= 15, "warning", "danger")))
        
        tagList(
          tags$strong(df$Measure[i]),
          progressBar(
            id = paste0("bar_", i),
            value = progress,
            total = target,
            status = color,
            display_pct = TRUE
          ),
          p(em("Gap to target:"), paste0(round(gap, 1), "%"))
        )
      })
    )
  })
  
  # Update reactive_df on DT cell edit
  observeEvent(input$comparison_dt_cell_edit, {
    info <- input$comparison_dt_cell_edit
    df <- reactive_df()
    filtered_df <- filtered_data()
    
    i <- info$row
    j <- info$col
    colname <- names(filtered_df)[j + 1]  # adjust for JS indexing
    val <- as.numeric(info$value)
    
    match_row <- which(df$Measure == filtered_df$Measure[i] & df$Disparity == filtered_df$Disparity[i])
    df[match_row, colname] <- val
    reactive_df(df)  # update reactive value
  })
}

# === Run App ===
shinyApp(ui, server)
