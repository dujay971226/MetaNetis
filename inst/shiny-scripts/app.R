# ------------------------------------------------------------------------------
# NOTE: This app.R assumes the MetaNetis package is loaded.
# It relies on the availability of package functions (MetabAnalysis, MapToPathway,
# PlotNetwork, GetRefRanges) and package data objects (metab_to_pwys,
# reference_ranges_df).
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# SHINY UI DEFINITION
# ------------------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),

  titlePanel("MetaNetis Interactive Analysis Tool"),

  tags$head(
    tags$style(HTML("
      /* Custom styles for better look */
      .sidebar {
        background-color: #f8f8f8;
        padding: 20px;
        border-right: 1px solid #ddd;
      }
      .main-panel {
        padding: 20px;
      }
      .btn {
        margin-bottom: 10px;
        width: 100%;
        font-weight: bold;
      }
      .description {
        margin-bottom: 20px;
        padding: 15px;
        border: 1px solid #cce5ff;
        background-color: #e6f7ff;
        border-radius: 5px;
      }
      .input-group {
        margin-bottom: 5px;
      }
    "))
  ),

  sidebarLayout(

    # Left Panel (Inputs and Controls)
    sidebarPanel(class = "sidebar",
                 width = 4,

                 div(class = "description",
                     strong("Workflow Controls:"),
                     "Use the buttons below to load data, view reference ranges,
                     and run the analysis. Data should be a CSV with
                     'HMDB_ID' and 'Concentration' columns."
                 ),

                 # Mode switch: match by HMBD ID or name
                 div(style = "display: flex; align-items: center;
                     justify-content: space-between; margin-bottom:
                     20px; padding: 5px 0; border-bottom: 1px solid #ddd;",

                     # 1. Fixed "Match by" Label on the Left
                     div(strong("Match by:")),

                     # 2. Toggle Group: Label 1 | Switch | Label 2
                     div(style = "display: flex; align-items: center;
                         gap: 8px; flex-shrink: 0;",

                         # HMDB ID Label (Initially Active - Default)
                         span(id = "hmdb_label", "HMDB ID",
                              class = "match-label",
                              style = "font-weight: bold; color: #4092C4;"),

                         # Switch Input (compact, without built-in labels)
                         switchInput(
                           inputId = "match_by_switch",
                           label = NULL, # Remove internal label
                           value = FALSE, # Default to OFF (hmbd)
                           onLabel = "",
                           offLabel = "",
                           size = "small",
                           onStatus = "success",
                           offStatus = "info"
                         ),

                         # Metabolite Name Label (Initially Inactive)
                         span(id = "name_label", "Metabolite_Name",
                              class = "match-label",
                              style = "font-weight: normal; color: #6C757D;")
                     )
                 ),

                 # Row 1
                 actionButton("btn_view_ref", "View Reference Range",
                              icon = icon("table")),
                 hr(),

                 # Row 2 (Data Loading/Input)
                 actionButton("btn_upload_data", "Upload Your Own Dataset",
                              icon = icon("upload")),
                 actionButton("btn_try_testset", "Try Testset",
                              icon = icon("vial-virus")),

                 # Hidden inputs for upload
                 div(id = "upload_inputs", style = "margin-top: 15px;",
                     fileInput("file_metabolites",
                               "1. Metabolites Data (CSV)",
                               accept = ".csv"),
                     p(strong("When Uploading you data, make sure it's in .csv
                           format. The 1st row of the csv must be sample
                           names and the 1st column of the csv mush be either
                           titled \"HMDB_ID\" \"or Metabolite_Name\".")),
                     p(strong("Remember to switch the HMDB ID/Metabolite Name button
                           to the corresponding indentifier in your csv")),
                     textInput("input_age",
                               "2. Age Data (comma-sep. or single value)",
                               value = "18"),
                     textInput("input_sample_type",
                               "3. Sample Type (comma-sep. or single value)",
                               value = "Blood/Serum/Plasma"),
                     p(strong("Allowed Sample Types:"),
                       "Blood/Serum/Plasma, Feces, Breast Milk, Urine, CSF, Saliva,",
                       "Cellular Cytoplasm, Sweat, Bile, Pericardial Effusion, Semen,",
                       "Amniotic Fluid, Aqueous Humour, Ascites Fluid, Lymph, Tears"
                     ),
                 ),
                 hr(),

                 # Row 3 (Analysis Run)
                 actionButton("btn_run_analysis", "Run Analysis",
                              icon = icon("play-circle"), class = "btn-primary"),

                 # Display Current Data Status
                 div(style = "margin-top: 20px; padding: 10px; border: 1px dashed #aaa; background-color: white;",
                     strong("Current Sample Status:"),
                     textOutput("data_status")
                 )
    ),

    # Right Panel (Outputs and Display)
    mainPanel(class = "main-panel",
              width = 8,

              # Initial Welcome Text
              div(id = "welcome_text",
                  h3("Welcome to MetaNetis Analysis"),
                  p("MetaNetis takes raw metabolite concentration data and the
                    associated biological context (age and biospecimen type) and
                    benchmarks it against a robust, internally-curated baseline
                    derived from the Human Metabolome Database (HMDB). The core
                    biological data being analyzed are metabolite concentration
                    values (e.g., Lactate or Glucose) and their functional
                    associations with metabolic pathways."),
                  p(strong("How to Use:")),
                  tags$ol(
                    tags$li("Use 'View Reference Range' to inspect
                            the baseline data."),
                    tags$li("Load your data using 'Upload Your Own Dataset'
                            or use the 'Try Testset' for a quick example."),
                    tags$li("After loading data, click 'Run Analysis' to
                            see the metabolite classification,
                            pathway classification and pathway network plot.")
                  )
              ),

              # Dynamic Output Container (for tables, plots, or errors)
              uiOutput("dynamic_output")
    )
  )
)

# ------------------------------------------------------------------------------
# SHINY SERVER DEFINITION
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

  # Reactive values to store state and data
  rv <- reactiveValues(
    metabolite_data = NULL,
    sample_age = 18,
    sample_type = "Blood/Serum/Plasma",
    match_by = "HMDB_ID",
    metab_status = NULL,
    pathway_status = NULL,
    error_message = NULL,
    display_mode = "welcome"
  )

  shinyjs::hide("upload_inputs")

  # --- Dynamic UI Handlers ---

  # Toggle match_by mode and update reactive value
  observeEvent(input$match_by_switch, {
    if (input$match_by_switch) {
      rv$match_by <- "Metabolite_Name"
    } else {
      rv$match_by <- "HMDB_ID"
    }
  })

  # Toggle upload inputs when button is clicked
  observeEvent(input$btn_upload_data, {
    shinyjs::toggle("upload_inputs")
    # Reset to single sample mode
    rv$metabolite_data <- NULL
    rv$metab_status <- NULL
    rv$pathway_status <- NULL
    rv$display_mode <- "welcome"
  })

  # View Reference Range Handler
  observeEvent(input$btn_view_ref, {
    tryCatch({
      # CALLING PACKAGE FUNCTION: GetRefRanges()
      ref_data <- MetaNetis::GetRefRanges()
      output$ref_table_content <- DT::renderDT({
        DT::datatable(
          ref_data,
          options = list(pageLength = 20, autoWidth = TRUE, scrollX = TRUE),
          filter = "top",
          rownames = TRUE
        )
      })

      rv$display_mode <- "reference"
      rv$error_message <- NULL
    }, error = function(e) {
      rv$error_message <- paste(
        "Error accessing Reference Ranges (GetRefRanges()):", e$message)
      rv$display_mode <- "error"
    })
  })

  # Try Testset Handler (Simulates Sample Data)
  observeEvent(input$btn_try_testset, {

    # SIMULATION: This block simulates 10 adult patient data
    # List of HMDB IDs
    hmdb_ids <- c(
      "HMDB0005065", "HMDB0001913", "HMDB0000214", "HMDB0002329", "HMDB0000208",
      "HMDB0013466", "HMDB0013467"
    )

    N_METABOLITES <- length(hmdb_ids)
    N_SAMPLES <- 10
    N_TOTAL_ROWS <- N_METABOLITES * N_SAMPLES

    # 1. Generate random age (18-64 inclusive)
    set.seed(Sys.time()) # Set seed for realism
    sample_age <- sample(18:64, 10)

    # 2. Define Sample Type (fixed value)
    sample_type_text <- "Blood/Serum/Plasma"

    # Create sample IDs (e.g., Test_Sample_1 to Test_Sample_10)
    sample_ids <- paste0("Test_Sample_", 1:N_SAMPLES)

    # Generate the complex data table
    concentration_matrix <- matrix(
      exp(runif(N_TOTAL_ROWS, min = log(1e-4), max = log(1e3))),
      nrow = N_METABOLITES,
      ncol = N_SAMPLES
    )

    # Convert to a data frame and set column names (Sample_IDs) and row names
    # (HMDB_ID)
    test_sample_data_simulated <- as.data.frame(concentration_matrix)
    colnames(test_sample_data_simulated) <- sample_ids
    rownames(test_sample_data_simulated) <- hmdb_ids

    # Update the reactive values
    rv$metabolite_data <- test_sample_data_simulated
    rv$sample_age <- sample_age
    rv$sample_type <- sample_type_text
    rv$match_by <- "HMDB_ID"
    rv$metab_status <- NULL
    rv$pathway_status <- NULL
    rv$error_message <- NULL
    rv$display_mode <- "analysis"

    # Clear the text inputs
    updateTextInput(session, "input_age", value = "")
    updateTextInput(session, "input_sample_type", value = "")

    showNotification("Test dataset loaded successfully. Click 'Run Analysis'.",
                     type = "message")
  })

  # Handle File Upload
  observeEvent(input$file_metabolites, {
    req(input$file_metabolites)
    tryCatch({
      df <- read.csv(input$file_metabolites$datapath)
      if (!(trimws(colnames(df)[1]) %in% c("HMDB_ID", "Metabolite_Name"))) {
        stop("The first column must be titled 'HMDB_ID' or 'Metabolite_Name'.
             Please check your CSV format.")
      }

      # Convert first column to row names
      rownames(df) <- as.character(df[[1]])
      df <- df[, -1, drop = FALSE]

      # Store in reactive values
      rv$metabolite_data <- df
      rv$metab_status <- NULL
      rv$pathway_status <- NULL
      rv$error_message <- NULL
      rv$display_mode <- "analysis"
      showNotification("Metabolite data uploaded successfully.",
                       type = "message")

    }, error = function(e) {
      rv$error_message <- paste("Data Upload Error:", e$message)
      rv$display_mode <- "error"
    })
  })

  # --- Analysis Handler ---

  observeEvent(input$btn_run_analysis, {

    # 1. Check for data
    # Check whether metabolite data exist
    if (is.null(rv$metabolite_data) || nrow(rv$metabolite_data) == 0) {
      rv$error_message <- "Please upload a dataset or use the Testset
                           before running the analysis."
      rv$display_mode <- "error"
      return()
    }

    # Process age input, only if no age info saved
    age_input <- trimws(input$input_age)
    if (nchar(age_input) == 0) {
      rv$sample_age <- 18
    } else {
      age_vec <- suppressWarnings(as.numeric(trimws(strsplit(age_input, ",")[[1]])))
      rv$sample_age <- age_vec
    }

    # Process sample_type, only if no type info saved
    type_input <- trimws(input$input_sample_type)
    if (nchar(type_input) == 0) {
      rv$sample_type <- "Blood/Serum/Plasma"
    } else {
      type_vec <- trimws(strsplit(type_input, ",")[[1]])
      rv$sample_type <- type_vec
    }

    # 2. Run Analysis Pipeline
    tryCatch({

      # Step A: MetabAnalysis (Classification) - CALLING PACKAGE FUNCTION
      metab_status_df <- MetaNetis::MetabAnalysis(
        data_input = rv$metabolite_data,
        age = rv$sample_age,
        sample_type = rv$sample_type,
        match_by = rv$match_by
      )

      # Step B: MapToPathway (Aggregation) - CALLING PACKAGE FUNCTION
      pathway_status_df <- MetaNetis::MapToPathway(
        metab_results = metab_status_df,
        match_by = rv$match_by)

      # Store results
      rv$metab_status <- metab_status_df
      rv$pathway_status <- pathway_status_df
      rv$display_mode <- "analysis"
      rv$error_message <- NULL
      showNotification("Analysis pipeline completed successfully!",
                       type = "message")

    }, error = function(e) {
      rv$error_message <- paste("Analysis Error (Check package functions):",
                                e$message)
      rv$display_mode <- "error"
    })
  })

  # --- Single Plotting Handler: Generates only the plot for the selected sample ---
  output$single_network_plot <- renderPlot({

    req(rv$pathway_status, input$plot_sample_id)

    sample_id_to_plot <- trimws(input$plot_sample_id)

    # 1. Check if data exists and the sample ID is valid
    if (is.null(rv$metabolite_data) || !(sample_id_to_plot %in% colnames(rv$metabolite_data))) {
      # Use an empty ggplot with a message if the ID is invalid
      return(
        ggplot() +
          annotate("text",
                   x = 0.5,
                   y = 0.5,
                   label = paste("Please enter a valid Sample ID.\nAvailable
                                  samples must match the column headers in your
                                  data."),
                   size = 6,
                   color = "#C44040") +
          theme_void()
      )
    }

    # 2. Call the PlotNetwork function
    MetaNetis::PlotNetwork(result = rv$pathway_status,
                           sample_id = sample_id_to_plot)
  })


  # --- Output Renderers ---

  # Display current data status
  output$data_status <- renderText({
    if (is.null(rv$metabolite_data)) {
      "No sample data loaded."
    } else {
      num_samples <- ncol(rv$metabolite_data)
      paste(
        "Loaded:", nrow(rv$metabolite_data), "metabolites in",
        num_samples, "samples.",
        "Match by:", rv$match_by, ".",
        "Age(s):", rv$sample_age,
        "Type(s):", rv$sample_type
      )
    }
  })

  # Input Data Table
  output$input_data <- DT::renderDT({
    req(rv$metabolite_data)
    DT::datatable(
      rv$metabolite_data,
      options = list(pageLength = 15, scrollX = TRUE),
      filter = "top",
      rownames = TRUE
    )
  })

  # Metabolite Analysis Table
  output$metab_table <- DT::renderDT({
    req(rv$metab_status)
    DT::datatable(
      rv$metab_status,
      options = list(pageLength = 15, scrollX = TRUE),
      filter = "top",
      rownames = TRUE
    )
  })

  # Pathway Analysis Table
  output$pwy_table <- DT::renderDT({
    req(rv$pathway_status)
    DT::datatable(
      rv$pathway_status,
      options = list(pageLength = 15, scrollX = TRUE),
      filter = "top",
      rownames = TRUE
    )
  })

  # Render UI to create plotOutput containers for each dynamic plot
  output$dynamic_network_plots <- renderUI({
    req(rv$pathway_status)

    # CRITICAL FIX: Get the unique sample IDs from the metabolite data column names
    req(rv$metabolite_data) # Ensure metabolite data is available
    sample_ids <- colnames(rv$metabolite_data)

    # Create a list of plot containers (title + plotOutput)
    plot_list <- lapply(sample_ids, function(id) {
      tagList(
        div(class = "plot-container",
            tags$h4(paste("Network Analysis for Sample:", id),
                    style = "font-weight: bold; color: #333; margin-bottom: 10px;"),
            # Reference the dynamic output ID created in the observeEvent
            plotOutput(paste0("network_plot_", id), height = "350px")
        )
      )
    })

    # Combine the list of tagLists into a single UI element
    do.call(tagList, plot_list)
  })


  # Main dynamic content renderer
  output$dynamic_output <- renderUI({
    shinyjs::hide("welcome_text")

    if (rv$display_mode == "error") {
      return(
        div(class = "alert alert-danger", role = "alert",
            h4("An Error Occurred!"),
            p(rv$error_message)
        )
      )
    } else if (rv$display_mode == "reference") {
      # The table content is rendered in the observeEvent for error handling
      return(
        div(
          h3("MetaNetis Internal Reference Ranges"),
          DT::DTOutput("ref_table_content")
        )
      )
    } else if (rv$display_mode == "analysis") {

      # Determine the default or current sample ID for the plot input
      default_sample_id <- if (!is.null(rv$metabolite_data) && ncol(rv$metabolite_data) > 0) {
        colnames(rv$metabolite_data)[1]
      } else {
        "Sample_1"
      }

      return(
        tabsetPanel(
          id = "analysis_tabs",
          tabPanel("Input Data", icon = icon("list-ol"),
                   div(style = "margin-top: 20px; overflow-x: auto;",
                       DT::DTOutput("input_data")),
                   p(strong("This shows the input data for analysis."))
          ),
          tabPanel("Metabolite Analysis Table", icon = icon("list-ol"),
                   div(style = "margin-top: 20px; overflow-x: auto;",
                       DT::DTOutput("metab_table")),
                   p(strong("This shows the classification of each metabolite's
                     concentration in each sample. Metabolite concentrations are
                     ranked \"Normal,\" \"High,\" or \"Low\" based on the
                     standardized healthy reference ranges provided by
                     Human Metabolome Database(HMDB). If reference range not
                     available, it will show \"Missing Reference\" instead."))
          ),
          tabPanel("Pathway Analysis Table", icon = icon("list-ol"),
                   div(style = "margin-top: 20px; overflow-x: auto;",
                       DT::DTOutput("pwy_table")),
                   p(strong("This shows the pathway's overall activity level
                     (Hypoactive, Mild Activation, Normal, Mild Inhibition or
                     Hyperactive) based on the sum of metabolite levels."))
          ),
          tabPanel("Metabolite Network Plot", icon = icon("project-diagram"),
                   div(style = "margin-top: 20px; padding: 15px; border:
                       1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
                       fluidRow(
                         column(9,
                                # Sample ID Text Box
                                textInput("plot_sample_id", "Enter Sample ID to Plot:",
                                          value = default_sample_id,
                                          placeholder = "e.g., Sample_1, Patient_A, etc.")
                         ),
                         column(3, style = "padding-top: 25px;", # Align button with text input
                                # New Plot Button
                                actionButton("btn_plot_sample", "Plot",
                                             class = "btn-primary",
                                             style = "width: 100%;")
                         )
                       )
                   ),
                   # Single plot output
                   plotOutput("single_network_plot", height = "600px"),
                   p(strong("This plot shows pathway-level network where each node is
                     a pathway, and an edge connects two pathways if they share at
                     least one metabolite.\nNode color is determined by pathway
                     Net_Score (red = hyperactive, blue = hypoactive,
                     grey = normal)")),
                   p(strong("When testing lots of metabolites, edges will overlap and
                     become less obvious. Could would around it by using
                     MapToPathway() to set alternative Pathway-Metabolite
                     relationships(Not available in browser interface)"))
          )
        )
      )
    } else {
      shinyjs::show("welcome_text")
      return(invisible(NULL))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
