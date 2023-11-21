#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Developing Belief Network"),
  
  sidebarLayout(
    # Log in/out
    sidebarPanel(
      # textInput("email", label = h3("Databrary ID"), value = ""),
      # textInput("password", label = h3("Password"), value = ""),
      checkboxInput("use_stored", label = "Stored credentials", value = TRUE),
      actionButton("login", label = "Log in", class = "btn-success"),
      actionButton("logout", label = "Log out", class = "btn-danger"),
      verbatimTextOutput("login_status"),
      
    # Session    
      verbatimTextOutput("download_status")
    ),
    
    # Show selected data
    mainPanel(
      selectInput(
        "volume_id",
        label = h3("Select Volume"),
        choices = list(
          "Volume 1" = 1,
          "Volume 4" = 4,
          "Volume 8" = 8
        ),
        selected = 1
      ),
      downloadButton("download_session", label = "Download .csv"),
      tableOutput("preview"),
              hr(),
              selectInput(
                "session_id",
                label = h3("Select session"),
                choices = NULL
              ),
              tableOutput("this_session_data_table"))
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  status <- reactiveValues(db_login = NULL)
  
  observeEvent(input$logout, {
    result <- databraryr::logout_db()
    if (result) {
      status$db_login <- "Logged out"
    } else {
      status$db_login <- "Login error"
    }
  })
  
  observeEvent(input$login, {
    if (input$use_stored) {
      result <-
        databraryr::login_db(Sys.getenv("DATABRARY_LOGIN"), store = TRUE)
      if (result) {
        status$db_login <- "Logged in"
      } else {
        status$db_login <- "Login failure with stored credentials"
      }
    } else {
      result <- databraryr::login_db(
        email = input$email,
        password = input$password,
        store = TRUE
      )
      if (result) {
        status$db_login <- "Logged in"
      } else {
        status$db_login <- "Login failure with entered credentials"
      }
    }
  })
  
  output$login_status <- reactive({
    status$db_login
  })
  
  output$value <- renderPrint({
    input$checkbox
  })
  
  session_data <- reactive({
    out <- databraryr::get_session_as_df(as.numeric(input$volume_id))
    if (!is.data.frame(out)) {
      validate(paste0("'", input$volume_id, "' does not have a session CSV"))
    }
    out
  })
  
  output$preview <- renderTable({
    session_data() |>
      dplyr::select(session_id, session_name, session_date) |>
    head()
  })
  
  this_session_data <- reactive({
    req(input$session_id)
    out <- databraryr::list_assets_in_session(as.numeric(input$session_id)) |>
      dplyr::select(asset_id, name, mimetype)
    
    if (!is.data.frame(out)) {
      validate(
        paste0(
          "'volume ",
          input$volume_id,
          " session ",
          input$session_id,
          "' is not available."
        )
      )
    }
    out
  })
  
  output$this_session_data_table <- renderTable({
    this_session_data()
  })
  
  observeEvent(input$volume_id, {
    session_choices <- unique(session_data()$session_id)
    updateSelectInput(inputId = "session_id", choices = session_choices)
  })
  
  session_list <- reactive({
    select(session_table, session_id)
  })
  
  output$download_status <- reactive({
    status$session_csv_download
  })
  
  observeEvent(input$download_session, {
    session_csv_fn <- paste0("./vol_", input$volume_id, ".csv")
    databraryr::download_session_csv(
      vol_id = as.numeric(input$volume_id),
      file_name = paste0(as.numeric(input$volume_id), ".csv"),
      target_dir = "."
    )
    status$session_csv_download <-
      paste("Downloaded :'", session_csv_fn, "'.")
  })
  
  output$session_csv_download <- downloadHandler(
    filename = function() {
      paste0("./vol_", input$volume_id, ".csv")
    },
    content = function(file) {
      vroom::vroom_write(session_data(), file)
    }
  )
  
  session_table <- reactive({
    df <- session_data()
    df |>
      dplyr::select(session_id, session_name, session_date)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
