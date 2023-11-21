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
      verbatimTextOutput("download_status"),
    
    # Testing download functions
      hr(),
      downloadButton("download", "Test download .tsv")
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
      downloadButton("download_volume", label = "Download .tsv"),
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
  
  data_volume <- reactive({
    out <- databraryr::list_sessions(as.numeric(input$volume_id))
    if (!is.data.frame(out)) {
      validate(paste0("'", input$volume_id, "' does not have a session CSV"))
    }
    out
  })
  
  output$download_volume <- downloadHandler(
    filename = function() {
      paste0("volume_", input$volume_id, "_sessions", ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(data_volume(), file)
    }
  )

  output$preview <- renderTable({
    data_volume() |>
      dplyr::select(session_id, session_name, session_date) |>
    head()
  })
  
  data_session <- reactive({
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
    data_session()
  })
  
  observeEvent(input$volume_id, {
    session_choices <- unique(data_volume()$session_id)
    updateSelectInput(inputId = "session_id", choices = session_choices)
  })
  
  session_list <- reactive({
    select(session_table, session_id)
  })
  
  # output$download_status <- reactive({
  #   status$session_csv_download
  # })
  # 
  
  # observeEvent(input$download_session, {
  #   session_csv_fn <- paste0("./vol_", input$volume_id, ".csv")
  #   databraryr::download_session_csv(
  #     vol_id = as.numeric(input$volume_id),
  #     file_name = paste0(as.numeric(input$volume_id), ".csv"),
  #     target_dir = "."
  #   )
  #   status$session_csv_download <-
  #     paste("Downloaded :'", session_csv_fn, "'.")
  # })
  # 
  
  session_table <- reactive({
    session_data() |>
      dplyr::select(session_id, session_name, session_date)
  })
  
  # Testing downloads
  data <- reactive({
    out <- databraryr::list_sessions()
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
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("test_vol_1", ".tsv")
    },
    content = function(file) {
      vroom::vroom_write(data(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
