library(shiny)
library(ggplot2)
library(chron)

mem.maxNSize(nsize = Inf)
options(warn = 2, error = recover)

longProcess <- function(wait) {
  start <- Sys.time()
  Sys.sleep(wait)
  end   <- Sys.time()
  data.frame(
    start    = as.character(times(strftime(start, "%H:%M:%S"))),
    end      = as.character(times(strftime(end,   "%H:%M:%S"))),
    duration = paste(round(end - start, 1L), "sec")
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "#vtext { font-weight: bold; font-size: 22px;}"
      )
    )
  ),
  titlePanel("Asynchronous Shiny app"),
  sidebarLayout(
    sidebarPanel(
      br(),
      wellPanel(
        style = "background-color: yellow;",
        textOutput("vtext")
      ),
      br(),
      actionButton(
        "SimulateAsyncProcesses", 
        "Simulate long process.", 
        class = "btn-block"
      ), 
      br(), br(),
      actionButton(
        "GenerateDataToPlot", 
        "Generate plot.",
        class = "btn-block"
      )
    ),
    mainPanel(
      fluidRow(
        column(
          4, 
          tableOutput("ProcessInfo")
        ),
        column(
          8, 
          plotOutput("GeneratedHeatMap", height = "350px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  Text <- reactiveVal(
    "Hello. Click the first button."
  )
  
  output[["vtext"]] <- renderText({
    Text()
  })
  
  ProcessInfo <- reactiveVal()
  
  observeEvent(input[["SimulateAsyncProcesses"]], {
    Text(
      paste(
        "The long process is running but you don't need to wait!",
        "You can generate the plot now."
      )
    )
  })
  
  observeEvent(input[["GenerateDataToPlot"]], {
    Text("Now wait...")
  })
  
  Promise <- eventReactive(input[["SimulateAsyncProcesses"]], {
    ProcessInfo(longProcess(10L))
  })
  
  output[["ProcessInfo"]] <- renderTable({
    req(Promise())
    ProcessInfo()
  })
  
  observeEvent(ProcessInfo(), {
    Text("The long process is done!")  
  })
  
  # plot stuff ####
  DataToPlot <- eventReactive(input[["GenerateDataToPlot"]], {
    matrix(runif(100L), nrow = 10L, ncol = 10L)
  })
  
  output[["GeneratedHeatMap"]] <- renderPlot({
    req(DataToPlot())
    ggplot(iris) + geom_point(aes(x=Sepal.Width, y=Sepal.Length))
  })
  
}


shinyApp(ui = ui, server = server)
