library(shiny); library(DT)
library(tidyverse); library(shinythemes) ;library(caret); library(ranger)

load("Best-Subset-Random-Forest.RData", .GlobalEnv)

# Define UI 
ui <- fixedPage(
  theme = shinytheme("readable"),
  # Application title
  titlePanel(h1("Original Scraper Mass Calculator v.1.0.0", align = "center")),
  hr(),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 4,
      # Input: Select a file ----
      fileInput("file1", "Upload csv file here",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Button
      downloadButton("downloadData", "Download the Predictions"),
      
      hr(),
      hr(),
      
      # Help files (pdf and sample CSV)
      actionButton("pdf", "Help/User guide", onclick = "window.open('Help.pdf')", align = "center"),
      hr(),
      downloadButton(outputId = "sampleCSV",label =  "Sample CSV")
    ),
    
    # Show the table with the predictions
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Table of Predictions",
          DT::dataTableOutput("mytable")),
        
        tabPanel(
          "Original Scraper Mass Calculator",
          hr(),
          numericInput(inputId = 're.mass', 
                       label = 'Enter remaining scraper mass in g.', 
                       min = 0.01, max = 300, value = NA),
          numericInput(inputId = 'mean.t', 
                       label = 'Enter average height of retouch in mm.', 
                       min = 0.01, max = 40, value = NA),
          numericInput(inputId = 'max.thick', 
                       label = 'Enter flake maximum thickness in mm.', 
                       min = 0.01, max = 100, value = NA),
          numericInput(inputId = 'GIUR.in', 
                       label = 'Enter average GIUR of the scraper', 
                       min = 0.01, max = 1, value = NA, step = 0.1),
          
          actionButton("calculate_mass", "Calculate original mass"),
          hr(),
          textOutput("mass.pred"),
          textOutput("curated.pred")
        )
      )
    )
  )
)

# SERVER 
server <- function(input, output) {
  
  sample.csv <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(sample.csv) <- c("Rem.Weight", "Max.thick", "Mean.t", "GIUR")
  
  output$sampleCSV <- downloadHandler(
    filename = function() {
      paste("Sample csv", ".csv")
    },
    content = function(file) {
      write.csv(sample.csv, file)
    }
  )
  
  #### Batch processing section ####
  # input the data and make predictions
  reactiveDF <- reactive({
    
    req(input$file1)
    DFcsv <- read.csv(input$file1$datapath)
    
    DFcsv2 <- data.frame(
      Rem.Weight = DFcsv$Rem.Weight,
      Mean.t = DFcsv$Mean.t,
      Log.M.Thick = round(log10(DFcsv$Max.thick),3),
      GIUR = DFcsv$GIUR
    )
    
    Original.Mass <- round(predict(RF_Model.BS, DFcsv2), 3)
    Percent.Consumed <- round((100 - (DFcsv2$Rem.Weight/Original.Mass)*100), 3)
    
    DFcsv <- cbind(DFcsv2, Original.Mass, Percent.Consumed)
    
  })
  
  # Render the table of predictions
  output$mytable = DT::renderDataTable({
    req(input$file1)
    DT::datatable(reactiveDF(),  options = list(pageLength = 100), filter = c("top"))
  })
  
  # Downloadable csv of selected dataset 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactiveDF(), file, row.names = FALSE)
    }
  )
  
  #### Individual calculation section ####
  # Organize inputs into a data frame
  rval_df <- eventReactive(input$calculate_mass, {
    data.frame(
      Rem.Weight = input$re.mass,
      Mean.t = input$mean.t,
      Log.M.Thick = log10(input$max.thick),
      GIUR = input$GIUR.in)
  })
  
  # Make predictions of mass (reacts when pressing the calculate mass boton)
  rval_pred <- eventReactive(input$calculate_mass, {
    RF_Model.BS <- readRDS("Random-Forest-Model.RDS")
    predict(RF_Model.BS, rval_df())
  })
  
  rval_curated <- eventReactive(input$calculate_mass,{
    (100 - (input$re.mass/rval_pred())*100)
  })
  
  output$mass.pred <- renderText({
    paste("Original mass of the scraper:", round(rval_pred(), 2), "g.")
  })
  
  output$curated.pred <- renderText({
    paste("The scraper has been consumed a", round(rval_curated(),2), "%")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)