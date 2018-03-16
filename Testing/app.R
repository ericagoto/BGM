################### libraries ###########################
library(shiny)
library (shinydashboard)
library (tidyverse)
library(leaflet)
library(sf)
library(shinythemes)
library(latex2exp)
library (markdown)
library(rmarkdown)
library(knitr)
library(latexpdf)
##########################################################

ui<-
  fluidPage(
pageWithSidebar(
  headerPanel("Test"), 
  
sidebarPanel (
  
  
  
  sliderInput("slider", "Slider", 1, 100, 50),
  ### Input Angulo de Inclinacao
  
  p("Em relação ao ângulo de inclinação(teta) da encosta/talude: "),
  selectInput("select_angle", label = h5("Ângulo de inclinação (em graus):"), choices = list("< 10" = "A10", "entre 10 e 17" = "A10_17","entre 17 e 30" = "A_17_30", "entre 30 e 60" = "A_30_60", "entre 60 e 90" = "A_60_90", "90"="90"), 
              selected = "escolher ângulo de inclinação da encosta"),
  
  
  #### Input 02 - Geologia do Terreno
  
  selectInput("select_geol", label = h5("Geologia do terreno:"), choices = list("favorável à estabilidade" = "favoravel",
                                                                                "desfavorável à estabilidade" = "desfav",
                                                                                "não observada" = "nao_obs"), 
              selected = ""),
  
  ###### Agua no Terreno
  checkboxGroupInput("select_agua", label = h5("Água no terreno:"), choices = list(
    "Lançamento de água servida em superfície " =     "servida", 
    "concentração de água de chuva em superfície" =    "conc_chuva",
    "vazamento da tubulação" = "vazamento",
    " fossa séptica " = " fossa"
    
  ), 
  selected = "presença de água no terreno... ")
  
  
  
),

mainPanel (
  
  
  
  h5("Parametros selecionados"),
  br(), 
  verbatimTextOutput("selectout_angle"), 
  verbatimTextOutput("selectout_geol"), 
  verbatimTextOutput("selectout_agua"), 
  
  tableOutput("values"),
  
  
 h5("Grau de risco do setor:"), 
verbatimTextOutput("risco_output"),


downloadButton("report", "Generate report - HTML"),

downloadButton("report_PDF", "Generate report - PDF")

  
)



)

)




server <- function (input, output){
  
  
  
  output$report <- downloadHandler(
    # For html output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list (n=input$slider,
                      x = input$select_angle, 
                      y = input$select_geol, 
                      i = paste(input$select_agua, collapse=";")
                      )


                      
      
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$report_PDF <- downloadHandler(
    # For html output, change this to "report.pdf"
    filename = "report2.pdf",
    content = function(file2) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report2.Rmd")
      file.copy("report2.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list (n=input$slider,
                      x = input$select_angle, 
                      y = input$select_geol, 
                      i = paste(input$select_agua, collapse=";")
      )
      
      
      
      
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file2,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #Combining data 
  
 # selectedData <- reative ({
    
  #  risco[,c(input$select_angle, input$select_geol, input$select_agua)]
  #})
  
  
  #### Table
  textValues <- reactive({
    
    data.frame ( 
      Caracteristica = c("Angulo", 
                         "Geologia",
                         "Agua"),
                        
      Resposta = as.character(c(
                  input$select_angle,
                   input$select_geol, 
                  paste(input$select_agua, collapse = "; ")
                  )),
  stringsAsFactors = FALSE)
    
    


})





  
  
  
  

#show the values in the HTML table ----
output$values <- renderTable ({
  textValues ()
})
    
      
      
      
      
  
  output$selectout_angle <- renderText({
    paste("Angulo de inclinacao:", as.character(input$select_angle))})
  output$selectout_geol <- renderText({
    paste("Geologia:", as.character(input$select_geol))})
  output$selectout_agua <- renderText({
    paste("Agua no terreno:", as.character(input$select_agua))})
  
  
  
}




# Run the application 
shinyApp(ui, server)


