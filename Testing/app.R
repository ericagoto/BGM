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
##########################################################

ui<-
  
pageWithSidebar(
  headerPanel("Test"), 
  
sidebarPanel (
  
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
verbatimTextOutput("risco_output")
  
)


)






server <- function (input, output, session){
  
  #Combining data 
  
  selectedData <- reative ({
    
    risco[,c(input$select_angle, input$select_geol, input$select_agua)]
  })
  
  
  
  
  
  
  
  ### Computing Degree of Risk
  
  
  
  ##### Function 01 - Angle 
  Weight_01 <- function(A10, A10_17, A17_30, A_30_60, A_60_90, A90) {
    #angulo
    if (A10) { return(angulo <-  0.5028996)}
    if (A10_17) {return(angulo <- 0.8866914)}
    if (A_17_30) {return(angulo <- 2.0248326)}
    if (A_30_60) {return(angulo <- 4.1555388)}
    if (A_60_90) {return(angulo <- 3.4541262)}
    if (A90) {return(angulo <- 2.2101114)}
    
  }  
  
  
  
  ##### Function 02 - Geologia
  Weight_04 <- function (favoravel, desfav, nao_obs){
    # Geologia
    if (favoravel) { return(geologia <-  1.0030592) }
    if (desfav) { return(geologia <- 2.9995328)}
    
    if (nao_obs) { return(geologia <- 0.819808)
    }
  }
  
  
  ##### Function 03 - Agua
  Weight_08 <- function (servida, conc_chuva, vazamento, fossa) {
    agua = servida*0.9726108 + conc_chuva*0.5842662 + vazamento*1.1650338 + fossa*0.5737704
    return(agua)
  }
  
  
  Risco <- function (agua, geologia, angulo) {
    
    final<- (agua + geologia + angulo)/5
    
    if (final < 7.11) {(return(R1))}
    if (final >= 7.11 & final <12) {(return(R2))}
    if (final >=12 & final >37) {(return(R3))}
    if (final >=37) {(return(R4))}
    
  }
  
  
  
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


