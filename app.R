################# MY BGM Project ##################



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



##########################################################




################## DATASET ##################################






############################ UI ######################################################                                                                        


ui <-
  
  
  
  
  
  
  navbarPage( 
    
    #### App title --- 
    "Mapeamento de Areas e Risco",
    
    
    ### theme selector
    tabPanel("Diagnostico", 
             
             shinythemes::themeSelector(),
             
             
             # Application title
             titlePanel(h2("Diagnóstico para mapeamento de Áreas de Risco de Escorregamento Raso")),
             
             ############################################ inicio Roteiro Intrucoes  ########################################################  
             p("A) Este roteiro objectiva auxiliar a tomada de decisão sobre as moradias que estão sob risco de escorregamentos."),
             p("B) Ao final do preenchimento o grau de risco será computado automaticamente."),
             p("C) O preenchimento deve ser feito passo-a-passo. Para cada passo existem instruções que devem ser lidas com atenação. Nos espaços em branco preencher as informações solicitadas. "),
             p("D) Converse com os moradores das casas e vizinhos. As pessoas tÊm a tendência de tentar esconder fatos, pensando nos problemas que uma remoção pode lhes causar. Quando for possível pergunte para crianças. "),
             br(),
             ######################################### termino Roteiro Instrucoes ########################################################
             
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               
               
               sidebarPanel(
                 
                 
                 textInput("txt_mun", "Municipio:", "nome do municipio"), 
                 textInput("txt_area", "Area:", " nome da area"), 
                 textInput("txt_bairro", "Bairro:", "Bairro"), 
                 textInput("txt_setor", "Setor:", "Setor"),
                 textInput("txt_equip", "Equipe:", "participantes da equipe"),
                 dateInput("date",
                           label = 'Date input: yyyy-mm-dd',
                           value = Sys.Date()),
                 selectInput("select_UA", label = h5("Unidade de analise"), 
                             choices = list("Deslizamento" = "Deslizamento", "Solapamento" = "Solapamento"), 
                             selected = "Deslizamento")
               ),
               # Show MainPanel
               mainPanel(
                 
                 
                 ################## CARACTERISTICAS DA AREA #####################    
                 
                 h3("Características da Área:"),
                 h4("Instruções:"),
                 p("Este campo deve ser preenchido com cuidado, pois deverá permitir que qualquer pessoa possa chegar/retornar ao local. Colocar a localização/endereço das moradias- usar nome ou número da rua, viela, escadaria, ligação de água ou luz, nomes de vizinhos, nomes de moradores e as condições de acesso à area, como por examploÇ via de terra, escadaria de cimento, rua asfaltada, boas ou más condições, etc. Mencionar os tipos de moradias"),
                 
                 
                 
                 textInput("txt_loc", "Localizacao: (rua/viela/escadaria,etc)", ""), 
                 
                 textInput("txt_moradores", "Nome de moradores: (nome de moradores para referencia/contato)", ""),
                 
                 textInput("txt_acesso", "Condicoes de Acesso a area (descreva condicoes de acesso da area):", ""),
                 selectInput("select_moradia", label = h5("Tipos predominante de  construcao:"), 
                             choices = list("Alvenaria" = "Alvenaria", "Madeira" = "Madeira", "Mista" = "mista (alvenaria/madeira/outros materiais"), 
                             selected = ""),
                 
                 textInput("txt_moradia", "Obs:", ""),
                 selectInput("select_densidade", label = h5("Densidade de ocupacao:"), 
                             choices = list("1" = "1", "2" = "2", "3" = "3", "4"="4"), 
                             selected = ""),
                 
                 selectInput("select_via", label = h5("Condicao da via:"), 
                             choices = list("pavimentada" = "pavimentada", "nao pavimentada" = "nao pavimentada"), 
                             selected = ""),
                 
                 textInput("txt_via", "Obs:", ""),
                 
                 
                 
                 ########### fim caracteristica area ###################
                 
                 
                 
                 ############inicio condiciionantes ############################
                 h3("Condicionantes"),
                 h4("Instruções:"),
                 p("Descrever o terreno onde estão as moradias. Utilize o desenho no primeiro quadro como referência para as condições encontradas. Antes de preencher dê um passeio no entorno das moradias. Olhe com atenção os barrancos (taludes) e suba neles se for necessário."),
                 br(),
                 
                 selectInput("select_encosta", label = h5("Tipos de encosta:"), 
                             choices = list("Encosta Natural" = "Encosta Natural", "Talude de corte" = "Talude de corte"), 
                             selected = "Encosta Natural"),   
                 textInput("txt_h_max", "Altura maxima (m):", "preencher em metros"), 
                 
                 textInput("txt_dist_moradia", "Distancia moradia em relação encosta/talude m):", "preencher em metros"),
                 
                 p("Em relação ao ângulo de inclinação(teta) da encosta/talude: "),
                 selectInput("select_angle", label = h5("Ângulo de inclinação (em graus):"), choices = list("< 10" = "< 10", "entre 10 e 17" = "entre 10 e 17","entre 17 e 30" = "entre 17 e 30", "entre 30 e 60" = "entre 30 e 60", "entre 60 e 90" = "entre 60 e 90", "90"="90"), 
                             selected = "escolher ângulo de inclinação da encosta"),
                 
                 selectInput("select_posicao_moradia", label = h5("Posição da moradia:"), choices = list(
                   "Próxima a base da encosta" ="Próxima a base da encosta",          "Distante da base da encosta" =  "Distante da base da encosta",
                   "Próxima ao topo da encosta" ="Próxima ao topo da encosta",          "Distante do topo da encosta" =  "Distante do topo da encosta",
                   "No meio da encosta" =  "No meio da encosta", 
                   "Na encosta" = "Na encosta"
                 ), 
                 selected = "escolha posição da moradia"),
                 
                 selectInput("select_geol", label = h5("Geologia do terreno:"), choices = list("favorável à estabilidade" = "favorável à estabilidade",
                                                                                               "desfavorável à estabilidade" = "desfavorável à estabilidade",
                                                                                               "não observada" = "não observada"), 
                             selected = ""),
                 
                 
                 
                 selectInput("select_predominante", label = h5("Material predominante:"), choices = list("solo residual" = "solo residual",
                                                                                                         "saprolito" = "saprolito",
                                                                                                         "rocha alterada" = "rocha alterada", 
                                                                                                         "rocha sa" = "rocha sa", 
                                                                                                         "aterro" = "aterro", 
                                                                                                         "lixo" = "lixo", 
                                                                                                         "entulho" = "entulho"), 
                             selected = "escolha material predominante na encosta"),    
                 
                 
                 ################# fim condicionantes ######################
                 
                 ####################### inicio AGUA ###########################
                 
                 h3("Água no terreno"),
                 p("A água é uma das principais causas de escorregamentos. A presença pode ocorrer de várias formas e deve ser observada. Pergunte aos moradores de onde vem a água (servida) e o que é feito dela depois do uso e o que ocorre com as águas das chuvas."),
                 br(),
                 
                 checkboxGroupInput("select_agua", label = h5("Água no terreno:"), choices = list(
                   "Lançamento de água servida em superfície " =     "Lançamento de água servida em superfície ", 
                   "concentração de água de chuva em superfície" =    "concentração de água de chuva em superfície",
                   "vazamento da tubulação" = "vazamento da tubulação",
                   " fossa séptica " = " fossa septica ",
                   "vazamento da tubulação" = "vazamento da tubulação"
                   
                 ), 
                 selected = "presença de água no terreno... "),
                 
                 
                 selectInput("select_drenag", label = h5("Tipo de sistema de drenagem:"), choices = list("inexistente" = "inexistente",
                                                                                                         "satisfatório" = "satisfatório" ,
                                                                                                         "precário" = "precário" 
                 ), 
                 selected = "escolha o tipo de sistema de drenagem")
                 ,
                 
                 
                 
                 
                 
                 
                 
                 
                 #tipo de sistema de drenagem 
                 ####################### fim AGUA ###########################
                 
                 
                 #################################### inicio VEGETACAO ###############
                 
                 h3("Cobertura do terreno"),
                 h4("Instruções:"),
                 p("Dependendo do tipo de vegeação, ela pode ser boa ou ruim 
                   para a segurança da encosta. Anotar a vegetação que se encontra na 
                   área que está sendo avaliada, principalmente se existirem bananeiras."), 
                 
                 selectInput("select_veg", label = h5("Cobertura do terreno:"), 
                             choices = list( 
                               "Presença de árvores" = "Presença de árvores",
                               "vegetação rasteira/arbustiva" = "vegetação rasteira/arbustiva",
                               
                               "área desmatada/solo exposto" = "área desmatada/solo exposto",
                               
                               "gramado/campo" = "gramado/campo",
                               " presença de bananeira" = " presença de bananeira ",
                               "cobertura urbana"="cobertura urbana"
                               
                             ), 
                             selected = "escolha o tipo de cobertura do terreno"),
                 
                 
                 
                 ############################## fim VEGETACAO ######################
                 
                 ############################## inicio EVIDENCIAS de MOVIMENTACAO ####  
                 
                 h3("Evidências de movimenação (feições de instabilidade"),
                 h4("Intruções: "),
                 p("Lembre-se que antes de ocrorrer um escorregamento, a encosta dá sinais que está se movimentando. A observação desses sinais é muito importante para a classificação do risco, a retirada preventiva de moradores e a execução de obras de conteção. "),
                 
                 checkboxGroupInput("select_inst", label = h5("Instabilidade do terreno:"), 
                                    choices = list( 
                                      "Muro e/ou parede embarrigado" = "Muro e/ou parede embarrigado",
                                      "trinca na moradia" = "trinca na moradia",
                                      "árvores, postes, muros inclinados" = "árvores, postes, muros inclinados",
                                      "degrau de abatimento" = "degrau de abatimento", 
                                      "cicatriz de escorregamento"="cicatriz de escorregamento",
                                      "trinca no terreno" = "trinca no terreno"
                                      
                                    ), 
                                    selected = "instabilidade do terreno")
                 
                 ))
             ),
    
    
    
    
    
    ############################## fim EVIDENCIAS de MOVIMENTACAO ####  
    
    
    tabPanel("Respostas",
             
             
             
             ######################### INICIO OUTPUT ########################
             h3("Formulário Preenchido"),
             verbatimTextOutput("txtout_munic"),
             verbatimTextOutput("txout_area"),
             verbatimTextOutput("txtoutt_bairro"),
             verbatimTextOutput("txout_setor"),
             verbatimTextOutput("txout_equipe"),
             verbatimTextOutput("dateText"),
             verbatimTextOutput("select_out_UA"),
             verbatimTextOutput("txout_loc"),
             verbatimTextOutput("txout_moradores"),
             verbatimTextOutput("txout_acesso"),
             verbatimTextOutput("select_out_moradia"), 
             verbatimTextOutput("select_out_encosta"), 
             verbatimTextOutput("txtout_h_max"),
             verbatimTextOutput("txtout_dist_moradia"),
             verbatimTextOutput("selectout_angle"),
             verbatimTextOutput("selectout_posicao_moradia"),
             verbatimTextOutput("selectout_geol"),
             verbatimTextOutput("select_predominante"),
             verbatimTextOutput("selectout_agua"),
             verbatimTextOutput("selectout_drenag"),
             verbatimTextOutput("selectout_veg"),
             verbatimTextOutput("selectout_inst"),
             verbatimTextOutput("txtout_moradia"),
             verbatimTextOutput("selectout_densidade"),
             verbatimTextOutput("selectout_via"),
             verbatimTextOutput("txtout_via"),
             
             
             
             
             
             
             
             
             
             
             ######################### FIM OUTPUT ########################
             
             
             
             br(),
             
             radioButtons('format', 'Formato do documento', c('PDF', 'HTML', 'Word'),
                          inline = TRUE),
             downloadButton("report", "Gerar relatorio")
             
             
             
             
             
             ########################## fim PDF file #####################
             
    ) 
    )










# Define server

server <- function(input, output) {
  
  
  output$txtout_munic <- renderText({
    paste("Municipio:", as.character(input$txt_mun))})
  
  output$txout_area <- renderText({
    paste("Area:", as.character(input$txt_area))})
  
  output$txtoutt_bairro <- renderText({
    paste("Bairro:", as.character(input$txt_bairro))})
  
  output$txout_setor <- renderText({
    paste("Setor:", as.character(input$txt_setor))})
  
  output$txout_equipe <- renderText({
    paste("Equipe:", as.character(input$txt_equip))})
  
  output$dateText  <- renderText({
    paste("Data:", as.character(input$date))})
  
  output$select_out_UA <- renderPrint({input$select_UA})
  
  output$txout_loc <- renderText({
    paste("Localizacao:", as.character(input$txt_loc))})
  output$txout_moradores <- renderText({
    paste("Moradores:", as.character(input$txt_moradores))})
  
  output$txtout_moradia <- renderText({
    paste("Observacao sobre moradias:", as.character(input$txt_moradia))})
  output$selectout_densidade <- renderText({
    paste("Densidade de ocupacao:", as.character(input$select_densidade))})
  output$selectout_via <- renderText({
    paste("Condicoes das vias:", as.character(input$select_via))})
  output$txtout_via <- renderText({
    paste("Feições de instabilidade:", as.character(input$txt_via))})
  
  
  
  
  
  output$txout_acesso <- renderText({
    paste("Acesso:", as.character(input$txt_acesso))})
  output$select_out_moradia <- renderText({
    paste("Tipo de moradia:", as.character(input$select_moradia))})
  output$select_out_encosta<- renderText({
    paste("Encosta:", as.character(input$select_encosta))})
  output$txtout_h_max <- renderText({
    paste("Altura maxima da encosta:", as.character(input$txt_h_max))})
  output$txtout_dist_moradia <- renderText({
    paste("Distancia da moradia em relacao a encosta:", as.character(input$txt_dist_moradia))})
  output$selectout_angle <- renderText({
    paste("Ângulo de inclinação:", as.character(input$select_angle))})
  
  output$selectout_posicao_moradia <- renderText({
    paste("Posição da moradia:", as.character(input$select_posicao_moradia))})
  output$selectout_geol <- renderText({
    paste("Geologia:", as.character(input$select_geol))})
  
  output$selectout_predominante <- renderText({
    paste("Geologia:", as.character(input$select_predominante))})
  
  
  
  output$selectout_agua <- renderText({
    paste("Presença de água no terreno:", as.character(input$select_agua))})
  
  output$selectout_drenag <- renderText({
    paste("Tipo de drenagem:", as.character(input$select_drenag))})
  
  output$selectout_veg <- renderText({
    paste("Cobertura do terreno:", as.character(input$select_veg))})
  
  output$selectout_inst <- renderText({
    paste("Feições de instabilidade:", as.character(input$select_inst))})
  
  
  
  
  #output$equation_out <- renderText ({
  # paste("Grau de risco":, as.character$equation) })
  
  
  
  
  
  
  
  
  
  
  
  ##### output for PDF report ##########
  
  output$report <- downloadHandler (
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  
  
}






# Run the application 
shinyApp(ui, server)



