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
library(knitr)



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
                             choices = list("1" = "densidade1", "2" = "densidade2", "3" = "densidade3", "4"="densidade4"), 
                             selected = ""),
                 
                 selectInput("select_via", label = h5("Condicao da via:"), 
                             choices = list("pavimentada" = "pavimentada", "nao pavimentada" = "nao pavimentada"), 
                             selected = ""),
                 
                 textInput("txt_via", "Obs:", "escreva observacoes"),
                 
                 
                 
                 ########### fim caracteristica area ###################
                 
                 
                 
                 ############inicio condiciionantes ############################
                 h3("Condicionantes"),
                 h4("Instruções:"),
                 p("Descrever o terreno onde estão as moradias. Utilize o desenho no primeiro quadro como referência para as condições encontradas. Antes de preencher dê um passeio no entorno das moradias. Olhe com atenção os barrancos (taludes) e suba neles se for necessário."),
                 br(),
                 
                 checkboxGroupInput("select_encosta", label = h5("Tipos de encosta:"), 
                             choices = list("Encosta Natural" = "Encosta Natural", "Talude de corte" = "Talude de corte"), 
                             selected = "Encosta Natural"),   
                 textInput("txt_h_max", "Altura maxima (m):", "preencher em metros"), 
                 
                 textInput("txt_dist_moradia", "Distancia moradia em relação encosta/talude m):", "preencher em metros"),
                 
                 p("Em relação ao ângulo de inclinação(teta) da encosta/talude: "),
                 selectInput("select_angle", label = h5("Ângulo de inclinação (em graus):"), choices = list("< 10" = "A10", "entre 10 e 17" = "A10_17","entre 17 e 30" = "A_17_30", "entre 30 e 60" = "A_30_60", "entre 60 e 90" = "A_60_90", "90"="90"), 
                             selected = "escolher ângulo de inclinação da encosta"),
                 
                 selectInput("select_posicao_moradia", label = h5("Posição da moradia:"), choices = list(
                   "Na encosta" = "na_encosta",          "Distante da base da encosta" =  "dist_base",
                          "Distante do topo da encosta" =  "dist_topo"
                 ), 
                 selected = "escolha posição da moradia"),
                 
                 selectInput("select_geol", label = h5("Geologia do terreno:"), choices = list("favorável à estabilidade" = "favoravel",
                                                                                               "desfavorável à estabilidade" = "desfav",
                                                                                               "não observada" = "nao_obs"), 
                             selected = ""),
                 
                 
                 
                 checkboxGroupInput("select_predominante", 
                                    label = h5("Material predominante:"), 
                                    choices = list("solo residual" = "solo residual",
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
                   "Lançamento de água servida em superfície " =     "servida", 
                   "concentração de água de chuva em superfície" =    "conc_chuva",
                   "vazamento da tubulação" = "vazamento",
                   " fossa séptica " = " fossa"

                 ), 
                 selected = "presença de água no terreno... "),
                 
                 
                 selectInput("select_drenag", label = h5("Tipo de sistema de drenagem:"), choices = list("inexistente" = "inexistente",
                                                                                                         "satisfatório" = "satisf" ,
                                                                                                         "precário" = "precario" 
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
                 
                 checkboxGroupInput("select_veg", label = h5("Cobertura do terreno:"), 
                             choices = list( 
                               "Presença de árvores" = "arvore",
                               "vegetação rasteira/arbustiva" = "rasteira",
                               
                               "área desmatada/solo exposto" = "solo_exposto",
                               " presença de bananeira" = "bananeira",
                               "cobertura urbana"="cobertura_urbana"
                               
                             ), 
                             selected = "escolha o tipo de cobertura do terreno"),
                 
                 
                 
                 ############################## fim VEGETACAO ######################
                 
                 ############################## inicio EVIDENCIAS de MOVIMENTACAO ####  
                 
                 h3("Evidências de movimenação (feições de instabilidade"),
                 h4("Intruções: "),
                 p("Lembre-se que antes de ocrorrer um escorregamento, a encosta dá sinais que está se movimentando. A observação desses sinais é muito importante para a classificação do risco, a retirada preventiva de moradores e a execução de obras de conteção. "),
                 
                 checkboxGroupInput("select_inst", label = h5("Instabilidade do terreno:"), 
                                    choices = list( 
                                      "Muro e/ou parede embarrigado" = "embarricado",
                                      "trinca na moradia" = "trinca_moradia",
                                      "árvores, postes, muros inclinados" = "inclinados",
                                      "degrau de abatimento" = "degrau", 
                                      "cicatriz de escorregamento"="cicatriz",
                                      "trinca no terreno" = "trinca_terreno"
                                      
                                    ), 
                                    selected = "instabilidade do terreno")
                 
                 ))
             ),
    
    
    
    
    
    ############################## fim EVIDENCIAS de MOVIMENTACAO ####  
    
    
    tabPanel("Respostas",
             
             
             
             ######################### INICIO OUTPUT ########################
             h3("Formulário Preenchido"),

             
             
             
             
             
             
             
             
             
             ######################### FIM OUTPUT ########################
             
             ####### Testing Table output #############################
             
             
             tableOutput("values"),
             
  
             
             
             
             
             
             
             ######################## end of testing table output ##############
             
             br(),
            
             p("Baixar relatorio:"),
             
             
             ################ inicio HTML button ####################
             downloadButton( "format", "Baixar - HTML"),
  
             ########################## end html file #####################
             
             
             ################ inicio PDF button ####################
             downloadButton( 'format_pdf', 'Baixar - PDF'),
             
             ########################## end html file #####################
             
             
             
             
             # #################### inicio EXCELL dowonload button ############
             downloadButton("downloadData", "Baixar em Excel"), 
             br()
             
             ###################### end EXCEL download button #################
             
             
    )
             
             
             
             
    ) 











# Define server

server <- function(input, output) {
  
  
  
  
  
  
  
  
  ### Reactive expression to create dataframe of all inputs values for html and visualizatin
  
  
  textValues <- reactive({
    
    data.frame (
      
      Caracteristica = c("Municipio", 
                         "Area",
                         "Bairro",
                         "Setor", 
                         "Equipe",
                         "Data",
                         "UA",
                         "Localizacao", 
                         "Moradores",
                         "Acesso ao local",
                         "Tipo de moradias",
                         "Observacao moradias",
                         "Densidade da ocuapacao", 
                         "Via", 
                         "Observacao da via",
                         "Tipo de encosta", 
                         "Altura maxima da encosta natural",
                         "Distancia da moradia",
                         "Angulo de inclinacao", 
                         "Posicao da  moradia",
                         "Geologia", 
                         "Material predominante",
                         "Agua",
                         "Tipo de drenagem" ,
                         "Tipo de vegetacao",
                         
                         "Instabilidade do terreno"
                         ), 
      
      Resposta = as.character (c(
                              input$txt_mun, 
                              input$txt_area, 
                              input$txt_bairro, 
                              input$txt_setor,
                              input$txt_equip, 
                              as.character(input$date), 
                              input$select_UA, 
                              input$txt_loc, 
                              input$txt_moradores, 
                              input$txt_acesso, 
                              input$select_moradia,
                              input$txt_moradia,
                              input$select_densidade,
                              input$select_via, 
                              input$txt_via,
                              paste(input$select_encosta, collapse = ";"), 
                              input$txt_h_max, 
                              input$txt_dist_moradia, 
                              input$select_angle, 
                              input$select_posicao_moradia,
                              input$select_geol, 
                              paste(input$select_predominante, collapse =";"),
                              paste(input$select_agua, collapse=";"),
                              input$select_drenag, 
                              paste(input$select_veg, collapse = ";"),
                              paste(input$select_inst, collapse =";")
                              )), 
      stringsAsFactors = FALSE)
                              
                            
  })
     
  ####################### end of reactive inputs for html and vizualizaiton ###############
                

    #show the values in the HTML table ----
    output$values <- renderTable ({
      textValues ()
    })
    
  
    
  ############# Reactive for excel #######################
   
    
    textValues2 <- reactive({
   data.frame("Municipio" = input$txt_mun, 
               "Area" = input$txt_area, 
               "Bairro"= input$txt_bairro, 
               "Setor" = input$txt_setor,
               "Equipe" = input$txt_equip, 
               "Data" = as.character(input$date), 
               "UA" = input$select_UA, 
               "Localizacao" = input$txt_loc, 
               "Moradores contato" = input$txt_moradores, 
               "Acesso" = input$txt_acesso, 
               "Tipo de Moradia" = input$select_moradia,
               "Moradia (observacoes)" = input$txt_moradia,
               "Densidade" = input$select_densidade,
               "Via de acesso" = input$select_via, 
               "Via de acesso (observacoes)" = input$txt_via,
               "Tipo de encosta" = paste(input$select_encosta, collapse = ";"), 
               "Altura maxima do talude" = input$txt_h_max, 
               "Distancia Moradia" = input$txt_dist_moradia, 
               "Angulo de inclinacao" = input$select_angle, 
               "Posicao/Localiz das moradias" = input$select_posicao_moradia,
               "Geologia" = input$select_geol, 
               "Material presente/predominante" = paste(input$select_predominante, collapse =";"),
               "Agua no terreno" = paste(input$select_agua, collapse=";"),
               "Drenagem" = input$select_drenag, 
               "Tipo de Vegetacao" = paste(input$select_veg, collapse = ";"),
               "Instabilidade" = paste(input$select_inst, collapse =";"))
  
} )
    
    
  ######################## end Reactive for excel ##################
                             
    
  
  
  
  
  
  
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
    paste("Tipo de material predominante:", as.character(input$select_predominante))})
  
  
  
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
  
  
  
  
  
  
  
  
  
  
  
  ##### output for HTML report ##########
  
  output$format <- downloadHandler (
    
    
    # Specify the file name
    filename = "report.html", 
    content = function(file)
     {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      ### parameters 
      
      params <- list(    a= input$txt_mun, 
                         b = input$txt_area, 
                          c= input$txt_bairro, 
                          d=input$txt_setor,
                          e=input$txt_equip, 
                          f= as.character(input$date),
                        g = input$select_UA, 
                        h=input$txt_loc, 
                        i = input$txt_moradores, 
                         j =input$txt_acesso, 
                        k = input$select_moradia,
                          l= input$txt_moradia,
                          m= input$select_densidade,
                          n = input$select_via, 
                         o =  input$txt_via,
                        p =paste(input$select_encosta, collapse="; "), 
                         q =input$txt_h_max, 
                         r =input$txt_dist_moradia, 
                        s = input$select_angle,
                         t =input$select_posicao_moradia,
                          u =input$select_geol,
                         v = paste(input$select_predominante, collapse ="; "),
                          x = paste(input$select_agua, collapse="; "),
                          z = input$select_drenag, 
                          y = paste(input$select_veg, collapse=";"),
                          w = paste(input$select_inst, collapse =";")
      )
      
      
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
   ) }
      
  ) 
  #### ################ end of output report
 
  ################ beginning of output report PDF #############
 
  output$format_pdf <- downloadHandler (
    
    
    # Specify the file name
    filename = "report2.pdf", 
    content = function(file)
    {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_pdf.Rmd")
      file.copy("report_pdf.Rmd", tempReport, overwrite = TRUE)
      
      ### parameters 
      
      params <- list(    a= input$txt_mun, 
                         b = input$txt_area, 
                         c= input$txt_bairro, 
                         d=input$txt_setor,
                         e=input$txt_equip, 
                         f= as.character(input$date),
                         g = input$select_UA, 
                         h=input$txt_loc, 
                         i = input$txt_moradores, 
                         j =input$txt_acesso, 
                         k = input$select_moradia,
                         l= input$txt_moradia,
                         m= input$select_densidade,
                         n = input$select_via, 
                         o =  input$txt_via,
                         p =paste(input$select_encosta, collapse="; "), 
                         q =input$txt_h_max, 
                         r =input$txt_dist_moradia, 
                         s = input$select_angle,
                         t =input$select_posicao_moradia,
                         u =input$select_geol,
                         v = paste(input$select_predominante, collapse ="; "),
                         x = paste(input$select_agua, collapse="; "),
                         z = input$select_drenag, 
                         y = paste(input$select_veg, collapse=";"),
                         w = paste(input$select_inst, collapse =";")
      )
      
      
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      ) }
    
  ) 
  
  ################ end of output report PDF ###############
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###### ################## excel download #######################
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(       
        data.frame(input$txt_mun, 
        input$txt_area, 
        input$txt_bairro, 
        input$txt_setor,
        input$txt_equip, 
        as.character(input$date), 
        input$select_UA, 
        input$txt_loc, 
        input$txt_moradores, 
        input$txt_acesso, 
        input$select_moradia,
        input$txt_moradia,
        input$select_densidade,
        input$select_via, 
        input$txt_via,
        paste(input$select_encosta, collapse = ";"), 
        input$txt_h_max, 
        input$txt_dist_moradia, 
        input$select_angle, 
        input$select_posicao_moradia,
        input$select_geol, 
        paste(input$select_predominante, collapse =";"),
        paste(input$select_agua, collapse=";"),
        input$select_drenag, 
        paste(input$select_veg, collapse = ";"),
        paste(input$select_inst, collapse =";")), 
        
        colnames(df) <- c("Municipio", 
                          "Area",
                          "Bairro",
                          "Setor", 
                          "Equipe",
                          "Data",
                          "UA",
                          "Localizacao", 
                          "Moradores",
                          "Acesso ao local",
                          "Tipo de moradias",
                          "Observacao moradias",
                          "Densidade da ocuapacao", 
                          "Via", 
                          "Observacao da via",
                          "Tipo de encosta", 
                          "Altura maxima da encosta natural",
                          "Distancia da moradia",
                          "Angulo de inclinacao", 
                          "Posicao da  moradia",
                          "Geologia", 
                          "Material predominante",
                          "Agua",
                          "Tipo de drenagem" ,
                          "Tipo de vegetacao",
                          "Instabilidade do terreno")
      , ".csv", sep = "")
    },
    content = function(file) {
      write.csv(textValues2(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
  ######################## end download excel ###################################
  
    }







# Run the application 
shinyApp(ui, server)



