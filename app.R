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
                 p("Tipos predominante de construcao:"),
                 checkboxInput("select_moradia_alvenaria", label = "Moradia Alvenaria"), 
                 checkboxInput("select_moradia_madeira", label = "Moradia Madeira"), 
                 checkboxInput("select_moradia_mista", label = "Moradia Mista"), 
                 
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
                 h5("Tipos de encosta:"),
                 
                 checkboxInput("select_encosta", label = "Encosta Natural"),
                 checkboxInput("select_talude", label = "Talude de corte"),
                 
                 
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
                 
                 
                 h5("Material predominante:"),
                 checkboxInput("select_residual", label = "solo residual"),
                 checkboxInput("select_saprolito", label = "saprolito"),
                 checkboxInput("select_rocha_alterada", label = "rocha alterada"),
                 checkboxInput("select_rocha_sa", label = "rocha sa"),
                 checkboxInput("select_aterro", label = "aterro"),
                 checkboxInput("select_lixo", label = "lixo"),
                 checkboxInput("select_entulho", label = "entulho"),
                 
                 
                 ################# fim condicionantes ######################
                 
                 ####################### inicio AGUA ###########################
                 
                 h3("Água no terreno"),
                 p("A água é uma das principais causas de escorregamentos. A presença pode ocorrer de várias formas e deve ser observada. Pergunte aos moradores de onde vem a água (servida) e o que é feito dela depois do uso e o que ocorre com as águas das chuvas."),
                 br(),
                 
                 h5("Agua no terreno:"),
                 checkboxInput("select_lancamento", label = "lançamento de água servida em superfície"),
                 checkboxInput("select_chuva", label = "concentração de água de chuva em superfície"),
                 checkboxInput("select_vazamento", label = "vazamento da tubulação"),
                 checkboxInput("select_fossa", label = "fossa septica"),
                 
                 
                 
                 
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
                 
                 h5("Cobertura do terreno:"),
                 checkboxInput("select_arvore", label = "presença de árvores"),
                 checkboxInput("select_rasteira", label = "vegetação rasteira/arbustiva"),
                 checkboxInput("select_desmatado", label = "área desmatada/solo exposto"),
                 checkboxInput("select_urbana", label = "cobertura_urbana"),
                 
                 
                 ############################## fim VEGETACAO ######################
                 
                 ############################## inicio EVIDENCIAS de MOVIMENTACAO ####  
                 
                 h3("Evidências de movimenação (feições de instabilidade"),
                 h4("Intruções: "),
                 p("Lembre-se que antes de ocrorrer um escorregamento, a encosta dá sinais que está se movimentando. A observação desses sinais é muito importante para a classificação do risco, a retirada preventiva de moradores e a execução de obras de conteção. "),
                 
                 h5("Instabilidade do terreno:"),
                 checkboxInput("select_embarrigado", label = "muro embarrigado"),
                 checkboxInput("select_trinca_moradia", label = "trinca moradia"),
                 checkboxInput("select_inclinados", label = "árvores, postes, muros inclinados"),
                 checkboxInput("select_degrau", label = "degrau de abatimento"),
                 checkboxInput("select_trinca_moradia", label = "trinca moradia"),
                 checkboxInput("select_cicatriz", label = "cicatriz de escorregamento"),
                 checkboxInput("select_trinca_terreno", label = "trinca no terreno")
                 
                 ))
             ),
    
    
    
    
    
    ############################## fim EVIDENCIAS de MOVIMENTACAO ####  
    
    
    tabPanel("Respostas",
             
             
             
             ######################### INICIO OUTPUT ########################
             h3("Formulário Preenchido"),
             
             
             
             
             
             
             
             
             
             
             ######################### FIM OUTPUT ########################
             
             ####### Testing Table output #############################
             
             
             tableOutput("values"),
             br(), 
             p("Grau de risco:"),
             verbatimTextOutput("grau_risco"),
             
             
             
             
             
             
             
             
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











############################## Define SERVER #############################

server <- function(input, output) {
  
  
  
  ############# Functions ##################
  x <- reactive(input$select_angle)
  x_geol <- reactive(input$select_geol)
  x1<-reactive(input$select_moradia_alvenaria)
  x2<-reactive(input$select_moradia_madeira)
  x3<-reactive(input$select_moradia_mista) 
  a <- reactive(input$select_aterro)
  l<-reactive(input$select_lixo)
  e <-reactive(input$select_entulho)
  densidade<-reactive(input$select_densidade) 
  servida <-reactive(input$select_lancamento)
  conc_chuva <-reactive(input$select_chuva)
  vazamento <-reactive(input$select_vazamento)
  fossa <- reactive(input$select_fossa)
  drenagem <- reactive(input$select_drenag)
  embarricado <-reactive(input$select_embarrigado)
  trinca_moradia <-reactive(input$select_trinca_moradia)
  trinca_terreno <- reactive(input$select_trinca_terreno)
  degrau <-reactive(input$select_degrau)
  inclinados <-reactive(input$select_inclinados)
  cicatriz <-reactive(input$select_cicatriz)
  posicao_moradia <-reactive(input$select_posicao_moradia)
  
  #Angulo, geologia, solo
  Angulo <-reactive( {
    if (x() ==  "A10") { y <- 0.5028996}
    if (x() == "A10_17") {y<- 0.8866914}
    if (x() == "A_17_30") {y<-2.0248326}
    if (x() == "A_30_60") {y<-4.1555388}
    if (x() == "A_60_90") {y<-3.4541262}
    if (x() == "A90") {y <- 2.2101114}
    y
  })
  
  
  Geologia <- reactive ({
    #solo <- 6.165
    
    # Geologia
    
    if (x_geol() == "favoravel") {geologia <-  1.0030592 }
    if (x_geol() == "desfav") {geologia <- 2.9995328}
    if (x_geol() == "nao_obs") {geologia <- 0.819808}
    
    geologia
  })
  
  # Tipo de Encosta, Moradia, Densidade, Posicao Moradia
  # Need to change because each one of the types of houses is going to be a different column
  
  Moradia <-reactive ({
    #x1 = alvenaria, x2 = madeira, x3 = mista 
    #In this case, I am considering the weight value of the most vulnerable. So, for alvenaria & madeira, the weight is of the madeira. And so on. 
    
    if (x1() == TRUE & x2() == FALSE & x3() == FALSE) {moradia = 0.1582308 } # alvenaria
    if (x1() == FALSE & x2() == TRUE & x3() == FALSE) {moradia = 0.882882 } # madeira
    if (x1() == FALSE & x2() == FALSE & x3() == TRUE) {moradia = 1.2520872} # mista
    if (x1() == TRUE & x2() == TRUE & x3() == FALSE) {moradia = 0.882882 } #alvenaria & madeira
    if (x1() == TRUE & x2() == FALSE & x3() == TRUE)  {moradia = 1.2520872} # alvenaria e mista
    if (x1() == FALSE & x2() == TRUE & x3() == TRUE)  {moradia = 1.2520872}# madeira e mista
    if (x1() == TRUE & x2() == TRUE & x3() == TRUE)   {moradia = 1.2520872} # alvenaria, madeira e mista
    
    moradia
  })
  
  # Aterro, lixo, entulho
  Material <- reactive ({
    
    # a = aterro, l=lixo, e = entulho
    if (a() == TRUE) {a <- 1}
    else if (a() == FALSE) {a <- 0}
    
    if (l() == TRUE) {l <- 1}
    else if (l == FALSE) {l <- 0}
    
    if (e() == TRUE) {e <- 1}
    else if (e() == FALSE) {e<-0}
    
    material <- 2.8959*a + 2.8959*l + 2.8959*e
    material
  })
  
  Densidade <- reactive ({
    
    if (densidade() == "densidade1") { densidade <- 0.1241856 }
    if (densidade() == "densidade2") { densidade <- 0.3796128}
    if (densidade() == "densidade3") { densidade <- 0.613872 }
    if (densidade() == "densidade4") { densidade <- 0.2935296}
    
    densidade
  })
  
  
  
  Agua <- reactive ({
    
    servida=0
    conc_chuva=0
    vazamento=0
    fossa=0
    drenagem="satisf"
    
    if (servida() == "servida") {servida <- 0.9726108}
    if (conc_chuva() == "conc_chuva") {conc_chuva <- 0.5842662}
    if (vazamento() == "vazamento") {vazamento <- 1.1650338}
    if (fossa() == "fossa") {fossa <- 0.5737704}
    
    if (drenagem() == "inexistente") {drenagem <- 0.1022710752}
    if (drenagem() == "precario") { drenagem <-0.0840083832}
    if (drenagem() == "satisf") {drenagem <-0.0166393416}
    
    A <- servida + conc_chuva + vazamento + fossa
    A
  })
  
  
  # need to find a solution for this one... 
  Vegetacao <- reactive ({
    arvore=0
    rasteira=0
    solo_exposto=0
    bananeira=0
    cobertura_urbana=0
    if(arvore() == "arvore"){arvore <- 1}
    if(rasteira() == "rasteira" ) {rasteira <- 1}
    if (solo_exposto() == "solo_exposto" ) {solo_exposto <- 1}
    if (bananeira() == "bananeira")(bananeira <- 1)
    if (cobertura_urbana() == "cobertura_urbana")(cobertura_urbana <- 1)
    
    cobertura <-   (0.022344*arvore+0.1854552*rasteira+0.6948984*solo_exposto+0.290472*bananeira+0.245784*cobertura_urbana)
    cobertura
  })
  
  
  # Instabilidade do terreno
  Instabilidade  <- reactive ({
    embarricado=0 
    trinca_moradia=0
    trinca_terreno=0
    degrau=0
    inclinados=0
    cicatriz=0
    
    if(embarricado() == "embarricado"){embarricado <- 1}
    if(trinca_moradia() == "trinca_moradia" ) {trinca_moradia <- 1}
    if (trinca_terreno() == "trinca_terreno" ) {trinca_terreno <- 1}
    if (inclinados() == "inclinados") {inclinados <- 1}
    if (degrau() == "degrau")(degrau <- 1)
    if (cicatriz() == "cicatriz")(cicatriz <- 1)
    
    instabilidade <-                        (7.5849*embarricado+7.1217*trinca_moradia+6.1374*inclinados+13.7223*degrau+11.4642*cicatriz+11.8116*trinca_terreno)
    instabilidade
  })
  
  
  Posicao_moradia <- reactive( {
    
    if (posicao() == encosta){posicao <- (0.8263458+0.5453406+0.6620292) }
    if (posicao() == dist_base) {posicao <- 0.202419}
    if (posicao() == dist_topo) {posicao <-0.1452654}
    posicao
  })
  
  
  
  risk <-reactive ({
    final = Agua() + Angulo() + Densidade() + Instabilidade() + Material() + Moradia() + Vegetacao()
    if (final < 7.11) {return("R1")}
    if (final >= 7.11 & final <12) {return("R2")}
    if (final >=12 & final >37) {return("R3")}
    if (final >=37) {return("R4")}
  })   
  
  output$grau_risco <- reactive({
    renderText(risk())
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ### Reactive expression to create dataframe of all inputs values for html and visualizatin
  
  
  textValues <- reactive({
    
    data.frame (
      
      Caracteristica = c("Municipio", 
                         "Area",
                         "Bairro",
                         "Setor", 
                         "Equipe",
                         "Data",
                         "Unidade de Analise",
                         "Localizacao", 
                         "Moradores",
                         "Acesso ao local",
                         "Moradia Alvenaria",
                         "Moradia Madeira",
                         "Moradia Mista",
                         "Observacao moradias",
                         "Densidade da ocuapacao", 
                         "Condicao da Via", 
                         "Observacao da via",
                         "Encosta Natural",
                         "Talude de Corte",
                         "Altura maxima da encosta natural",
                         "Distancia da moradia",
                         "Angulo de inclinacao", 
                         "Posicao da  moradia",
                         "Geologia", 
                         "Solo Residual",
                         "Saprolito",
                         "Rocha alterada",
                         "Rocha sa", 
                         "Aterro",
                         "Lixo",
                         "Entulho",
                         "Lançamento de água servida em superfície",
                         "Concentração de água de chuva em superfície",
                         "Vazamento da tubulação",
                         "Fossa septica",
                         "Sistema de drenagem",
                         "Presença de árvores",
                         "Vegetação rasteira/arbustiva",
                         "área desmatada/solo exposto",
                         "Cobertura urbana",
                         "Muro embarrigado", 
                         "Trinca na moradia", 
                         "Arvores, postes, e muros inclinados",
                         "Degrau de abatimento",
                         "Cicatrizes de escorregamento",
                         "Trinca no terreno"
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
        input$select_moradia_alvenaria,
        input$select_moradia_madeira,
        input$select_moradia_mista, 
        input$txt_moradia,
        input$select_densidade,
        input$select_via, 
        input$txt_via,
        input$select_encosta,
        input$select_talude,
        input$txt_h_max, 
        input$txt_dist_moradia, 
        input$select_angle, 
        input$select_posicao_moradia,
        input$select_geol, 
        input$select_residual,
        input$select_saprolito, 
        input$select_rocha_alterada,
        input$select_rocha_sa,
        input$select_aterro,
        input$select_lixo,
        input$select_entulho,
        input$select_lancamento,
        input$select_chuva,
        input$select_vazamento, 
        input$select_fossa,
        input$select_drenag, 
        input$select_arvore,
        input$select_rasteira,
        input$select_desmatado,
        input$select_urbana,
        input$select_embarrigado,
        input$select_trinca_moradia,
        input$select_inclinados,
        input$select_degrau,
        input$select_cicatriz,
        input$select_trinca_terreno),
        
        stringsAsFactors = FALSE))
    
    
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
               "Moradia alvernaria" = input$select_moradia_alvenaria,
               "Moradia madeira" = input$select_moradia_madeira,
               "Moradia mista" = input$select_moradia_mista,
               "Moradia (observacoes)" = input$txt_moradia,
               "Densidade" = input$select_densidade,
               "Via de acesso" = input$select_via, 
               "Via de acesso (observacoes)" = input$txt_via,
               "Encosta Natural" = input$select_encosta, 
               "Talude de corte" = input$select_talude,
               "Altura maxima do talude" = input$txt_h_max, 
               "Distancia Moradia" = input$txt_dist_moradia, 
               "Angulo de inclinacao" = input$select_angle, 
               "Posicao/Localiz das moradias" = input$select_posicao_moradia,
               "Geologia" = input$select_geol, 
               "Solo residual" = input$select_residual,
               "Saprolito" = input$select_saprolito, 
               "Rocha alterada" = input$select_rocha_alterada,
               "Rocha sa" = input$select_rocha_sa,
               "Aterro" = input$select_aterro,
               "Lixo" = input$select_lixo,
               "Entulho" =input$select_entulho,
               "Lançamento de água servida em superfície" = input$select_lancamento,
               "Concentração de água de chuva em superfície" = input$select_chuva,
               "Vazamento da tubulação" = input$select_vazamento,
               "Fossa septica" = input$select_fossa,
               "Drenagem" = input$select_drenag, 
               "Presença de árvores"  = input$select_arvore,
               "Vegetação rasteira/arbustiva" = input$select_rasteira,
               "área desmatada/solo exposto" = input$select_desmatado,
               "Cobertura_urbana" = input$select_urbana,
               "Muro embarrigado" = input$select_embarrigado,
               "Trinca moradia" = input$select_trinca_moradia,
               "Arvores, postes, e muros inclinados" = input$select_inclinados,
               "Degrau de abatimento" = input$select_degrau,
               "Cicatriz de escorregamento" = input$select_cicatriz,
               "Trinca no terreno" =input$select_trinca_terreno,
               "Grau de risco" = output$grau_risco)
  } )
  
  
  ######################## end Reactive for excel ##################
  
  
  
  
  
  
  
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
                         k1 = input$select_moradia_alvenaria,
                         k2 = input$select_moradia_madeira,
                         k3 = input$select_moradia_mista,
                         l= input$txt_moradia,
                         m= input$select_densidade,
                         n = input$select_via, 
                         o =  input$txt_via,
                         p = input$select_encosta, 
                         p1 = input$select_talude,
                         q =input$txt_h_max, 
                         r =input$txt_dist_moradia, 
                         s = input$select_angle,
                         t =input$select_posicao_moradia,
                         u =input$select_geol,
                         v = input$select_residual,
                         v1 = input$select_saprolito, 
                         v2 = input$select_rocha_alterada,
                         v3 = input$select_rocha_sa,
                         v4 = input$select_aterro,
                         v5 = input$select_lixo,
                         v6 = input$select_entulho, 
                         x = input$select_lancamento,
                         x1 = input$select_chuva,
                         x2 = input$select_vazamento,
                         x3 = input$select_fossa,
                         z = input$select_drenag, 
                         y = input$select_arvore,
                         y1 = input$select_rasteira,
                         y2 = input$select_desmatado,
                         y3 = input$select_urbana,
                         w = input$select_embarrigado,
                         w1 = input$select_trinca_moradia,
                         w2 = input$select_inclinados,
                         w3 = input$select_degrau,
                         w5 = input$select_cicatriz,
                         w6 = input$select_trinca_terreno,
                         w7 = output$grau_risco
                         
                         
                         
                         
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
                         k1 = input$select_moradia_alvenaria,
                         k2 = input$select_moradia_madeira,
                         k3 = input$select_moradia_mista,
                         l= input$txt_moradia,
                         m= input$select_densidade,
                         n = input$select_via, 
                         o =  input$txt_via,
                         p = input$select_encosta,
                         p1 = input$select_talude,
                         q =input$txt_h_max, 
                         r =input$txt_dist_moradia, 
                         s = input$select_angle,
                         t =input$select_posicao_moradia,
                         u =input$select_geol,
                         v = input$select_residual,
                         v1 = input$select_saprolito, 
                         v2 = input$select_rocha_alterada,
                         v3 = input$select_rocha_sa,
                         v4 = input$select_aterro,
                         v5 = input$select_lixo,
                         v6 = input$select_entulho, 
                         x = input$select_lancamento,
                         x1 = input$select_chuva,
                         x2 = input$select_vazamento,
                         x3 = input$select_fossa,                         
                         z = input$select_drenag, 
                         y = input$select_arvore,
                         y1 = input$select_rasteira,
                         y2 = input$select_desmatado,
                         y3 = input$select_urbana,                         
                         w1 = input$select_trinca_moradia,
                         w2 = input$select_inclinados,
                         w3 = input$select_degrau,
                         w5 = input$select_cicatriz,
                         w6 = input$select_trinca_terreno, 
                         w7 = output$grau_risco
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
    filename = function(file) {
      paste( df <- data.frame(
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
        input$select_moradia_alvenaria,
        input$select_moradia_madeira,
        input$select_moradia_mista,
        input$txt_moradia,
        input$select_densidade,
        input$select_via, 
        input$txt_via,
        input$select_encosta, 
        input$select_talude,
        input$txt_h_max, 
        input$txt_dist_moradia, 
        input$select_angle, 
        input$select_posicao_moradia,
        input$select_geol,
        input$select_residual,
        input$select_saprolito, 
        input$select_rocha_alterada,
        input$select_rocha_sa,
        input$select_aterro,
        input$select_lixo,
        input$select_entulho, 
        input$select_lancamento,
        input$select_chuva,
        input$select_vazamento,
        input$select_fossa,        
        input$select_drenag, 
        input$select_arvore,
        input$select_rasteira,
        input$select_desmatado,
        input$select_urbana,
        input$select_embarrigado, 
        input$select_trinca_moradia,
        input$select_inclinados,
        input$select_degrau,
        input$select_cicatriz,
        input$select_trinca_terreno,
        output$grau_risco),
        
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
                          "Moradia Alvenaria",
                          "Moradia Madeira",
                          "Moradia Mista",
                          "Observacao moradias",
                          "Densidade da ocuapacao", 
                          "Via", 
                          "Observacao da via",
                          "Tipo de encosta",
                          "Talude de corte",
                          "Altura maxima da encosta natural",
                          "Distancia da moradia",
                          "Angulo de inclinacao", 
                          "Posicao da  moradia",
                          "Geologia", 
                          "Solo residual",
                          "Saprolito", 
                          "Rocha alterada",
                          "Rocha sa", 
                          "Aterro",
                          "Lixo",
                          "Entulho",
                          "Lancamento de agua servida",
                          "Concentracao de agua de chuva",
                          "Vazamento",
                          "Fossa septica",
                          "Tipo de drenagem" ,
                          "Presenca de arvores",
                          "Presenca de vegetacao rasteira",
                          "Solo exposto/vegetacao desmatada",
                          "Cobertura urbana",
                          "Muro embarrigado", 
                          "Trinca na moradia", 
                          "Arvores, postes, e muros inclinados",
                          "Degrau de abatimento",
                          "Cicatrizes de escorregamento",
                          "Trinca no terreno", 
                          "Grau de risco"
                          
        )
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


