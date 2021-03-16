################### libraries ###########################
library(shiny)
library (shinydashboard)
library (tidyverse)
library(leaflet)
#library(sf)
library(shinythemes)
library(latex2exp)
library (markdown)
library(rmarkdown)
library(knitr)



##########################################################

####to run App use:
##runApp("newdir")
#on the console


################## DATASET ##################################






############################ UI ######################################################                                                                        


ui <-
  
  
  
  
  
  
  navbarPage( 
    
    #### App title --- 
    "Mapeamento de Áreas e Risco",
    
    
    ### theme selector
    tabPanel("Diagnóstico", 
             
             shinythemes::themeSelector(),
             
             
             # Application title
             titlePanel(h2("Diagnóstico para mapeamento de Áreas de Risco de Escorregamento Raso")),
             
             ############################################ inicio Roteiro Intrucoes  ########################################################  
             p("A) Este roteiro objectiva auxiliar a tomada de decisão sobre as moradias que estão sob risco de escorregamentos."),
             p("B) Ao final do preenchimento o grau de risco será computado automaticamente."),
             p("C) O preenchimento deve ser feito passo-a-passo. Para cada passo existem instruções que devem ser lidas com atenção. Nos espaços em branco, preencha as informações solicitadas. "),
             p("D) Converse com os moradores das casas e vizinhos. As pessoas têm a tendência de tentar esconder fatos, pensando nos problemas que uma remoção pode lhes causar. Quando for possível pergunte para crianças. "),
             br(),
             ######################################### termino Roteiro Instrucoes ########################################################
             
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               
               
               sidebarPanel(
                 
                 
                 textInput("txt_mun", "Município:", "nome do município"), 
                 textInput("txt_area", "Área:", " nome da área"), 
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
                 p("Este campo deve ser preenchido com cuidado, pois deverá permitir que qualquer pessoa possa chegar/retornar ao local. Não esqueça de colocar a localização/endereço das moradias - usar nome ou número da rua, viela, escadaria, ligação de água ou luz, nomes de vizinhos, nomes de moradores e as condições de acesso à área, como por exemplo: via de terra, escadaria de cimento, rua asfaltada, boas ou más condições, etc. Também deve mencionar os tipos de moradias"),
                 
                 
                 
                 textInput("txt_loc", "Localização (rua/viela/escadaria,etc):", ""), 
                 
                 textInput("txt_moradores", "Nome de moradores: (nome de moradores para referencia/contato)", ""),
                 
                 textInput("txt_acesso", "Condições de Acesso a Área (descreva condições de acesso da área):", ""),
                 p("Tipos predominante de construção:"),
                 checkboxInput("select_moradia_alvenaria", label = "Moradia Alvenaria"), 
                 checkboxInput("select_moradia_madeira", label = "Moradia Madeira"), 
                 checkboxInput("select_moradia_mista", label = "Moradia Mista"), 
                 
                 textInput("txt_moradia", "Obs:", ""),
                 selectInput("select_densidade", label = h5("Densidade de ocupação:"), 
                             choices = list("1" = "densidade1", "2" = "densidade2", "3" = "densidade3", "4"="densidade4"), 
                             selected = ""),
                 
                 selectInput("select_via", label = h5("Condição da via:"), 
                             choices = list("pavimentada" = "pavimentada", "nao pavimentada" = "nao pavimentada"), 
                             selected = ""),
                 
                 textInput("txt_via", "Obs:", "escreva observações"),
                 
                 
                 
                 ########### fim caracteristica area ###################
                 
                 
                 
                 ############inicio condiciionantes ############################
                 h3("Condicionantes"),
                 h4("Instruções:"),
                 p("Descrever o terreno onde estão as moradias. Utilize o desenho no primeiro quadro como referência para as condições encontradas. Antes de preencher, dê um passeio no entorno das moradias; olhe com atenção os barrancos (taludes) e suba neles se for necessário."),
                 br(),
                 h5("Tipos de encosta:"),
                 
                 checkboxInput("select_encosta", label = "Encosta Natural"),
                 checkboxInput("select_talude", label = "Talude de corte"),
                 
                 
                 textInput("txt_h_max", "Altura máxima (m):", "preencher em metros"), 
                 
                 textInput("txt_dist_moradia", "Distância moradia em relação encosta/talude m):", "preencher em metros"),
                 
                 p("Em relação ao ângulo de inclinação(teta) da encosta/talude: "),
                 selectInput("select_angle", label = h5("Ângulo de inclinação (em graus):"), choices = list("< 10" = "A10", "entre 10 e 17" = "A10_17","entre 17 e 30" = "A_17_30", "entre 30 e 60" = "A_30_60", "entre 60 e 90" = "A_60_90", "90"="90"), 
                             selected = "escolher ângulo de inclinação da encosta"),
                 
                 selectInput("select_posicao_moradia", label = h5("Posição da moradia em relação à encosta:"), choices = list(
                   "Meio" = "meio_encosta", 
                   "Próximo da base da encosta" = "prox_base",
                   "Próximo do topo da encosta" = "prox_topo",
                   "Distante da base da encosta" =  "dist_base",
                   "Distante do topo da encosta" =  "dist_topo",
                   "Meio, próximo da base, e próximo do topo"= "encosta_tudo"
                 ), 
                 selected = "escolha posição da moradia"),
                 
                 
                 selectInput("select_solo", label = h5("Solo do terreno:"), choices = list("favorável à estabilidade" = "favoravel",
                                                                                               "desfavorável à estabilidade" = "desfav",
                                                                                               "não observada" = "nao_obs"), 
                             selected = ""),
                 
                 selectInput("select_cobertura_natural", label = h5("Cobertura Natural:"), choices = list("favorável à estabilidade" = "favoravel",
                                                                                               "desfavorável à estabilidade" = "desfav",
                                                                                               "não observada" = "nao_obs"), 
                             selected = ""),
                 
                 
                
                 
                 selectInput("select_geol", label = h5("Geologia do terreno:"), choices = list("favorável à estabilidade" = "favoravel",
                                                                                               "desfavorável à estabilidade" = "desfav",
                                                                                               "não observada" = "nao_obs"), 
                             selected = ""),
                 
                 
                 h5("Material predominante:"),
                 checkboxInput("select_residual", label = "solo residual"),
                 checkboxInput("select_saprolito", label = "saprolito"),
                 checkboxInput("select_rocha_alterada", label = "rocha alterada"),
                 checkboxInput("select_rocha_sa", label = "rocha sã"),
                 checkboxInput("select_aterro", label = "aterro"),
                 checkboxInput("select_lixo", label = "lixo"),
                 checkboxInput("select_entulho", label = "entulho"),
                 
                 
                 ################# fim condicionantes ######################
                 
                 ####################### inicio AGUA ###########################
                 
                 h3("Água no terreno"),
                 p("A água é uma das principais causas de escorregamentos. A presença pode ocorrer de várias formas e deve ser observada. Pergunte aos moradores de onde vem a água (servida) e o que é feito dela depois do uso e o que ocorre com as águas das chuvas."),
                 br(),
                 
                 h5("Água no terreno:"),
                 checkboxInput("select_lancamento", label = "lançamento de água servida em superfície"),
                 checkboxInput("select_chuva", label = "concentração de água de chuva em superfície"),
                 checkboxInput("select_vazamento", label = "vazamento da tubulação"),
                 checkboxInput("select_fossa", label = "fossa séptica"),
                 
                 
                 
                 
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
                 checkboxInput("select_arbusto", label = "vegetação arbustiva"),
                 
                 checkboxInput("select_rasteira", label = "vegetação rasteira"),
                 checkboxInput("select_desmatado", label = "área desmatada/solo exposto"),
                 checkboxInput("select_bananeira", label = "bananeira"),
                 checkboxInput("select_urbana", label = "cobertura urbana"),
                 
                 
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







####to run App use:
##runApp("newdir")
#on the console



############################## Define SERVER #############################

server <- function(input, output) {
  
  
  
  ############# Functions ##################
  x <- reactive ({ input$select_angle
    
  })
  x_geol <- reactive({ input$select_geol})
  x_solo <- reactive({input$select_solo})
  x_cobertura_natural <- reactive({input$cobertura_natural})
  x1<-reactive({input$select_moradia_alvenaria})
  x2<-reactive({input$select_moradia_madeira})
  x3<-reactive({input$select_moradia_mista}) 
  a <- reactive({input$select_aterro})
  l<-reactive({input$select_lixo})
  e <-reactive({input$select_entulho})
  densidade<-reactive({input$select_densidade}) 
  servida <-reactive({input$select_lancamento})
  conc_chuva <-reactive({input$select_chuva})
  vazamento <-reactive({input$select_vazamento})
  fossa <- reactive({input$select_fossa})
  drenagem <- reactive({input$select_drenag})
  embarricado <-reactive({input$select_embarrigado})
  trinca_moradia <-reactive({input$select_trinca_moradia})
  trinca_terreno <- reactive({input$select_trinca_terreno})
  degrau <-reactive({input$select_degrau})
  inclinados <-reactive({input$select_inclinados})
  cicatriz <-reactive({input$select_cicatriz})
  posicao_moradia <-reactive({input$select_posicao_moradia})
  arvore <- reactive({input$select_arvore})
  arbusto <- reactive({input$select_arbusto})
  vegetacao <- reactive({input$select_rasteira})
  rasteira <- reactive({input$select_rasteira})
  desmatado <-reactive({input$select_desmatado})
  cobertura_urbana <- reactive({input$select_urbana})
  bananeira <-reactive({input$select_bananeira})
  
  
  
  #Angulo, geologia, solo
  #Values are already adjusted to give the natural weight for the equation
  
  Angulo <-reactive( {
    if (x() ==  "A10") { y <- 2.76613484713376}
    if (x() == "A10_17") {y<- 4.87713249363057}
    if (x() == "A_17_30") {y<-11.137332410828}
    if (x() == "A_30_60") {y<-22.857009}
    if (x() == "A_60_90") {y<-18.9989788184713}
    if (x() == "A90") {y <- 12.1564347229299}
    y
  })
  
  sOLO <- reactive ({

    # Solo
    
    if (x_solo() == "favoravel") {solo <-  10.647675 }
    if (x_solo() == "desfav") {solo <- 0}
    if (x_solo() == "nao_obs") {solo <- 5.3238375}
    
    solo
  })
  
  
  cobertura_natural <- reactive ({
    
    # Solo
    
    if (x_cob_nat() == "favoravel") {cobertura_natural <-  5.489468 }
    if (x_cob_nat() == "desfav") {cobertura_natural <- 2.744734}
    if (x_cob_nat() == "nao_obs") {cobertura_natural <- 0}
    
    cobertura_natural
  })
  
  
  Geologia <- reactive ({
    
    
    # Geologia
    
    if (x_geol() == "favoravel") {geologia <-  8.328848 }
    if (x_geol() == "desfav") {geologia <- 0}
    if (x_geol() == "nao_obs") {geologia <- 4.164424}
    
    geologia
  })
  
  # Tipo de Encosta, Moradia, Densidade, Posicao Moradia
  # Need to change because each one of the types of houses is going to be a different column
  
  Moradia <-reactive ({
    #x1 = alvenaria, x2 = madeira, x3 = mista 
    #In this case, I am considering the weight value of the most vulnerable. So, for alvenaria & madeira, the weight is of the madeira. And so on. 
    
    if (x1() == TRUE & x2() == FALSE & x3() == FALSE) {moradia = 0.500518114285714 } # alvenaria
    if (x1() == FALSE & x2() == TRUE & x3() == FALSE) {moradia = 2.792746 } # madeira
    if (x1() == FALSE & x2() == FALSE & x3() == TRUE) {moradia = 3.9606216} # mista
   
    
    
    ### NEED to fix input as Percentage
     if (x1() == TRUE & x2() == TRUE & x3() == FALSE) {moradia = 1.646632057142857 } #alvenaria & madeira
    if (x1() == TRUE & x2() == FALSE & x3() == TRUE)  {moradia = 2.230569857142857} # alvenaria e mista
    if (x1() == FALSE & x2() == TRUE & x3() == TRUE)  {moradia = 3.3766838}# madeira e mista
    if (x1() == TRUE & x2() == TRUE & x3() == TRUE)   {moradia = 2.4179619047619047} # alvenaria, madeira e mista
    
    moradia
  })
  
  # Aterro, lixo, entulho
  Material <- reactive ({
    
    # a = aterro, l=lixo, e = entulho
    if (a() == TRUE & l()==FALSE & e()==FALSE) {material = 5.0015542} # so aterro
    if (l() == TRUE  & a()==FALSE & e()==FALSE) {material = 5.0015542} # so lixo
     if (e() == TRUE & a() == FALSE & l()==FALSE) {material = 5.0015542} # so entulho
    if (a() == TRUE & l()==TRUE & e()==FALSE) {material = 5.0015542} #  aterro & lixo
    if (l() == TRUE  & a()==FALSE & e()==TRUE) {material = 5.0015542} #  lixo & entulho
    if (e() == TRUE & a() == TRUE & l()==FALSE) {material = 5.0015542} #  entulho & aterro
    if (e() == TRUE & a() == TRUE & l()==TRUE) {material = 5.0015542} #  entulho & aterro & lixo
    if (e() == FALSE & a() == FALSE & l()==FALSE) {material = 0} # sem aterro, sem lixo, sem entulho 
    
    material
  })
  
  
  
  Densidade <- reactive ({
    
    if (densidade() == "densidade1") { densidade <- 0.49306412137931 }
    if (densidade() == "densidade2") { densidade <- 1.50720737103448}
    if (densidade() == "densidade3") { densidade <- 2.4373056 }
    if (densidade() == "densidade4") { densidade <- 1.16542428689655}
    
    densidade
  })
  
  
  
  Agua <- reactive ({
    
    drenagem="satisf"
    
    if (servida() == "TRUE") {servida = 5.04447846966967} 
    else if (servida() == "FALSE") {servida = 0} 
   
    if (conc_chuva() == "TRUE") {conc_chuva = 3.030316203003}
    else if (conc_chuva() == "FALSE") {conc_chuva = 0}
  
    if (vazamento() == "TRUE") {vazamento = 6.0424868}
    else  if (vazamento() == "FALSE") {vazamento = 0}
  
    if (fossa() == "TRUE") {fossa = 2.97587938498499}
    else if (fossa() == "FALSE") {fossa = 0}

    
    if (drenagem() == "inexistente") {drenagem = 0.0442203843843844}
    if (drenagem() == "precario") { drenagem = 0.0363238871728872}
    if (drenagem() == "satisf") {drenagem = 0.00719458634825302}
    
    A <- servida + conc_chuva + vazamento + fossa
    if (A > 6.04) {
      A == 6.04
    }
    A
  })
  
  
  # need to find a solution for this one... 
  Vegetacao <- reactive ({
   
    cobertura_urbana=0
    if(arvore() == "TRUE"){arvore <- 0.818966029581993}
    else if (arvore() == "FALSE"){arvore <- 0}
    
    if(arbusto() == "TRUE"){arbusto <- 1.02991182508039}
    else if (arbusto() == "FALSE"){arbusto <- 0}
    
    if(rasteira() == "TRUE" ) {rasteira <- 1.613114906752411}
    else   if(rasteira() == "FALSE" ) {rasteira <- 0}
    
    if (desmatado() == "TRUE" ) {desmatado <- 3.8590672}
    else     if (desmatado() == "FALSE" ) {desmatado <- 0}

    if (bananeira() == "TRUE")(bananeira <- 3.69775570932476)
    else if  (bananeira() == "FALSE")(bananeira <- 0)
    
    if (cobertura_urbana() == "TRUE")(cobertura_urbana <- 1.36494338263666)
    else if (cobertura_urbana() == "FALSE")(cobertura_urbana <- 0)
    
    cobertura <-arvore + rasteira + desmatado + bananeira + cobertura_urbana + arbusto
    
    if (cobertura >3.8590672) {
      cobertura == 3.8590672
    }
    cobertura
  })
  
  
  # Instabilidade do terreno
  Instabilidade  <- reactive ({
    #embarricado=0 
    #trinca_moradia=0
   # trinca_terreno=0
  #  degrau=0
  #  inclinados=0
  #cicatriz=0
    
    if(embarricado() == "TRUE"){embarricado = 13.1}
   else if (embarricado() == "FALSE") {embarricado = 0 }
    
    if(trinca_moradia() == "TRUE" ) {trinca_moradia = 12.3}
  else if (trinca_moradia() == "FALSE") {trinca_moradia=0}
    
    if (trinca_terreno() == "TRUE" ) {trinca_terreno = 20.4}
  else if (trinca_terreno() == "FALSE" ) {trinca_terreno=0}
    
    if (inclinados() == "TRUE") {inclinados = 10.6}
  else if (inclinados() == "FALSE")  {inclinados =0}
    
    if (degrau() == "TRUE") {degrau = 23.7}
  else if (degrau() == "FALSE") {degrau=0}
    
    if (cicatriz() == "TRUE"){cicatriz = 19.8}
  else if (cicatriz() == "FALSE") {cicatriz=0}
    
    instabilidade <- embarricado + trinca_moradia+inclinados+degrau+cicatriz+trinca_terreno
    
    if (instabilidade >5.0015542) {
      instabilidade == 5.0015542
    }
    
    
    
    
    
    instabilidade
  })
  
  
  Posicao_moradia <- reactive( {
    
    if (posicao() == prox_base){prox_base <- 4.1129532 }
    if (posicao() == dist_base){dist_base <-  2.71431205417867}
    if (posicao() == meio_encosta){meio_encosta <-  3.29510371642651}
    if (posicao() == dist_base) {dist_base <- 0.202419}
    if (posicao() == dist_topo) {dist_topo <-0.1452654}
    if (posicao() == encosta_tudo ) {encosta_tudo <- 4.1129532}
   
    
     posicao <- prox_topo + prox_topo + dist_base + dist_topo + meio_encosta + encosta_tudo
     posicao

     
     
     
    
     
     
     
  
    
  })
  
  
  
  risk <-reactive ({
    final <- (Agua() + Angulo() + Densidade() + Instabilidade() + Material() + Moradia() + Vegetacao())
    if (final < 36) {return("R1")}
    if (final >= 36 & final < 53) {return("R2")}
    if (final >= 53 & final < 66) {return("R3")}
    if (final >= 66) {return("R4")}
    final
  })   
  
  output$grau_risco <- renderText( paste("Grau de risco igual", risk()))
  output$instab <- renderText( paste("Instabilidade", Instabilidade()))
  
  
  
  
  
  
  
  
  
  
  
  
  
  ### Reactive expression to create dataframe of all inputs values for html and visualizatin
  
  
  textValues <- reactive({
    
    data.frame (
      
      Caracteristica = c("Município", 
                         "Área",
                         "Bairro",
                         "Setor", 
                         "Equipe",
                         "Data",
                         "Unidade de Análise",
                         "Localização", 
                         "Moradores",
                         "Acesso ao local",
                         "Moradia Alvenaria",
                         "Moradia Madeira",
                         "Moradia Mista",
                         "Observação moradias",
                         "Densidade da ocuapação", 
                         "Condição da Via", 
                         "Observação da via",
                         "Encosta Natural",
                         "Talude de Corte",
                         "Altura máxima da encosta natural",
                         "Distância da moradia",
                         "Ângulo de inclinação", 
                         "Posição da  moradia",
                         "Geologia", 
                         "Solo",
                         "Cobertura natural",
                         "Solo Residual",
                         "Saprólito",
                         "Rocha alterada",
                         "Rocha sã", 
                         "Aterro",
                         "Lixo",
                         "Entulho",
                         "Lançamento de água servida em superfície",
                         "Concentração de água de chuva em superfície",
                         "Vazamento da tubulação",
                         "Fossa séptica",
                         "Sistema de drenagem",
                         "Presença de árvores",
                         "Vegetação arbustiva",
                         "Vegetação rasteira",
                         "Área desmatada/solo exposto",
                         "Bananeira",
                         "Cobertura urbana",
                         "Muro embarrigado", 
                         "Trinca na moradia", 
                         "Árvores, postes, e muros inclinados",
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
        input$select_solo,
        input$select_cobertura_natural,
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
        input$select_arbusto,
        input$select_rasteira,
        input$select_desmatado,
        input$select_bananeira,
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
    data.frame("Município" = input$txt_mun, 
               "Área" = input$txt_area, 
               "Bairro"= input$txt_bairro, 
               "Setor" = input$txt_setor,
               "Equipe" = input$txt_equip, 
               "Data" = as.character(input$date), 
               "UA" = input$select_UA, 
               "Localização" = input$txt_loc, 
               "Moradores contato" = input$txt_moradores, 
               "Acesso" = input$txt_acesso, 
               "Moradia alvernaria" = input$select_moradia_alvenaria,
               "Moradia madeira" = input$select_moradia_madeira,
               "Moradia mista" = input$select_moradia_mista,
               "Moradia (observações)" = input$txt_moradia,
               "Densidade" = input$select_densidade,
               "Via de acesso" = input$select_via, 
               "Via de acesso (observações)" = input$txt_via,
               "Encosta Natural" = input$select_encosta, 
               "Talude de corte" = input$select_talude,
               "Altura máxima do talude" = input$txt_h_max, 
               "Distância Moradia" = input$txt_dist_moradia, 
               "Ângulo de inclinação" = input$select_angle, 
               "Posição/Localização das moradias" = input$select_posicao_moradia,
               "Geologia" = input$select_geol, 
               "Solo" = input$select_solo, 
               "Cobertura Natural" = input$cobertura_natural, 
               "Solo residual" = input$select_residual,
               "Saprólito" = input$select_saprolito, 
               "Rocha alterada" = input$select_rocha_alterada,
               "Rocha sã" = input$select_rocha_sa,
               "Aterro" = input$select_aterro,
               "Lixo" = input$select_lixo,
               "Entulho" =input$select_entulho,
               "Lançamento de água servida em superfície" = input$select_lancamento,
               "Concentração de água de chuva em superfície" = input$select_chuva,
               "Vazamento da tubulação" = input$select_vazamento,
               "Fossa séptica" = input$select_fossa,
               "Drenagem" = input$select_drenag, 
               "Presença de árvores"  = input$select_arvore,
               "Presença de arbusto" = input$select_arbusto,
               "Vegetação rasteira" = input$select_rasteira,
               "Área desmatada/solo exposto" = input$select_desmatado,
               "Bananeira" = input$select_bananeira,
               "Cobertura urbana" = input$select_urbana,
               "Muro embarrigado" = input$select_embarrigado,
               "Trinca moradia" = input$select_trinca_moradia,
               "[Arvores, postes, e muros inclinados" = input$select_inclinados,
               "Degrau de abatimento" = input$select_degrau,
               "Cicatriz de escorregamento" = input$select_cicatriz,
               "Trinca no terreno" =input$select_trinca_terreno,
               "Grau de risco" =  risk())
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
      
      params <- list(     a= input$txt_mun, #1 
                          b = input$txt_area, 
                          c= input$txt_bairro, 
                          d=input$txt_setor,
                          e=input$txt_equip, 
                          f= as.character(input$date),
                          g = input$select_UA, 
                          h=input$txt_loc, 
                          i = input$txt_moradores, 
                          j =input$txt_acesso, #10 
                          k1 = input$select_moradia_alvenaria,
                          k2 = input$select_moradia_madeira,
                          k3 = input$select_moradia_mista,
                          l= input$txt_moradia,
                          m= input$select_densidade,
                          n = input$select_via, 
                          o =  input$txt_via,
                          p = input$select_encosta, 
                          p1 = input$select_talude,
                          q =input$txt_h_max,  #20
                          r =input$txt_dist_moradia, 
                          s = input$select_angle,
                          t =input$select_posicao_moradia,
                          u =input$select_geol,
                          u1 = input$select_solo, 
                          u2 =input$select_cobertura_natural, 
                          v = input$select_residual,
                          v1 = input$select_saprolito, 
                          v2 = input$select_rocha_alterada,
                          v3 = input$select_rocha_sa, #30
                          v4 = input$select_aterro,
                          v5 = input$select_lixo, 
                          v6 = input$select_entulho, 
                          x = input$select_lancamento,
                          x1 = input$select_chuva,
                          x2 = input$select_vazamento,
                          x3 = input$select_fossa,
                          z = input$select_drenag, 
                          y = input$select_arvore,
                          y5 = input$select_arbusto, #40 
                          y1 = input$select_rasteira,
                          y2 = input$select_desmatado,
                          y4 = input$select_bananeira, #43
                          y3 = input$select_urbana,
                          w = input$select_embarrigado,
                          w1 = input$select_trinca_moradia,
                          w2 = input$select_inclinados,
                          w3 = input$select_degrau,
                          w5 = input$select_cicatriz,
                          w6 = input$select_trinca_terreno, #50
                          w7 = risk()
                         
                         
                         
                         
                         
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
      
      params <- list(    a= input$txt_mun, #1 
                         b = input$txt_area, 
                         c= input$txt_bairro, 
                         d=input$txt_setor,
                         e=input$txt_equip, 
                         f= as.character(input$date),
                         g = input$select_UA, 
                         h=input$txt_loc, 
                         i = input$txt_moradores, 
                         j =input$txt_acesso, #10 
                         k1 = input$select_moradia_alvenaria,
                         k2 = input$select_moradia_madeira,
                         k3 = input$select_moradia_mista,
                         l= input$txt_moradia,
                         m= input$select_densidade,
                         n = input$select_via, 
                         o =  input$txt_via,
                         p = input$select_encosta, 
                         p1 = input$select_talude,
                         q =input$txt_h_max,  #20
                         r =input$txt_dist_moradia, 
                         s = input$select_angle,
                         t =input$select_posicao_moradia,
                         u =input$select_geol,
                         u1 = input$select_solo, 
                         u2 =input$select_cobertura_natural, 
                         v = input$select_residual,
                         v1 = input$select_saprolito, 
                         v2 = input$select_rocha_alterada,
                         v3 = input$select_rocha_sa, #30
                         v4 = input$select_aterro,
                         v5 = input$select_lixo, 
                         v6 = input$select_entulho, 
                         x = input$select_lancamento,
                         x1 = input$select_chuva,
                         x2 = input$select_vazamento,
                         x3 = input$select_fossa,
                         z = input$select_drenag, 
                         y = input$select_arvore,
                         y5 = input$select_arbusto, #40 
                         y1 = input$select_rasteira,
                         y2 = input$select_desmatado,
                         y4 = input$select_bananeira, #43
                         y3 = input$select_urbana,
                         w = input$select_embarrigado,
                         w1 = input$select_trinca_moradia,
                         w2 = input$select_inclinados,
                         w3 = input$select_degrau,
                         w5 = input$select_cicatriz,
                         w6 = input$select_trinca_terreno, #50
                         w7 = risk()
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
        input$txt_mun,  #1
        input$txt_area, 
        input$txt_bairro, 
        input$txt_setor,
        input$txt_equip, 
        as.character(input$date), 
        input$select_UA, 
        input$txt_loc, 
        input$txt_moradores, 
        input$txt_acesso, #10
        input$select_moradia_alvenaria,
        input$select_moradia_madeira,
        input$select_moradia_mista,
        input$txt_moradia,
        input$select_densidade,
        input$select_via, 
        input$txt_via,
        input$select_encosta, 
        input$select_talude,
        input$txt_h_max,  #20
        input$txt_dist_moradia, 
        input$select_angle, 
        input$select_posicao_moradia,
        input$select_geol,
        input$select_solo, 
        input$select_cobertura_natural, 
        input$select_residual,
        input$select_saprolito, 
        input$select_rocha_alterada,
        input$select_rocha_sa,
        input$select_aterro,
        input$select_lixo, #30
        input$select_entulho, 
        input$select_lancamento,
        input$select_chuva,
        input$select_vazamento,
        input$select_fossa,        
        input$select_drenag, 
        input$select_arvore,
        input$select_arbusto,
        input$select_rasteira,
        input$select_desmatado,
        input$select_bananeira, #41
        input$select_urbana,
        input$select_embarrigado, 
        input$select_trinca_moradia,
        input$select_inclinados,
        input$select_degrau,
        input$select_cicatriz,
        input$select_trinca_terreno, #48
        risk()),
        
        colnames(df) <- c("Município", #1
                          "Área",
                          "Bairro",
                          "Setor", 
                          "Equipe",
                          "Data",
                          "UA",
                          "Localização", 
                          "Moradores",
                          "Acesso ao local", #10
                          "Moradia Alvenaria",
                          "Moradia Madeira",
                          "Moradia Mista",
                          "Observacao moradias",
                          "Densidade da ocuapação", 
                          "Via", 
                          "Observação da via",
                          "Tipo de encosta",
                          "Talude de corte",
                          "Altura máxima da encosta natural", #20
                          "Distância da moradia",
                          "Ângulo de inclinação", 
                          "Posição da  moradia",
                          "Geologia", 
                          "Solo", 
                          "Cobertura Natural", 
                          "Solo residual",
                          "Saprólito", 
                          "Rocha alterada",
                          "Rocha sã", 
                          "Aterro",
                          "Lixo", #32 (inclui solo e cob natural)
                          "Entulho",
                          "Lançamento de água servida",
                          "Concentração de água de chuva",
                          "Vazamento",
                          "Fossa séptica",
                          "Tipo de drenagem" ,
                          "Presença de árvores",
                          "Presença de vegetação arbustiva",
                          "Presença de vegetação rasteira",
                          "Solo exposto/vegetação desmatada",
                          "Bananeira", #43
                          "Cobertura urbana",
                          "Muro embarrigado", 
                          "Trinca na moradia", 
                          "Árvores, postes, e muros inclinados",
                          "Degrau de abatimento",
                          "Cicatrizes de escorregamento",
                          "Trinca no terreno", 
                          "Grau de risco" #51
                          
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


