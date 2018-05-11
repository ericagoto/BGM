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

####to run App use:
##runApp("newdir")
#on the console


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
                 checkboxInput("select_bananeira", label = "bananeia"),
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









