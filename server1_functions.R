

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

