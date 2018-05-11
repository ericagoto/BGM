

####to run App use:
##runApp("newdir")
#on the console



############################## Define SERVER #############################

server <- function(input, output) {
  
  
  
  ############# Functions ##################
  x <- reactive ({ input$select_angle
    
  })
  x_geol <- reactive({ input$select_geol})
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
  vegetacao <- reactive({input$select_rasteira})
  rasteira <- reactive({input$select_rasteira})
  desmatado <-reactive({input$select_desmatado})
  cobertura_urbana <- reactive({input$select_urbana})
  bananeira <-reactive({input$select_bananeira})
  
  
  
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
    if (a() == TRUE) {aterro = 1}
    else if (a() == FALSE) {aterro = 0}
    
    if (l() == TRUE) {lixo = 1}
    else if (l() == FALSE) {lixo = 0}
    
    if (e() == TRUE) {entulho = 1}
    else if (e() == FALSE) {entulho = 0}
    
    material <- 2.8959*aterro + 2.8959*lixo + 2.8959*entulho
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
    
    drenagem="satisf"
    
    if (servida() == "TRUE") {servida = 0.9726108} 
    else if (servida() == "FALSE") {servida = 0} 
    
    if (conc_chuva() == "TRUE") {conc_chuva = 0.5842662}
    else if (conc_chuva() == "FALSE") {conc_chuva = 0}
    
    if (vazamento() == "TRUE") {vazamento = 1.1650338}
    else  if (vazamento() == "FALSE") {vazamento = 0}
    
    if (fossa() == "TRUE") {fossa = 0.5737704}
    else if (fossa() == "FALSE") {fossa = 0}
    
    
    if (drenagem() == "inexistente") {drenagem = 0.1022710752}
    if (drenagem() == "precario") { drenagem = 0.0840083832}
    if (drenagem() == "satisf") {drenagem = 0.0166393416}
    
    A <- servida + conc_chuva + vazamento + fossa
    A
  })
  
  
  # need to find a solution for this one... 
  Vegetacao <- reactive ({
    
    cobertura_urbana=0
    if(arvore() == "TRUE"){arvore <- 1}
    else if (arvore() == "FALSE"){arvore <- 0}
    
    if(rasteira() == "TRUE" ) {rasteira <- 1}
    else   if(rasteira() == "FALSE" ) {rasteira <- 0}
    
    if (desmatado() == "TRUE" ) {desmatado <- 1}
    else     if (desmatado() == "FALSE" ) {desmatado <- 0}
    
    if (bananeira() == "TRUE")(bananeira <- 1)
    else if  (bananeira() == "FALSE")(bananeira <- 0)
    
    if (cobertura_urbana() == "TRUE")(cobertura_urbana <- 1)
    else if (cobertura_urbana() == "FALSE")(cobertura_urbana <- 0)
    
    cobertura <-   (0.022344*arvore+0.1854552*rasteira+0.6948984*desmatado+0.290472*bananeira+0.245784*cobertura_urbana)
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
    
    if(embarricado() == "TRUE"){embarricado = 1}
    else if (embarricado() == "FALSE") {embarricado = 0 }
    
    if(trinca_moradia() == "TRUE" ) {trinca_moradia = 1}
    else if (trinca_moradia() == "FALSE") {trinca_moradia=0}
    
    if (trinca_terreno() == "TRUE" ) {trinca_terreno = 1}
    else if (trinca_terreno() == "FALSE" ) {trinca_terreno=0}
    
    if (inclinados() == "TRUE") {inclinados = 1}
    else if (inclinados() == "FALSE")  {inclinados =0}
    
    if (degrau() == "TRUE") {degrau = 1}
    else if (degrau() == "FALSE") {degrau=0}
    
    if (cicatriz() == "TRUE"){cicatriz = 1}
    else if (cicatriz() == "FALSE") {cicatriz=0}
    
    instabilidade <- 7.5849*embarricado+7.1217*trinca_moradia+6.1374*inclinados+13.7223*degrau+11.4642*cicatriz+11.8116*trinca_terreno
    instabilidade
  })
  
  
  Posicao_moradia <- reactive( {
    
    if (posicao() == encosta){posicao <- (0.8263458+0.5453406+0.6620292) }
    if (posicao() == dist_base) {posicao <- 0.202419}
    if (posicao() == dist_topo) {posicao <-0.1452654}
    posicao
  })
  
  
  
  risk <-reactive ({
    final <- Agua() + Angulo() + Densidade() + Instabilidade() + Material() + Moradia() + Vegetacao()
    if (final < 7.11) {return("R1")}
    if (final >= 7.11 & final <12) {return("R2")}
    if (final >=12 & final <37) {return("R3")}
    if (final >=37) {return("R4")}
    final
  })   
  
  output$grau_risco <- renderText( paste("Grau de risco igual", risk()))
  output$instab <- renderText( paste("Instabilidade", Instabilidade()))
  
  
  
  
  
  
  
  
  
  
  
  
  
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
                         "Bananeira",
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
               "Bananeira" = input$select_bananeira,
               "Cobertura_urbana" = input$select_urbana,
               "Muro embarrigado" = input$select_embarrigado,
               "Trinca moradia" = input$select_trinca_moradia,
               "Arvores, postes, e muros inclinados" = input$select_inclinados,
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
                         v = input$select_residual,
                         v1 = input$select_saprolito, 
                         v2 = input$select_rocha_alterada,
                         v3 = input$select_rocha_sa,
                         v4 = input$select_aterro,
                         v5 = input$select_lixo, #30
                         v6 = input$select_entulho, 
                         x = input$select_lancamento,
                         x1 = input$select_chuva,
                         x2 = input$select_vazamento,
                         x3 = input$select_fossa,
                         z = input$select_drenag, 
                         y = input$select_arvore,
                         y1 = input$select_rasteira,
                         y2 = input$select_desmatado,
                         y4 = input$select_bananeira, #40
                         y3 = input$select_urbana,
                         w = input$select_embarrigado,
                         w1 = input$select_trinca_moradia,
                         w2 = input$select_inclinados,
                         w3 = input$select_degrau,
                         w5 = input$select_cicatriz,
                         w6 = input$select_trinca_terreno #47
                         
                         
                         
                         
                         
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
      
      params <- list(   a= input$txt_mun, #1 
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
                        v = input$select_residual,
                        v1 = input$select_saprolito, 
                        v2 = input$select_rocha_alterada,
                        v3 = input$select_rocha_sa,
                        v4 = input$select_aterro,
                        v5 = input$select_lixo, #30
                        v6 = input$select_entulho, 
                        x = input$select_lancamento,
                        x1 = input$select_chuva,
                        x2 = input$select_vazamento,
                        x3 = input$select_fossa,
                        z = input$select_drenag, 
                        y = input$select_arvore,
                        y1 = input$select_rasteira,
                        y2 = input$select_desmatado,
                        y4 = input$select_bananeira, #40
                        y3 = input$select_urbana,
                        w = input$select_embarrigado,
                        w1 = input$select_trinca_moradia,
                        w2 = input$select_inclinados,
                        w3 = input$select_degrau,
                        w5 = input$select_cicatriz,
                        w6 = input$select_trinca_terreno #47
                        
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
        input$select_rasteira,
        input$select_desmatado,
        input$select_bananeira, #40
        input$select_urbana,
        input$select_embarrigado, 
        input$select_trinca_moradia,
        input$select_inclinados,
        input$select_degrau,
        input$select_cicatriz,
        input$select_trinca_terreno, #47
        risk()),
        
        colnames(df) <- c("Municipio", #1
                          "Area",
                          "Bairro",
                          "Setor", 
                          "Equipe",
                          "Data",
                          "UA",
                          "Localizacao", 
                          "Moradores",
                          "Acesso ao local", #10
                          "Moradia Alvenaria",
                          "Moradia Madeira",
                          "Moradia Mista",
                          "Observacao moradias",
                          "Densidade da ocuapacao", 
                          "Via", 
                          "Observacao da via",
                          "Tipo de encosta",
                          "Talude de corte",
                          "Altura maxima da encosta natural", #20
                          "Distancia da moradia",
                          "Angulo de inclinacao", 
                          "Posicao da  moradia",
                          "Geologia", 
                          "Solo residual",
                          "Saprolito", 
                          "Rocha alterada",
                          "Rocha sa", 
                          "Aterro",
                          "Lixo", #30
                          "Entulho",
                          "Lancamento de agua servida",
                          "Concentracao de agua de chuva",
                          "Vazamento",
                          "Fossa septica",
                          "Tipo de drenagem" ,
                          "Presenca de arvores",
                          "Presenca de vegetacao rasteira",
                          "Solo exposto/vegetacao desmatada",
                          "Bananeira", #40
                          "Cobertura urbana",
                          "Muro embarrigado", 
                          "Trinca na moradia", 
                          "Arvores, postes, e muros inclinados",
                          "Degrau de abatimento",
                          "Cicatrizes de escorregamento",
                          "Trinca no terreno", 
                          "Grau de risco" #48
                          
        )
        , ".csv", sep = "")
    },
    content = function(file) {
      write.csv(textValues2(), file, row.names = FALSE)
    }
  )
  
  
  
  
  
  ######################## end download excel ###################################
  
}






