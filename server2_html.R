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