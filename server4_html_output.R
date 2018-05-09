


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
