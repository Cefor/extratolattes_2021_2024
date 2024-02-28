
tecnica <- function(id, xml_data,
                              pontos_tec = function(estrato) estrato,
                              qualis, qualis_tecnica, nome, nome_area, ano_ini, ano_fim){

  producao_tec <- NULL
 
  # DEMAIS-TIPOS-DE-PRODUCAO-TECNICA
  ptec <- xml_data$"PRODUCAO-TECNICA"$"DEMAIS-TIPOS-DE-PRODUCAO-TECNICA"

  # para todos os itens em
  #`DESENVOLVIMENTO-DE-MATERIAL-DIDATICO-OU-INSTRUCIONAL`
  for(i in (1:length(ptec))[names(ptec) == "DESENVOLVIMENTO-DE-MATERIAL-DIDATICO-OU-INSTRUCIONAL"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-MATERIAL-DIDATICO-OU-INSTRUCIONAL"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-MATERIAL-DIDATICO-OU-INSTRUCIONAL"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
    
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-MATERIAL-DIDATICO-OU-INSTRUCIONAL"["TITULO"])
      titulo <- str_trim(encode_xml2(titulo))
      
      natureza <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-MATERIAL-DIDATICO-OU-INSTRUCIONAL"["NATUREZA"])
      natureza <- str_trim(str_to_lower(encode_xml2(natureza)))

      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)
      

      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = paste("desenvolvimento de material didatico","-", natureza),
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }

  # para todos os itens em
  #`RELATORIO-DE-PESQUISA`
  for(i in (1:length(ptec))[names(ptec) == "RELATORIO-DE-PESQUISA"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-RELATORIO-DE-PESQUISA"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-RELATORIO-DE-PESQUISA"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-RELATORIO-DE-PESQUISA"["TITULO"])
      
      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)

      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = "relatorio conclusivo de pesquisa aplicada",
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  
  # para todos os itens em
  #`PROGRAMA-DE-RADIO-OU-TV`
  for(i in (1:length(ptec))[names(ptec) == "PROGRAMA-DE-RADIO-OU-TV"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-PROGRAMA-DE-RADIO-OU-TV"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-PROGRAMA-DE-RADIO-OU-TV"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PROGRAMA-DE-RADIO-OU-TV"["TITULO"])
      titulo <- str_trim(encode_xml2(titulo))
      atuacao <- ptec[[i]]$"INFORMACOES-ADICIONAIS"["DESCRICAO-INFORMACOES-ADICIONAIS"] # <PRODUCAO> ou <PARTICIPACAO>
      tipo <- "participacao em programa de radio ou TV"
      
      if(!is.null(atuacao)){ # se diferente de NULL, recupera a primeira linha
        atuacao <- str_split(atuacao[1],"\n")[[1]][1]
        if(atuacao == "<PRODUCAO>") tipo <- "producao de programas de midia"
      }
      
      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)

      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = tipo,
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  # para todos os itens em
  # MIDIA-SOCIAL-WEBSITE-BLOG
  for(i in (1:length(ptec))[names(ptec) == "MIDIA-SOCIAL-WEBSITE-BLOG"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"["TITULO"])
      titulo <- str_trim(encode_xml2(titulo))
      tipo <- str_to_lower(as.vector(ptec[[i]]$"DADOS-BASICOS-DA-MIDIA-SOCIAL-WEBSITE-BLOG"["NATUREZA"]))

      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)
      
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = tipo,
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  
  # para todos os itens em
  #`APRESENTACAO-DE-TRABALHO`
  for(i in (1:length(ptec))[names(ptec) == "APRESENTACAO-DE-TRABALHO"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-APRESENTACAO-DE-TRABALHO"["TITULO"])
      titulo <- str_trim(encode_xml2(titulo))
      
      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)
      
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = "apresentacao de trabalho",
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  # para todos os itens em
  #`CURSO-DE-CURTA-DURACAO-MINISTRADO`
  for(i in (1:length(ptec))[names(ptec) == "CURSO-DE-CURTA-DURACAO-MINISTRADO"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["TITULO"])
      titulo <- str_trim(encode_xml2(titulo))
      nivel <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["NIVEL-DO-CURSO"])
      atuacao <- as.vector(ptec[[i]]$"DETALHAMENTO-DE-CURSOS-CURTA-DURACAO-MINISTRADO"["PARTICIPACAO-DOS-AUTORES"])
      
      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)
      
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = str_to_lower(paste("curso de curta duracao ministrado -", nivel, "-", atuacao)),
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }  
  
  # para todos os itens em
  #`ORGANIZACAO-DE-EVENTO`
  for(i in (1:length(ptec))[names(ptec) == "ORGANIZACAO-DE-EVENTO"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"["TITULO"])
      titulo <- str_trim(encode_xml2(titulo))
      
      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)
      
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = "organizacao de evento",
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }  
  

  # para todos os itens em
  #`OUTRA-PRODUCAO-TECNICA`
  for(i in (1:length(ptec))[names(ptec) == "OUTRA-PRODUCAO-TECNICA"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"["TITULO"])
      titulo <- str_trim(encode_xml2(titulo))
      natureza <- str_trim(str_to_lower(as.vector(ptec[[i]]$"DADOS-BASICOS-DE-OUTRA-PRODUCAO-TECNICA"["NATUREZA"])))
      
      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)

      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = natureza,
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  
  ptec <- xml_data$"PRODUCAO-TECNICA"
  
  # para todos os itens em
  # TRABALHO-TECNICO
  for(i in (1:length(ptec))[names(ptec) == "TRABALHO-TECNICO"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){

      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"["TITULO-DO-TRABALHO-TECNICO"])
      titulo <- str_trim(encode_xml2(titulo))
      tipo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-TRABALHO-TECNICO"["NATUREZA"])

      # quando o PARECER for "parecer de artigo de revista"
      if(tipo == "PARECER" & as.vector(ptec[[i]]$"DETALHAMENTO-DO-TRABALHO-TECNICO"["FINALIDADE"]) == "Parecer de artigo de revista"){
          # recupera o ISSN no inicio do titulo
          issn <- gsub("-", "", str_sub(titulo, 1, 9))
          # verifica o estrato na area especificada no parametro nome_area
          eqcp <- as.character(qualis[(qualis$Area %in% nome_area ) & (qualis$ISSN == issn), "Estrato"][1])
          if(is.na(eqcp)) eqcp <- "--"
          tipo <- paste("Parecer de artigo de revista com estrato", eqcp)
          
      } else if(tipo == "OUTRA"){
        
        # verificar se eh LAUDO TECNICO
        
        info_adicional <- ptec[[i]]$"INFORMACOES-ADICIONAIS"
        
        if(!is.null(info_adicional)){ # se diferente de NULL, recupera a primeira linha
          info_adicional <- str_split(info_adicional[1],"\n")[[1]][1]
          
          if(info_adicional == "<LAUDO TECNICO>"){
            tipo <- "Laudo tecnico"
          }
        }
        
      }
      
      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)

      if(!(ano >= ano_ini & ano <= ano_fim)) {
        pontos <- 0
      }

      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = str_to_lower(tipo),
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  

  # para todos os itens em
  # SOFTWARE
  for(i in (1:length(ptec))[names(ptec) == "SOFTWARE"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-SOFTWARE"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-SOFTWARE"['FLAG-RELEVANCIA']

    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-SOFTWARE"["TITULO-DO-SOFTWARE"])
      titulo <- str_trim(encode_xml2(titulo))
      
      if("REGISTRO-OU-PATENTE" %in% names(ptec[[i]]$"DETALHAMENTO-DO-SOFTWARE")){
        tipo <- "registro de software"
        titulo <- paste0(titulo, " - Registro (", 
                         ptec[[i]]$"DETALHAMENTO-DO-SOFTWARE"$"REGISTRO-OU-PATENTE"["CODIGO-DO-REGISTRO-OU-PATENTE"],
                         ")")
      } else tipo <- "software"

      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)
      
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = tipo,
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  # para todos os itens em
  # PROCESSOS-OU-TECNICAS
  for(i in (1:length(ptec))[names(ptec) == "PROCESSOS-OU-TECNICAS"]){
    
    ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"["ANO"])
    
    # flag relevancia
    fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"['FLAG-RELEVANCIA']
    
    if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
      
      titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"["TITULO-DO-PROCESSO"])
      titulo <- str_trim(encode_xml2(titulo))
      
      tipo <- str_to_lower(paste0("Processo ou tecnica - ",
                     ptec[[i]]$"DADOS-BASICOS-DO-PROCESSOS-OU-TECNICAS"["NATUREZA"]))
      
      # novo qualis
      # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
      TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
      TT <- str_sub(paste0(TT,"--"),1,2)
      pontos <- pontos_tec(TT)
      
      if(!(ano >= ano_ini & ano <= ano_fim)){
        pontos <- 0
      }
      
      ap <- data.frame(
        id = id,
        ano = ano,
        titulo = titulo,
        tipo = tipo,
        estrato = TT,
        pontos = pontos,
        flag_relevancia = fl_rlvnc
      )
      
      producao_tec <- rbind(producao_tec, ap, row.names = NULL)
    }
  }
  
  
  
  ## REVER ESSE MODULO - integrar em producao bibliografica
  ptec <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"DEMAIS-TIPOS-DE-PRODUCAO-BIBLIOGRAFICA"
  # para todos os itens em
  # PREFACIO-POSFACIO
  if(length(ptec) > 0){
    for(i in (1:length(ptec))[names(ptec) == "PREFACIO-POSFACIO"]){
      
      ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["ANO"])
      
      # flag relevancia
      fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"['FLAG-RELEVANCIA']
      
      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
        
        titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["TITULO"])
        titulo <- str_trim(encode_xml2(titulo))
        tipo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["TIPO"])
        natureza <- as.vector(ptec[[i]]$"DADOS-BASICOS-DO-PREFACIO-POSFACIO"["NATUREZA"])
        
        fl_tecnica <- as.vector(ptec[[i]]$"INFORMACOES-ADICIONAIS"["DESCRICAO-INFORMACOES-ADICIONAIS"])
        if(!is.null(fl_tecnica)){ # se diferente de NULL, recupera a primeira linha
          fl_tecnica <- str_split(fl_tecnica,"\n")[[1]][1]
          if(fl_tecnica == "<OBRA TECNICA>"){
            tipo <- str_to_lower(paste(tipo, "de", natureza, "-", gsub(">", "", gsub("<", "", fl_tecnica))))  
          }
        } else tipo <- str_to_lower(paste(tipo, "de", natureza))
          
        # novo qualis
        # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
        TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
        TT <- str_sub(paste0(TT,"--"),1,2)
        pontos <- pontos_tec(TT)
        
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }
          
        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          tipo = tipo,
          estrato = TT,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )
        
        producao_tec <- rbind(producao_tec, ap, row.names = NULL)
      }
    }
  }
  
  # para todos os itens em
  # TRADUCAO
  if(length(ptec) > 0){
    for(i in (1:length(ptec))[names(ptec) == "TRADUCAO"]){
      
      ano <- as.numeric(ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"["ANO"])
      
      # flag relevancia
      fl_rlvnc <- ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"['FLAG-RELEVANCIA']

      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
        
        titulo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"["TITULO"])
        titulo <- str_trim(encode_xml2(titulo))
        tipo <- as.vector(ptec[[i]]$"DADOS-BASICOS-DA-TRADUCAO"["NATUREZA"])
        
        # novo qualis
        # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
        TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
        TT <- str_sub(paste0(TT,"--"),1,2)
        pontos <- pontos_tec(TT)
        
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }
        
        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          tipo = paste("traducao de ", str_to_lower(tipo)),
          estrato = TT,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )
        
        producao_tec <- rbind(producao_tec, ap, row.names = NULL)
      }
    }
  }
  
  
  ptec <- xml_data$"DADOS-COMPLEMENTARES"$"PARTICIPACAO-EM-EVENTOS-CONGRESSOS"
  # para todos os itens em
  # PARTICIPACAO-EM-EVENTOS-CONGRESSOS
  # Participacao em mesa redonda; Palestrante, conferencista
  if(length(ptec) > 0){
    for(i in (1:length(ptec))){
      
      ano <- as.numeric(ptec[[i]][[1]]["ANO"])
      
      # flag relevancia
      fl_rlvnc <- ptec[[i]][[1]]['FLAG-RELEVANCIA']

      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
        
        titulo <- as.vector(ptec[[i]][[1]]["TITULO"])
        titulo <- str_trim(encode_xml2(titulo))
        natureza <- as.vector(ptec[[i]][[1]]["NATUREZA"])
        
        tipo_participacao <- ptec[[i]][[1]]["TIPO-PARTICIPACAO"]
        forma_participacao <- ptec[[i]][[1]]["FORMA-PARTICIPACAO"]
        
        # novo qualis
        # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
        TT <- qualis_tecnica[qualis_tecnica$Titulo == str_to_lower(titulo) & qualis_tecnica$Ano == ano & qualis_tecnica$Autor == nome, "Estrato"]
        TT <- str_sub(paste0(TT,"--"),1,2)
        pontos <- pontos_tec(TT)
        
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }

        ap <- data.frame(
          id = id,
          ano = ano,
          titulo = titulo,
          tipo = str_to_lower(paste(natureza, "-", forma_participacao, "-", tipo_participacao)),
          estrato = TT,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )
        
        producao_tec <- rbind(producao_tec, ap, row.names = NULL)
      }
    }
  }
  
  

  
  
  
  #####################################
  # insere linha de totais
  ap <- data.frame(
    id = id,
    ano = "",
    titulo = "",
    tipo = "",
    estrato = "TOTAL",
    pontos = sum(producao_tec$pontos, na.rm = TRUE),
    flag_relevancia = ""
  )
  producao_tec <- rbind(producao_tec, ap, row.names = NULL)
  
  # retorna o dataframe
  producao_tec
}