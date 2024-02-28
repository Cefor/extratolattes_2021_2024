
artigos <- function(id, nome, xml_data, 
                    pontos_artigo = function(eq) eq,
                    qualis, nome_area, ano_ini, ano_fim){

  producao_artigos <- NULL
  
  n <- length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS")
  
  if(n > 0){
    for(i in 1:n){
      
      # identifica o ano do artigo
      ano = xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-ARTIGO"["ANO-DO-ARTIGO"]
      ano <- as.vector(ano) # transforma o named vector em somente vetor
      # flag relevancia
      fl_rlvnc <- xml_data$`PRODUCAO-BIBLIOGRAFICA`$`ARTIGOS-PUBLICADOS`[[i]]$`DADOS-BASICOS-DO-ARTIGO`['FLAG-RELEVANCIA']
      
      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc == "SIM"){
        # identifica o periodico e recupera o estrato qualis para CP
        issn <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-ARTIGO"["ISSN"]
        # verifica o estrato na area especificada em no parametro nome_area
        eqcp <- as.character(qualis[(qualis$Area %in% nome_area ) & (qualis$ISSN == issn), "Estrato"][1])
        if(is.na(eqcp)) { eqcp = "--" }
        # verifica o estrato mais alto do periodico
        eq <- as.character(levels(as.factor(as.character((qualis[qualis$ISSN == issn, "Estrato"])))))
        eq <- eq[order(eq)][1]
        # recupera a(s) area(s) do estrato mais alto
        if(is.na(eq)) { 
          eq = "--" 
          areas = "--"
        } else {
          area <- as.character(levels(as.factor(qualis[(qualis$Estrato == eq) & (qualis$ISSN == issn), "Area"])))
          for(j in 1:length(area)){
            if(j == 1) { 
              areas <- area[j]
            } else {
              areas <- paste0(areas,"; ",area[j])
            }
          }
        }
        # recupera o titulo do artigo 
        titulo <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-ARTIGO"["TITULO-DO-ARTIGO"] 
        
        # recupera o meio de publicacao do artigo
        meio <- str_to_lower(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-ARTIGO"["MEIO-DE-DIVULGACAO"])
        meio <- sub("_", " ", meio)
        periodico <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"ARTIGOS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-ARTIGO"["TITULO-DO-PERIODICO-OU-REVISTA"] 
        
        pontos <- pontos_artigo(eqcp)  

        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }

        ap <- data.frame(
          id = id,
          nome = nome,
          ano = ano,
          titulo = titulo,
          issn = paste0(str_sub(issn,1,4),"-",str_sub(issn,5,8)),
          periodico = periodico,
          meio = meio,
          estrato_cp = eqcp,
          maior_estrato = eq,
          areas = areas,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )
        
        producao_artigos <- rbind(producao_artigos, ap, row.names = NULL)
      }
    }
    
    # consolida pontuacao do bloco
    pontos <- producao_artigos$pontos
    if(length(pontos) == 0) {
      titulo = "NAO HA PRODUCAO NO PERIODO."
    } else {
      titulo = ""
    }
    pontos <- sum(pontos, na.rm = TRUE)
    
  } else {
    pontos <- 0
    titulo = "NAO HA PRODUCAO NO PERIODO."
  }
  
  # insere linha de totais
  ap <- data.frame(
    id = id,
    nome = nome,
    ano = "",
    titulo = titulo,
    issn = "",
    periodico = "",
    meio = "",
    estrato_cp = "",
    maior_estrato = "",
    areas = "TOTAL",
    pontos = pontos,
    flag_relevancia = ""
  )
  producao_artigos <- rbind(producao_artigos, ap, row.names = NULL)
  
  producao_artigos

}