
jornal_revista <- function(id, nome, xml_data, qualis_biblio,
                    pontos_jornal_revista = function(FUN, natCeT, estrato) {FUN(natCeT, estrato)},
                    ano_ini, ano_fim){

  producao_jornal_revista <- NULL
  
  jrevistas <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TEXTOS-EM-JORNAIS-OU-REVISTAS"
  
  n <- length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"TEXTOS-EM-JORNAIS-OU-REVISTAS")
  
  if(n > 0){
    for(i in 1:n){
      # identifica o ano do artigo
      ano = jrevistas[[i]]$`DADOS-BASICOS-DO-TEXTO`["ANO-DO-TEXTO"]
      ano <- as.vector(ano) # transforma o named vector em somente vetor
      # flag relevancia
      fl_rlvnc <- jrevistas[[i]]$`DADOS-BASICOS-DO-TEXTO`['FLAG-RELEVANCIA']
      
      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc == "SIM"){

        natureza <- jrevistas[[i]]$`DADOS-BASICOS-DO-TEXTO`["NATUREZA"]
        titulo <- jrevistas[[i]]$`DADOS-BASICOS-DO-TEXTO`["TITULO-DO-TEXTO"]
        titulo <- str_trim(encode_xml2(titulo))
        pais <- jrevistas[[i]]$`DADOS-BASICOS-DO-TEXTO`["PAIS-DE-PUBLICACAO"]
        meio <- str_to_lower(jrevistas[[i]]$`DADOS-BASICOS-DO-TEXTO`["MEIO-DE-DIVULGACAO"])
        meio <- sub("_", " ", meio)
        idioma <- jrevistas[[i]]$`DADOS-BASICOS-DO-TEXTO`["IDIOMA"]
        CeT <- jrevistas[[i]]$`DADOS-BASICOS-DO-TEXTO`["FLAG-DIVULGACAO-CIENTIFICA"]

        # novo qualis
        # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
        TT <- qualis_biblio[qualis_biblio$Titulo == str_to_lower(titulo) & qualis_biblio$Ano == ano & qualis_biblio$Autor == nome & qualis_biblio$Tipo == natureza, "Estrato"]
        TT <- str_sub(paste0(TT,"--"),1,2)

        pontos <- pontos_jornal_revista(paste0(natureza,"/",CeT),TT)
                

        # para o caso de ter flag de relevancia mas estar fora do periodo
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }

        ap <- data.frame(
          id = id,
          nome = nome,
          ano = ano,
          natureza = natureza,
          titulo = titulo,
          pais = pais,
          idioma = idioma,
          meio = meio,
          CeT = CeT,
          estrato = TT,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )
        
        producao_jornal_revista <- rbind(producao_jornal_revista, ap, row.names = NULL)
      }
    }
    
    # consolida pontuacao do bloco
    pontos <- producao_jornal_revista$pontos
    if(length(pontos) == 0) {
      titulo = "NAO HA PRODUCAO NO PERIODO."
    } else {
      titulo = ""
    }
    pontos <- sum(pontos, na.rm = TRUE)
    
  } else {
    pontos <- 0
    titulo = "NAO HA PRODUCAO NO PERIODO."
    fl_rlvnc = ""
  }
  
  # insere linha de totais
  ap <- data.frame(
    id = id,
    nome = nome,
    ano = "",
    natureza = "",
    titulo = titulo,
    pais = "",
    idioma = "",
    meio = "",
    CeT = "",
    estrato = "TOTAL",
    pontos = pontos,
    flag_relevancia = ""
  )
  producao_jornal_revista <- rbind(producao_jornal_revista, ap, make.row.names = FALSE)
  
  producao_jornal_revista

}