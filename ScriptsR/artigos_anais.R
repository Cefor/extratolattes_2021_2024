
artigos_anais <- function(id, nome, xml_data, qualis_biblio,
                    pontos_artigo_anais = function(natureza) natureza,
                    ano_ini, ano_fim){

  producao_artigos_anais <- NULL
  
  artigosanais <- xml_data$`PRODUCAO-BIBLIOGRAFICA`$`TRABALHOS-EM-EVENTOS`
  n <- length(artigosanais)
  
  if(n > 0){
    for(i in 1:n){
      
      # identifica o ano do artigo
      ano = artigosanais[[i]]$"DADOS-BASICOS-DO-TRABALHO"["ANO-DO-TRABALHO"]
      ano <- as.vector(ano) # transforma o named vector em somente vetor
      # flag relevancia
      fl_rlvnc <- artigosanais[[i]]$"DADOS-BASICOS-DO-TRABALHO"['FLAG-RELEVANCIA']
      
      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc == "SIM"){
        # recupera o titulo do artigo 
        titulo <- artigosanais[[i]]$"DADOS-BASICOS-DO-TRABALHO"["TITULO-DO-TRABALHO"]
        titulo <- str_trim(encode_xml2(titulo))
        natureza <- artigosanais[[i]]$"DADOS-BASICOS-DO-TRABALHO"["NATUREZA"]
        pais <- artigosanais[[i]]$"DADOS-BASICOS-DO-TRABALHO"["PAIS-DO-EVENTO"]
        doi <- artigosanais[[i]]$"DADOS-BASICOS-DO-TRABALHO"["DOI"]
        isbn <- str_trim(artigosanais[[i]]$`DETALHAMENTO-DO-TRABALHO`["ISBN"])
        evento <- artigosanais[[i]]$`DETALHAMENTO-DO-TRABALHO`["NOME-DO-EVENTO"]

        
        # novo qualis
        # TT == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
        TT <- qualis_biblio[qualis_biblio$Titulo == str_to_lower(titulo) & qualis_biblio$Ano == ano & qualis_biblio$Autor == nome & qualis_biblio$ISBN == isbn & qualis_biblio$Tipo == natureza, "Estrato"]
        TT <- str_sub(paste0(TT,"--"),1,2)
        
        
        pontos <- pontos_artigo_anais(natureza, TT)

        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }

        ap <- data.frame(
          id = id,
          nome = nome,
          ano = ano,
          titulo = titulo,
          doi = doi,
          isbn = isbn,
          pais = pais,
          evento = evento,
          natureza = natureza,
          estrato = TT,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )
        
        producao_artigos_anais <- rbind(producao_artigos_anais, ap, row.names = NULL)
      }
    }
    
    # consolida pontuacao do bloco
    pontos <- producao_artigos_anais$pontos
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
    doi = "",
    isbn = "",
    pais = "",
    evento = "",
    natureza = "",
    estrato = "TOTAL",
    pontos = pontos,
    flag_relevancia = ""
  )
  producao_artigos_anais <- rbind(producao_artigos_anais, ap, row.names = NULL)
  
  producao_artigos_anais

}