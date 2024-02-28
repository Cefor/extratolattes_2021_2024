
livros <- function(id, nome, xml_data, qualis_biblio, ano_ini, ano_fim,
                   pontos_livro = function(FUN, eqcp, tipo) {FUN(eqcp, tipo)}){

  producao_livros <- NULL
  
  n <- length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS")
  
  if(n > 0){
    for(i in 1:n){
      # identifica o ano do livro
      ano = xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DADOS-BASICOS-DO-LIVRO"["ANO"]
      
      # flag relevancia
      fl_rlvnc <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DADOS-BASICOS-DO-LIVRO"['FLAG-RELEVANCIA']

      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc == "SIM"){
        # recupera dados basicos do livro
        isbn <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DETALHAMENTO-DO-LIVRO"["ISBN"] %>% str_trim
        tipo <- rm_accent(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DADOS-BASICOS-DO-LIVRO"["TIPO"])
        titulo <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DADOS-BASICOS-DO-LIVRO"["TITULO-DO-LIVRO"]
        titulo <- str_trim(encode_xml2(titulo))
        edicao <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DETALHAMENTO-DO-LIVRO"["NUMERO-DA-EDICAO-REVISAO"]
        editora <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DETALHAMENTO-DO-LIVRO"["NOME-DA-EDITORA"]
        cidade <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"LIVROS-PUBLICADOS-OU-ORGANIZADOS"[[i]]$"DETALHAMENTO-DO-LIVRO"["CIDADE-DA-EDITORA"]
        # estrato qualis -busca pelo ISBN, Titulo, Autor e Ano
        #eqcp <- qualis_biblio[qualis_biblio$ISBN == isbn & qualis_biblio$Titulo == str_to_lower(titulo) & qualis_biblio$Autor == nome & qualis_biblio$Ano == ano & qualis_biblio_biblio$Tipo == tipo, "Estrato"]
        #nlinhas <- nrow(eqcp)
        #if(nlinhas > 1) {
        #  eqcp <- '>1'
        #} else if(nlinhas == 0) {
        #  eqcp <- "--"
        #} else{
        #  eqcp <- eqcp$Estrato  
        #}
        # pontos <- 0

        # novo qualis
        # eqcp == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
        eqcp <- qualis_biblio[qualis_biblio$ISBN == isbn & qualis_biblio$Titulo == str_to_lower(titulo) & qualis_biblio$Autor == nome & qualis_biblio$Ano == ano & qualis_biblio$Tipo == tipo, "Estrato"]
        eqcp <- str_sub(paste0(eqcp,"--"),1,2)

        
        pontos <- pontos_livro(eqcp, tipo)
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }
        
        ap <- data.frame(
          id = id,
          nome = nome,
          ano = ano,
          titulo = titulo,
          isbn = isbn,
          tipo = tipo,
          edicao = edicao,
          editora = editora,
          cidade = cidade,
          estrato_cp = eqcp,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )
        
        producao_livros <- rbind(producao_livros, ap, row.names = NULL)
      }
    }
  }
  
  # CAPITULOS
  n <- length(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS")
  if(n > 0){
    for(i in 1:n){
      # identifica o ano do livro
      ano = xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-CAPITULO"["ANO"]

      # flag relevancia
      fl_rlvnc <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-CAPITULO"['FLAG-RELEVANCIA']
      
      if((ano >= ano_ini & ano <= ano_fim) | fl_rlvnc=="SIM"){
        # recupera dados basicos do livro
        isbn <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-CAPITULO"["ISBN"] %>% str_trim
        tipo <- rm_accent(xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-CAPITULO"["TIPO"])
        titulo <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DADOS-BASICOS-DO-CAPITULO"["TITULO-DO-CAPITULO-DO-LIVRO"]
        titulo <- str_trim(encode_xml2(titulo))
        edicao <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-CAPITULO"["NUMERO-DA-EDICAO-REVISAO"]
        editora <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-CAPITULO"["NOME-DA-EDITORA"]
        cidade <- xml_data$"PRODUCAO-BIBLIOGRAFICA"$"LIVROS-E-CAPITULOS"$"CAPITULOS-DE-LIVROS-PUBLICADOS"[[i]]$"DETALHAMENTO-DO-CAPITULO"["CIDADE-DA-EDITORA"]
        # estrato qualis -busca pelo ISBN, Titulo, Autor e Ano
        #eqcp <- qualis_biblio[qualis_biblio$ISBN == isbn & qualis_biblio$Titulo == str_to_lower(titulo) & qualis_biblio$Autor == nome & qualis_biblio$Ano == ano, "Estrato"]
        #nlinhas <- nrow(eqcp)
        #if(nlinhas > 1) {
        #  eqcp <- '>1'
        #} else if(nlinhas == 0) {
        #  eqcp <- "--"
        #} else{
        #  eqcp <- eqcp$Estrato  
        #}
        # pontos <- 0
        
        # novo qualis
        # eqcp == "QA" é o estrato qualificado pelo autor - o titulo da producao deve ser igual ao do Lattes do autor
        eqcp <- qualis_biblio[qualis_biblio$ISBN == isbn & qualis_biblio$Titulo == str_to_lower(titulo) & qualis_biblio$Autor == nome & qualis_biblio$Ano == ano & qualis_biblio$Tipo == tipo, "Estrato"]
        eqcp <- str_sub(paste0(eqcp,"--"),1,2)
        
        
        pontos <- pontos_livro(eqcp, tipo)
        if(!(ano >= ano_ini & ano <= ano_fim)){
          pontos <- 0
        }
        
        ap <- data.frame(
          id = id,
          nome = nome,
          ano = ano,
          titulo = titulo,
          isbn = isbn,
          tipo = tipo,
          edicao = edicao,
          editora = editora,
          cidade = cidade,
          estrato_cp = eqcp,
          pontos = pontos,
          flag_relevancia = fl_rlvnc
        )
        
        producao_livros <- rbind(producao_livros, ap, row.names = NULL)
      }
    }
  }
  
  # consolida pontuacao do bloco
  pontos <- producao_livros$pontos
  if(length(pontos) == 0) {
    titulo = "NAO HA PRODUCAO NO PERIODO."
  } else {
    titulo = ""
  }
  
  # insere linha de totais
  ap <- data.frame(
    id = id,
    nome = nome,
    ano = "",
    titulo = titulo,
    isbn = "",
    tipo = "",
    edicao = "",
    editora = "",
    cidade = "",
    estrato_cp = "TOTAL",
    pontos = sum(pontos, na.rm = TRUE),
    flag_relevancia = ""
  )
  producao_livros <- rbind(producao_livros, ap, row.names = NULL)
  
  producao_livros

}