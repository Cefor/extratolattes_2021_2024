#############################################
# PONTUACAO CAPES - DOCENTE
#############################################

# criterios de pontuacao para artigos
pontos_artigo <- function(eq){ # eq: estrato qualis

  valor <- c(100, 85, 70, 55, 25, 20, 10, 5, 0, 0, 0, 0) # novo qualis  
  names(valor) <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", "B5", "C", "--", "NP")
  
  valor[eq]
  
}

# criterios de pontuacao de livros
pontos_livro <- function(eqcp = NULL, tipo){ # eq: estrato qualis

  # pontuacao CAPES
  if(tipo == "LIVRO_PUBLICADO"){
    
    valor <- c(100, 80, 60, 40, 20,     1, 10, 10)
    names(valor) <- c("L1", "L2", "L3", "L4", "L5",     "--", "LNC", "QA")
    as.numeric(valor[eqcp])


  } else if(tipo == "LIVRO_ORGANIZADO_OU_EDICAO"){

    # A area CP&RI considera essa categoria como producao tecnica
    
    #valor <- c(1, 0.75, 0, 0, 0)
    #names(valor) <- c("L4", "L3", "L2", "L1", "--")
    valor <- c(100, 80, 60, 40, 20,     1, 4)
    names(valor) <- c("T1", "T2", "T3", "T4", "T5",     "--", "QA")
    as.numeric(valor[eqcp])


  } else if(tipo == "Capitulo de livro publicado") {

    valor <- c(100, 80, 60, 40, 20,     1, 3, 3)
    names(valor) <- c("L1", "L2", "L3", "L4", "L5",     "--", "LNC", "QA")
    as.numeric(valor[eqcp])
     
  }  else{
    0
  }
}

# criterios de pontuacao de jornal / revista 
pontos_jornal_revista <- function(natCeT, estrato){

  if(estrato == "QA"){
    valor <- c(0, 0, 0, 2)
  } else {
    valor <- c(0, 0, 0, 1)
  }
  names(valor) <- c("JORNAL_DE_NOTICIAS/NAO", "JORNAL_DE_NOTICIAS/SIM", "REVISTA_MAGAZINE/NAO", "REVISTA_MAGAZINE/SIM")
  
  return(as.numeric(valor[natCeT]))
}


# criterios de pontuacao artigos em anais
pontos_artigo_anais <- function(natureza, estrato){
  
  if(estrato == "QA"){
    valor <- c(2, 0.5, 1)
  } else {
    valor <- c(1, 0.1, 0.5)
  }

  names(valor) <- c("COMPLETO", "RESUMO", "RESUMO_EXPANDIDO")
  
  return(as.numeric(valor[natureza]))
}


# criterios de pontuacao orientacoes
pontos_orientacao <- function(item, orientacao, nivel){

  # orientacoes concluidas
  if(item == 1){
    
    if(nivel == "dissertacao de mestrado"){
      if(orientacao == "orientador principal") {
        5
      } else if(orientacao == "co orientador"){
        3
      } else 0
    } else 0
    
    
    # orientacoes em andamento
  } else if(item == 2){
    
    if(nivel == "dissertacao de mestrado"){
      if(orientacao == "orientador principal") {
        3
      } else if(orientacao == "co orientador"){
        2
      } else 0
    } else 0
    
  }    

}


# criterios de pontuacao de bancas
pontos_banca <- function(item, nivel = NULL){
  
  # bancas no Programa de Mestrado
  if(item == 1){
    
    if(nivel == "mestrado"){
      2
    } else if(nivel == "exame de qualificacao de mestrado"){
      2
    } else if(nivel == "doutorado"){
      2
    } else if(nivel == "exame de qualificacao de doutorado"){
      2
    } else if(nivel == "curso de aperfeicoamento/especializacao"){ 
      0
    } else 0
    
    # bancas em outros programas
  } else if(item == 2){
    
    if(nivel == "mestrado"){
      1
    } else if(nivel == "exame de qualificacao de mestrado"){
      1
    } else if(nivel == "doutorado"){
      1
    } else if(nivel == "exame de qualificacao de doutorado"){
      1
    } else 0
    
    # quando for processo seletivo do MPPL
  } else if(item == 3){
    3
    
    # quando for outro tipo de banca julgadora
  } else if(item == 4){
    
    if(nivel == "professor titular"){
      0
    } else if(nivel == "concurso publico"){
      0
    } else if(nivel == "livre-docencia"){
      0
    } else if(nivel == "avalizacao de cursos"){
      0
    } else if(nivel == "outra"){
      0
    } else 0
  }
  
}


# criterios de pontuacao de projetos de pesquisa
pontos_projeto <- function(fl_responsavel, fl_financiado, tipo){
  
  # coordenador
  if(fl_responsavel == "SIM"){
    5

    # participante
  } else {
    2
  }
  
}


# criterios de pontuacao de disciplinas
pontos_disciplina <- function(fl_programa){
  if(fl_programa){
    1
  } else 0
  
}

# criterios de pontuacao colaboracao tecnica
pontos_coltec <- function(item, fl_qualificado, fl_editor){
 
  # Membro de comite assessor
  if(item == 1){
    5
    
    # Membro de corpo editorial qualificado
  } else if(item == 2){
    
    if(fl_qualificado){
      # editor
      if(fl_editor){
        5
      } else{
        1
      }
    } else 0
  }
  
}


# criterios de pontuacao tecnica
pontos_tec <- function(estrato){

  valor <- c(100, 80, 60, 40, 20,    1, 2)
  names(valor) <- c("T1", "T2", "T3", "T4", "T5",     "--", "QA")
  
  return(as.numeric(valor[estrato]))

}
