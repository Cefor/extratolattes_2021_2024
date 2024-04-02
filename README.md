## EXTRATO LATTES (2020-2024)

### Autor: Fabiano Peruzzo Schwartz

### Ano: 2023

## Introdução

O presente repositório se destina ao compartilhamento de scripts escritos na linguagem R para a geração de extrato da produção de docentes com base nas informações do currículo lattes.

O principal objetivo é acompanhar, de forma sistemática, a atuação de docentes em cursos de pós-graduação *stricto sensu* por meio da geração periódica de extratos. Dessa forma, pretende-se atrair a atenção para os principais pontos da avaliação da Capes, a fim de que a devida atualização do Lattes e ações corretivas possam acontecer em tempo. Essa prática pode contornar situações em que, por exemplo, um professor não venha a pontuar na avaliação do quadriênio por ter informado ISSN distinto daquele que consta na base Qualis.

O extrato traz informações sobre as linhas de pesquisa, a produção bibliográfica de artigos (art) e livros (liv), produção técnica (tec), orientações em andamento (ori), orientações concluídas (poc), participação em bancas (ban), projetos de pesquisa (pro), colaboração técnica (ctc) e disciplinas ministradas no programa (dis).

A atribuição de pontuação é baseada nos estratos atribuídos pelas comissões de avaliação da Capes no ciclo 2017-2020 e nos critérios de recredenciamento e reenquadramento de docentes do Mestrado Profissional em Poder Legislativo.

## Operação

A execução dos scripts deve obedecer à seguinte estrutura de arquivos:

**_Lattes**: pasta em que são baixados os currículos lattes compactados para a geração dos extratos; por exemplo, acessando-se o endereço 	
http://lattes.cnpq.br/5035568092503034, pode-se baixar a versão XML do respectivo Lattes (botão disponível na página do currículo aberto), que fará o download do arquivo "5035568092503034.zip", o qual deve ser movido para a pasta "_Lattes".

**ScriptsR**: pasta que contém os scripts R responsáveis pelo cômputo de pontos de cada item da produção; esses arquivos não devem ser alterados.

**ExtratoLattes.Rmd**: arquivo "Rmd" (R markdown) a ser executado para a geração dos extratos; antes da execução, é preciso cofigurar as variáveis do período da análise (**ano_ini** e **ano_fim**), o identificador do currículo lattes **id_especifico** (NULL - mostra todos os currículos), o vetor dos possíveis nomes da instituição **nome_instituição**, o vetor dos possíveis nomes do curso **nome_curso** e o nome da área **nome_area**.

**QualisPeriodicos_2017_2020.csv**: arquivo com a qualificação da Capes (2017-2020) para os periódicos de todas as áreas.

**QualisLivros_Capes2017_2020.csv**: arquivo com a qualificação da Capes para os livros do Mestrado Profissional em Poder Legislativo - CEFOR; para a geração de extrato de outros programas, substitua esse arquivo, mantendo-se a mesma estrutura (Ano;Autor;Tipo;Titulo;isbn;qualis). 

**QualisBiblio_Autor_2021_2024.csv**: arquivo com a qualificação da produção bibliográfica efetuada pelo autor; serve para auxiliar o coordenado do PPG no preenchimento da plataforma Sucupira; os dados são coletados por formulário Google, preenchido pelo próprio autor; para replicar o formulário, abra uma conta Google, acesse o menu "Arquivo" e selecione "Importar respostas"; escolha o arquivo **QualisBiblio_Autor_2021_2024.csv** e um formulário será criado com a mesma estrutura e perguntas.

**QualisTecnica_Capes_2017_2020.csv**: arquivo com a qualificação da Capes para as produções técnico-tecnológicas do Mestrado Profissional em Poder Legislativo - CEFOR; para a geração de extrato de outros programas, substitua esse arquivo, mantendo-se a mesma estrutura (Ano;Autor;Tipo;Titulo;isbn;qualis). 

**QualisTecnica_Autor_2021_2024.csv**: arquivo com a qualificação da produção técnico-tecnológica efetuada pelo autor; serve para auxiliar o coordenado do PPG no preenchimento da plataforma Sucupira; os dados são coletados por formulário Google, preenchido pelo próprio autor; para replicar o formulário, abra uma conta Google, acesse o menu "Arquivo" e selecione "Importar respostas"; escolha o arquivo **QualisTecnica_Autor_2021_2024.csv** e um formulário será criado com a mesma estrutura e perguntas.


## Outras aplicações

Esta ferramenta, pode ser usada para produzir o extrato dos discentes, de modo que cada professor tenha informações para instruir seus orientandos na inclusão da produção intelectual e técnica, desenvolvendo, assim, a cultura do preenchimento periódico ao longo do curso.

Da mesma forma, os egressos podem ser convidados a atualizar o Lattes a partir do envio do respectivo extrato e de mensagem explicativa, com orientações específicas de preenchimento, constituindo-se como ação de acompanhamento de egressos, item importante na avaliação Capes.


## Pasta *Documentos*


* **ManualPreenchimentoLattes.pdf**: contém instruções de preenchimento do currículo Lattes de forma que a pontuação possa ser computada corretamente no extrato; as instruções são ajustadas às orientações contidas nos documentos da área de Ciência Política e Relações Internacionais.

* **CriteiriosPontuacao.pdf**: critérios do Mestrado Profissional em Proder Legislativo que definem a pontuação por tipo de produção.