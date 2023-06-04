#'-----------------------------------------------------------------------------
#' PROJETO: Artigo Predição da personalidade corrigindo por aquiescência
#'
#' OBJETIVO: realizar as análises do artigo que está sendo escrito para a disciplina de produção
#'
#' PRODUTO: Análises
#'
#' AUTOR: Araê Cainã
#'
#' DATA DE CRIACAO: 14/11/2022
#'
#' DATA DE MODIFICACAO:
#'
#' MODIFICACOES:
#'
#' 03/12 - Primi corrigiu os pares semânticos
#'
#' 20/12 - Araê incluiu os vetores contextuais e predição
#'
#' 16/05 - Adequação do código para construção de bigramas, além da escolha por
#' somente tf e tf_idf
#'
#'-----------------------------------------------------------------------------

# Pacotes -----------------------------------------------------------------

library(xlsx)
library(stringr)
library(tm)
library(tidytext)
library(tidyr)
library(dplyr)
library(glue)
library(tidyverse)
library(scales)

# Carrega as funções do labape --------------------------------------------

source("http://www.labape.com.br/rprimi/R/recoding_functions.R")

#' ATENÇÃO!!!!!
#' Caso não queira refazer os passos abaixo, pule para a sessão `Predição`

# Correção dos escores por aquiescência -----------------------------------


## Bases ------------------------------------------------------------------

load('bfi2.RData')
# save.image('bfi2.RData')

#' Foi preciso substituir o dataframe `domain_facet` para incluir os pares
#' semânticos disponíveis no artigo "The Developmental Psychometrics of Big Five
#'  Self-Reports: Acquiescence, Factor Structure, Coherence, and Differentiation
#'   From Ages 10 to 20"", disponível em: `https://www.colby.edu/psych/wp-content/uploads/sites/50/2013/08/Soto_et_al_2008.pdf`.
#' Vale ressaltar que os pares semânticos apontados no artigo fizeram sentido quando
#' considerados os textos na tabela `BFI_translations.xlsx`, com exceção do último par,
#' item 30 com item 41. O item 30 está em Abertura, enquanto o item 41 em Extroversão.
#' Analisando o texto dos itens na tabela `BFI_translations.xlsx`, fiz a opçao por mudar
#' o par citado acima para 30 e 15, pois os textos me pareceram próximos.
##' ALTERAÇÃO!!!
##' Primi corrigiu os pares semânticos e criou outro domain_facet.
##' Siga os passos abaixo para carregar no ambiente

bfi_1_44_dic <- xlsx::read.xlsx(
  'bfi_1_44_dic.xlsx',
  sheetIndex = 1
)

bfi_1_44_dic %>% glimpse

# Incluído para ordenar o domain facet por OCEAN
bfi_1_44_dic <- bfi_1_44_dic %>%
  dplyr::mutate(
    domain = factor(domain, levels = c("O", "C", "E", "A", "N"))
  ) %>%
  dplyr::arrange(
    domain, facet, pole
  )

## Cria dicionário para escala acq_indx
dic_acq <- bfi_1_44_dic %>%
  dplyr::filter(!is.na(seman_pairs_3))

## Cria o domain_facet
domain_facet <- tibble(
    coditem = bfi_1_44_dic$cod_paraboni,
    scale = bfi_1_44_dic$domain,
    pole = bfi_1_44_dic$pole,
    seman_pairs =  bfi_1_44_dic$seman_pairs_3,
    item_text =  bfi_1_44_dic$bfi_vldny,
    en_text = bfi_1_44_dic$bfi_us
  ) %>%
  rbind(
    tibble(
    coditem = bfi_1_44_dic$cod_paraboni,
    scale = paste0(bfi_1_44_dic$domain, "_", bfi_1_44_dic$facet),
    pole = bfi_1_44_dic$pole,
    seman_pairs =  bfi_1_44_dic$seman_pairs_3,
    item_text =  bfi_1_44_dic$bfi_vldny,
    en_text = bfi_1_44_dic$bfi_us
    )
  ) %>%
  rbind(
    tibble(
      coditem = dic_acq$cod_paraboni,
      scale = "acq_indx",
      pole = 1,
      seman_pairs =  dic_acq$seman_pairs_3,
      item_text =  dic_acq$bfi_vldny,
      en_text = dic_acq$bfi_us
    )
  )

domain_facet <- domain_facet %>%
  dplyr::filter(!(scale %in% c("E_ass", "N_dep")))

#' Carrega a base de dados de respostas dos sujeitos. Foi preciso mudar o nome das colunas
#' para casar com o cod_item
db_bfi <- xlsx::read.xlsx(
  'b5/subjects table/BFI-44-items(PT).xlsx',
  sheetIndex = 1
)



## Correção propriamente dita ---------------------------------------------

#' Vamos corrigir por aquiescência

#' Essa função faz a correção por aquiescência
db_bfi2 <- db_bfi %>%
  dplyr::select(unique(domain_facet$coditem)) %>%
  recode_for_acq(
    item_dic = domain_facet
  )


bfi_1_44_dic %>% group_by(facet) %>% count

#' É importante rodar a função abaixo para verificar as propriedades psicométricas
#' do BFI.
bfi_psicom <- find_psychometrics(
  db_bfi2,
  likert = 5,
  center = 3
)

#' Em seguida, abra o arquivo para avaliar as propriedades psicométricas.
save_item_psicom(bfi_psicom, filename = "bfi_psicom.xlsx")

#' Rode esse gráfico para aferir a distribuiçã geral dos escores de aquiescência
bfi_psicom$scores %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x=acq_index
    )
  ) +
  ggplot2::geom_histogram(color = "white") +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(5)
  )

#' Se quiser verificar os pares semânticos, rode a função abaixo
dic_acq %>%
  dplyr::select(text, domain, pole_final, seman_pairs_3) %>%
  dplyr::arrange(domain, seman_pairs_3, pole_final) %>%
  View()


bfi_psicom$scores %>% glimpse


#' Adiciona os escores ao `db_bfi`
db_bfi <- db_bfi %>% bind_cols(bfi_psicom$scores)


# Criar base de textos ----------------------------------------------------


## Lexical ----
#' Essa base contém os indicadores lexicos de cada sujeito
lexical <- read.csv(
  'b5/posts_as_single_file/lexical-features/id-lexical-features.csv',
  sep = ';'
)
# é preciso converter para numeric
 lexical <- data.frame(lapply(lexical, function(x) as.numeric(sub(",", ".", x, fixed = TRUE))))

## Base id e textos ----

## Cria uma lista com os nomes de todos os arquivos dentro da pasta
lista_textos <- list.files('b5/posts_by_user/')

## Retira os arquivos que não são de texto
lista_textos <- lista_textos[grepl('.txt$', lista_textos)]

## Cria um dataframe que será alimentado pelos termos nas pastas
db_textos <- data.frame(
  matrix(
    nrow = length(lista_textos),
    ncol = 2
  )
)
colnames(db_textos) <- c(
  'id',
  'texto'
)

## Lê todos os arquivos de texto
for(i in 1:length(lista_textos)){
  base::print(paste('Texto', i, 'de', length(lista_textos)))
  db_textos$id[i] <- lista_textos[i]
  db_textos$texto[i] <- paste0(
    scan(
      paste0("b5/posts_by_user//",lista_textos[i]),
      what="",
      encoding = 'latin1'
    ),
    collapse = ' '
  )

}

## É preciso remover todo `.txt` da coluna ocupações
db_textos$id <- stringr::str_remove(db_textos$id, 'post-speaker-')
db_textos$id <- stringr::str_remove(db_textos$id, '-normalised.txt')

#' Vamos criar um novo df_textos. Uma das limitações do BERT é que ele só é
#' capaz de assimilar textos com no máximo 512 tokens. Como a maioria dos textos
#' é composta por mais tokens que isto, vamos separar os textos em grupos de 500
#' palavras

db_textos_dividido <- data.frame(
  matrix(
    nrow = 0,
    ncol = 4
  )
)
names(db_textos_dividido) <- c(
  'id',
  'id_texto_dividido',
  'texto',
  'texto_dividido'
)

numero_palavras <- 500

for(i in 1:nrow(db_textos)){
  print(paste('Texto', i, 'de', nrow(db_textos)))


  palavras <- strsplit(db_textos$texto[i], "\\s+")[[1]]
  num_grupos <- ceiling(length(palavras) / numero_palavras)

  grupos_palavras <- split(
    palavras,
    rep(
      1:num_grupos,
      each = numero_palavras,
      length.out = length(palavras)
    )
  )

  db_intermediario <- data.frame(
    matrix(
      nrow = length(grupos_palavras),
      ncol = 4
    )
  )
  names(db_intermediario) <- c(
    'id',
    'id_texto_dividido',
    'texto',
    'texto_dividido'
  )

  db_intermediario$id <- db_textos$id[i]
  db_intermediario$texto <- db_textos$texto[i]
  db_intermediario$id_texto_dividido <- 1:length(grupos_palavras)

  for(j in 1:length(grupos_palavras)){
    db_intermediario$texto_dividido[j] <- paste(grupos_palavras[[j]], collapse = " ")
  }

  db_textos_dividido <- rbind(
    db_textos_dividido,
    db_intermediario
  )
}


# remove os indesejados
rm(
  list = setdiff(
    ls(),
    c(
      'db_bfi',
      'db_textos',
      'lexical'
    )
  )
)

save.image('base_padrao.rda')

## tf e tf_idf -------------------------------------------------------------

### Tokenizacao ----

tokenized_corpus  <- db_textos %>%
  dplyr::select(id, texto) %>%
  tidytext::unnest_tokens(
    output = tokens,
    input = texto,
    # to_lower = TRUE,
    token = "ngrams",
    n = 2
  )

# Carrega as stopwords
stopwords <- read.csv(
  file = "http://www.labape.com.br/rprimi/ds/stopwords.txt"
)

names(stopwords) <- "word"

tokenized_corpus <- tokenized_corpus %>%
  # 1. remove stopwords via anti_join
  anti_join(
    stopwords,
    by = c("tokens"="word")
  ) %>%
  # 2. Steming
  dplyr::mutate(
    tokens = SnowballC::wordStem(
      tokens,
      language =  "portuguese"
    )
  )

### Calcula tf_idf ----

tokenized_corpus_freq <- tokenized_corpus %>%
  dplyr::count(id, tokens) %>%
  tidytext::bind_tf_idf(tokens, id, n) %>%
  dplyr::select(-idf)

tokenize_corpus_final <- tokenized_corpus_freq %>%
  dplyr::mutate(Visit = match(tokens, unique(tokens))) %>%
  dplyr::group_by(id) %>%
  tidyr::gather("tokens", "tf", 'tf_idf', 'n', key = variable, value = number) %>%
  tidyr::unite(combi, variable, Visit) %>%
  tidyr::spread(combi, number)

save(tokenize_corpus_final, file = 'tokenize_corpus_final_stem.rdata')

#' Será realmente necessário retirar os que tem menos de 20% preenchido?
#' Caso sim, o número de colunas cai para menos de 10k
#' Caso não, comente a linha abaixo, rode e espere sentado. Demorou mais de 4h
# tokenize_corpus_final <- tokenize_corpus_final[, unlist(lapply(tokenize_corpus_final, function(x) mean(is.na(x)) < 0.80))]
tokenize_corpus_final[is.na(tokenize_corpus_final)] <- '0'


## Cria as bases finais
# n <- tokenize_corpus_final[,grepl('^n', names(tokenize_corpus_final))]
# n$id <- tokenize_corpus_final$id


tf <- tokenize_corpus_final[,grepl('^tf', names(tokenize_corpus_final))]
tf$id <- tokenize_corpus_final$id

tf_idf <- tokenize_corpus_final[,grepl('^tf', names(tokenize_corpus_final))]
tf_idf$id <- tokenize_corpus_final$id

# Vamos arrumar as bases para ficar com o id primeiro
# n <- n %>%
#   dplyr::select(id, dplyr::everything())
tf <- tf %>%
  dplyr::select(id, dplyr::everything())
tf_idf <- tf_idf %>%
  dplyr::select(id, dplyr::everything())


# remove os indesejados
rm(
  list = setdiff(
    ls(),
    c(
      'db_bfi_scores_acq',
      'db_textos',
      'lexical',
      # 'n',
      'tf',
      'tf_idf'
    )
  )
)

## DTM --------------------------------------------------------------------

corpus <- tm::VCorpus(
  tm::DirSource(
    'b5/posts_by_user/',
    encoding = 'latin1'
  ),
  readerControl = list(
    language = 'pt'
  )
)

## Transforma em um dtm
dtm <- tm::DocumentTermMatrix(corpus)

## É preciso primeiro transformar em uma matiz para depois transformar em dataframe
dtm <- dtm %>%
  as.matrix() %>%
  data.frame()


## Em seguida, transforma os rownames em uma coluna com cada id
dtm$id <- rownames(dtm)

# Remove os rownames
rownames(dtm) <- NULL

## Coloca a coluna `id` no começo do dataframe para melhor visualização
dtm <- dtm %>%
  dplyr::select(
    id,
    dplyr::everything()
  )

## É preciso remover todo `.txt` da coluna ocupações
dtm$id <- stringr::str_remove(dtm$id, 'post-speaker-')
dtm$id <- stringr::str_remove(dtm$id, '-normalised.txt')



# remove os indesejados
rm(
  list = setdiff(
    ls(),
    c(
      'db_bfi_scores_acq',
      'db_textos',
      'lexical',
      'n',
      'tf',
      'tf_idf',
      'dtm'
    )
  )
)

#' Salva em um unico arquivo chamado `analises_80` ou `analises_completas`,
#' a depender da escolha feita na linha 292

# save.image(
#   'analises_80.rda'
# )
# save.image(
#   'analises_completas.rda'
# )


# Predição ----------------------------------------------------------------

# Escolha uma das duas
load('analises_80.rda')
load('analises_completas.rda')
# load('analises_completas.rda')

names(tf)
glimpse(tf_idf)


# Função para predição dos escores
predict_svd <- function(
    db_bfi_scores_acq,
    db_merge,
    coluna,
    n
){
  db <- merge(
    db_bfi_scores_acq[,c('id', coluna)],
    db_merge,
    by = 'id'
  )

  db <- data.frame(lapply(db, function(x) as.numeric(x)))

  #' A princípio é necessário dividir entre teste e treino. Todos recebem
  #' um número de 1 a 10
  folds <- sample(
    1:10,
    size = nrow(db),
    replace = T
  )

  #' Somente usuários com valor 1 vão para o grupo de teste
  test <- folds == 1

  #' Novamente é realizada a SVD, mas com `k = n`

  M <- as.matrix(db[!test,-c(1:2)])

  Msvd <- irlba::irlba(M, nv = n)
  v_rot <- unclass(varimax(Msvd$v)$loadings)
  u_rot <- as.data.frame(as.matrix(M %*% v_rot))

  #' Em seguida, aplica-se a função `glm()` para criar um modelo de regressão
  #' para predizer a variável `coluna`.
  #'
  #' Para regressão logística, é preciso usar o argumento `family = 'binominal'`
  #'
  #' È passada como argumento a base de dados e o subset da base de treino.

  # Modelo abertura
  fit_o <- glm(
    db[[coluna]][!test] ~ .,
    data = u_rot
  )


  #' Em seguida, é possível usar a função `predict()` para utilizar o modelo
  #' treinado na base de treino na base de teste.

  pred_o <- predict(
    fit_o,
    u_rot[test,]
  )

  #' É possível verificar a correlação da predição com o escore real
  correlacao <- cor.test(db[[coluna]][test], pred_o, use = "complete.obs")

  return(
    paste(
      correlacao$estimate,
      correlacao$p.value
    )
  )
}

#' Aplica a função acima, considerando
#' @param db_bfi_scores_acq db que contém a coluna escolhida para predição
#' @param db_merge db com as varíaveis que serão reduzidas
#' @param coluna com a coluna a ser predita
#' @param n número de vetores criados pela svd
#' LEMBRE-SE QUE PARA n ACIMA DE 50 A FUNÇÃO VAI DEMORAR
predict_svd(
  db_bfi_scores_acq = db_bfi_scores_acq,
  db_merge = tf,
  coluna = 'E_rec',
  n = 1
)



# Loop para obter os resultados -------------------------------------------
set.seed(23)

resultados <- list()

# Aqui vai cada um dos preditores

pred <- colnames(db_bfi_scores_acq[-c(1)])

# Cada uma das bases a ser utilizadas

databases <- c(
  'dtm',
  'n',
  'tf',
  'tf_idf'
)

# Cada um dos valores de n

loop <- c(3, 5, 10, 15, 20, 30, 40, 50, 70, 100)
Sys.time()
# Faz o loop
for(y in 1:length(pred)){

  # Printa o progresso
  print(paste(pred[y], ':', y, 'de', length(pred)))

  # Loop de databases
  for(j in 1:length(databases)){

    # Printa o progresso
    print(paste('  Seguindo para o db', databases[j]))

    # Loop com os n
    for(i in 1:length(loop)){
      # Printa o progresso
      print(paste('    ', databases[j], 'cujo n =', loop[i]))

      # Gera o resultado
      resultados[[pred[y]]][[databases[j]]][i] <- predict_svd(
        db_bfi_scores_acq = db_bfi_scores_acq,
        db_merge = get(databases[j]),
        coluna = 'E_rec',
        n = loop[i]
      )

    }

  }
}

Sys.time()

save(resultados, file = 'resultados_completos.rda')

resultados



# Word-embeddings ---------------------------------------------------------

#' A partir de agora vamos focar na busca por vetores contextuais. Assim como
#' as estratégias de tokenização acima utilizadas, estes vetores nos dão uma
#' representação numérica do texto. A principal diferença é que os vetores
#' contextuais permitem que palavras com significados similares tenham a mesma
#' representação.
#'
#' Neste exercício utilizaremos o modelo pré-treinado do bert em português. Vale
#' ressaltar que
load('base_padrao.rda')

library(text)

# for(i in 15:20){
#   print(paste('Testando a linha', i))
#   word_embeddings <- text::textEmbed(
#     texts = db_textos[i, 2],
#     model = "neuralmind/bert-base-portuguese-cased",
#     layers = -2,
#     aggregation_from_tokens_to_texts = "mean",
#     aggregation_from_tokens_to_word_types = "mean",
#     keep_token_embeddings = FALSE
#   )
#
#   word_embeddings$texts
#
# }

# Erro na linha 19 - aparentemente existem muitas palavras em inglês


library(qdapDictionaries)

remover <- c()

for(i in 1:nrow(db_textos)){
  # print(paste('Avaliando linha', i))

  vector <- db_textos[i, 2]
  vector <- gsub('\n', '', vector)

  vector <- tolower(vector)

  vector <- unlist(stringr::str_split(vector, ' '))

  vector <- gsub('["]', '', vector)
  vector <- gsub('[,]', '', vector)

  if(length(vector[vector %in% GradyAugmented]) >= 2000 | sum(grepl('[$]', vector))/length(vector)>.15){
    remover <- c(remover, i)
    print(i)
  }

  # print(length(vector[vector %in% GradyAugmented]))
}
# retirei e não funcionou

db_textos_pt <- db_textos[-remover, ]

word_embeddings_1_200 <- text::textEmbed(
  texts = db_textos_pt[1:200, 2],
  model = "neuralmind/bert-base-portuguese-cased",
  layers = -2,
  aggregation_from_tokens_to_texts = "mean",
  aggregation_from_tokens_to_word_types = "mean",
  keep_token_embeddings = FALSE
)

word_embeddings_3$texts

word_embeddings_1_100




# Segunda opção -----------------------------------------------------------

#' Rodou até a linha 500. Os itens com problema são os que seguem:
#'  19
#'  139
#'  205
#'  238
#'  263
#'  322
#'  369
#'  480
#'  481
#'
#'  entre 534 e 564 deu um problema. Não sei se existem linhas com problema nesse espectro
#'
#'  564
#'  710
#'  758
#'  790
#'  873
#'  877
#'  907

load('base_padrao.rda')

library(text)

remover <- c()

for(i in 901:1020){
  tryCatch(
    {
      print(paste('Testando linha', i))
      word_embeddings <- text::textEmbed(
          texts = db_textos[i, 2],
          model = "neuralmind/bert-base-portuguese-cased",
          layers = -2,
          aggregation_from_tokens_to_texts = "mean",
          aggregation_from_tokens_to_word_types = "mean",
          keep_token_embeddings = FALSE
        )
    },
    error = function(cond){
      print(cond)
      remover <<- c(remover, i)
    }
  )
}


#' Vamos remover os itens indesejados

remover <- c(
  19,
  139,
  205,
  238,
  263,
  322,
  369,
  480,
  481,
  564,
  710,
  758,
  790,
  873,
  877,
  907
)

db_textos_alterado <- db_textos[-c(remover),]


#' O próximo passo é tentar rodar o modelo inteiro, sem os itens que estão com problemas
word_embeddings <- text::textEmbed(
  texts = db_textos_alterado[1:500, 2],
  model = "neuralmind/bert-base-portuguese-cased",
  layers = -2,
  aggregation_from_tokens_to_texts = "mean",
  aggregation_from_tokens_to_word_types = "mean",
  keep_token_embeddings = FALSE,
  device = 'gpu:10DE 2504 250410DE'
)
#' Infelizmente não resolveu. A memória é insuficiente para rodar tudo que é preciso

#' Em seguida vamos rodar os vetores de 1 à 5 e de 5 à 11. A ideia é verificar se
#' com um grupo diferente de observações os vetores contextuais continuam os mesmo.

word_embeddings_1_5 <- text::textEmbed(
  texts = db_textos_alterado[1:5, 2],
  model = "neuralmind/bert-base-portuguese-cased",
  layers = -2,
  aggregation_from_tokens_to_texts = "mean",
  aggregation_from_tokens_to_word_types = "mean",
  keep_token_embeddings = FALSE
)
word_embeddings_5_10 <- text::textEmbed(
  texts = db_textos_alterado[5:10, 2],
  model = "neuralmind/bert-base-portuguese-cased",
  layers = -2,
  aggregation_from_tokens_to_texts = "mean",
  aggregation_from_tokens_to_word_types = "mean",
  keep_token_embeddings = FALSE
)

word_embeddings_1_5$texts
word_embeddings_5_10$texts

#' Como continuam, vamos rodar individualmente cada um indivíduos, criando uma
#' lista com todos os vetores contextuais

word_embeddings <- list()

for(i in 406:nrow(db_textos_alterado)){
  print(paste('Rodando linha', i))
  word_embeddings[[i]] <- text::textEmbed(
    texts = db_textos_alterado[i, 2],
    model = "neuralmind/bert-base-portuguese-cased",
    layers = -2,
    aggregation_from_tokens_to_texts = "mean",
    aggregation_from_tokens_to_word_types = "mean",
    keep_token_embeddings = FALSE
  )
  print(paste('Salvando os dados da linha', i))
  save(
    word_embeddings,
    file = paste0('word_embeddings.rdata')
  )

  print(paste('Registrando o acompanhamento'))

  fileConn <- file("acompanhamento_word_embeddings.txt")
  writeLines(as.character(i), fileConn)
  close(fileConn)
}




