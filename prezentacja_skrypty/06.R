# https://dsnotes.com/post/text2vec/
# http://text2vec.org/glove.html

library(tidyverse)
library(text2vec)

rm(list = ls())

setwd(here::here())

books <- readRDS("data/books_all.RDS")

# słowniki
stop_words_pl <- read_lines("dicts/polish_stopwords.txt")


prep_fun <- function(x) {
  x %>%
    # małe litery
    str_to_lower() %>%
    # usunięcie zbędnych znaków - zamiana ich na spacje
    str_replace_all("[^[:alnum:]]", " ") %>%
    # usunięcie wielokrotnych spacji
    str_replace_all("\\s+", " ")
}

book_full <- books %>%
  mutate(id = paste(author, title, part, par, sep = "-")) %>%
  select(id, text) %>%
  mutate(text_prep = prep_fun(text))


# tokenizer
itok_full <- itoken(book_full$text_prep,
                    tokenizer = word_tokenizer,
                    ids = book_full$id,
                    progressbar = TRUE)


# vocab
vocab_full <- create_vocabulary(itok_full,
                                stopwords = stop_words_pl,
                                ngram = c(1L, 1L))

vocab_full <- prune_vocabulary(vocab_full,
                               term_count_min = 3,
                               doc_proportion_max = 0.3,
                               doc_proportion_min = 0.001)

vectorizer_full <- vocab_vectorizer(vocab_full)


# term-co-occurrence matrix
tcm <- create_tcm(itok_full, vectorizer_full, skip_grams_window = 5)
tcm <- normalize(tcm, "l1")




tcm["ojciec", ] %>% sort(., decreasing = T) %>% head(., 10)
tcm["matka", ] %>% sort(., decreasing = T) %>% head(., 10)
tcm["syn", ] %>% sort(., decreasing = T) %>% head(., 10)
tcm["córka", ] %>% sort(., decreasing = T) %>% head(., 10)





glove <- GlobalVectors$new(word_vectors_size = 100,
                           vocabulary = vocab_full,
                           x_max = 10)

wv_main <- glove$fit_transform(tcm, n_iter = 20)
dim(wv_main)

wv_context <- glove$components
dim(wv_context)

word_vectors <- wv_main + t(wv_context)
dim(word_vectors)



sim_words <- function(a, b, c, n = 5) {

  test_word <- word_vectors[a, , drop = FALSE] -
    word_vectors[b, , drop = FALSE] +
    word_vectors[c, , drop = FALSE]

  cos_sim <- sim2(x = word_vectors,
                  y = test_word,
                  method = "cosine", norm = "l2")

  return(cos_sim[,1] %>% sort(. , decreasing = TRUE) %>% head(., n))
}

sim_words("ojciec", "mężczyzna", "kobieta", 10)



pca <- prcomp(word_vectors, scale. = TRUE)

pca$x[, 1:2] %>%
  as.data.frame() %>%
  rownames_to_column("word") %>%
  filter(word %in% c("ojciec", "mężczyzna", "kobieta", "matka", "syn", "córka")) %>%
  ggplot() +
  geom_point(aes(PC1, PC2)) +
  geom_text(aes(PC1, PC2, label = word))

