# skrypt do podstawowej analizy danych tekstowych z ksiazek


library(tidyverse)
library(tidytext)
library(wordcloud)
library(igraph)

rm(list = ls())

setwd(here::here())

books <- readRDS("data/books_all.RDS")






###############
#### CZĘŚĆ ####
###############

# przygotowanie 1- i 2- gramów

#### podzielenie na slowa ----
books_words <- books %>%
  unnest_tokens(word, text, token = "words")


#### podzielenie na bi-gramy ----
books_biwords <- books %>%
  unnest_tokens(word, text, token = "ngrams", n = 2)






###############
#### CZĘŚĆ ####
###############


#### popularność słów wg autora i książki ----
books_words_cnt <- books_words %>%
  count(author, title, word) %>%
  mutate(book = paste0(author, "\n", title)) %>%
  group_by(book) %>%
  mutate(book_words = sum(n)) %>%
  ungroup() %>%
  mutate(p = 100*n/book_words)


# po 10 najpopularniejszych słów wg książki - wartości bezwzględne
books_words_cnt %>%
  group_by(book) %>%
  top_n(10, n) %>%
  ungroup() %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~book, scales = "free")


# po 10 najpopularniejszych słów wg książki - wartości względne
books_words_cnt %>%
  group_by(book) %>%
  top_n(10, p) %>%
  ungroup() %>%
  ggplot(aes(word, p)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~book, scales = "free")






###############
#### CZĘŚĆ ####
###############


#### stop-words ----
books_words_cnt %>%
  filter(title == "Lalka") %>%
  arrange(desc(p)) %>%
  mutate(r = row_number()) %>%
  mutate(c = p*r) %>%
  ggplot() +
  geom_density(aes(c), fill = "lightgreen")







###############
#### CZĘŚĆ ####
###############




##### Popularność słów wg autora i książki bez stop words ----
stop_words_pl <- read_lines("dicts/polish_stopwords.txt")

books_words_cnt_nostop <- books_words_cnt %>%
  filter(!word %in% stop_words_pl)


books_words_cnt_nostop %>%
  filter(title == "Lalka") %>%
  arrange(desc(p)) %>%
  mutate(r = row_number()) %>%
  mutate(c = p*r) %>%
  ggplot() +
  geom_density(aes(c), fill = "lightgreen")



books_words_cnt_nostop %>%
  group_by(book) %>%
  top_n(10, p) %>%
  ungroup() %>%
  ggplot(aes(word, p)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~book, scales = "free")






###############
#### CZĘŚĆ ####
###############


#### steming ----

# stemming - wczytujemy słownik
stemm_dict <- readRDS("dicts/polimorfologik.RDS")


# stemming - przykłady
stemm_dict %>% filter(word %in% c("śpiąc", "domku", "szeptałyśmy", "kryjąc", "nieobleczoną", "kołdrą"))


stemm_dict %>% filter(stem == "dom")





###############
#### CZĘŚĆ ####
###############


#### Popularność słów wg autora i książki - po stemmingu ----
books_words_stem <- books_words_cnt_nostop %>%
  # łączymy ze słownikiem stemmingu
  left_join(stemm_dict, by = "word") %>%
  # dla słów brakujących wpisujemy słowo oryginalne
  mutate(stem = if_else(is.na(stem), word, stem))

saveRDS(books_words_stem, "data/books_word_stem.RDS")




###############
#### CZĘŚĆ ####
###############


books_words_stem <- readRDS("data/books_word_stem.RDS")

books_words_stem_cnt <- books_words_stem %>%
  group_by(book, stem, book_words) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(p = 100*n/book_words)

saveRDS(books_words_stem_cnt, "data/books_words_stem_cnt.RDS")



###############
#### CZĘŚĆ ####
###############

books_words_stem_cnt <- readRDS("data/books_words_stem_cnt.RDS")


books_words_stem_cnt %>%
  group_by(book) %>%
  top_n(10, p) %>%
  ungroup() %>%
  ggplot(aes(stem, p)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~book, scales = "free")




###############
#### CZĘŚĆ ####
###############

#### chmurka słów wg książki ----
plot_word_cloud <- function(author, title, max_words = 100) {

  words <- books_words_stem_cnt %>%
    filter(book == paste0(author, "\n", title)) %>%
    top_n(max_words, p) %>%
    select(stem, n)

  wordcloud(words$stem, words$n,
            max.words = max_words,
            scale = c(2, 0.8),
            colors = colorRampPalette(c("#313695", "#a50026"))(15))
}


# wykres słów wg książki
plot_word_bar <- function(author, title, max_words = 30) {
  books_words_stem_cnt %>%
    filter(book == paste0(author, "\n", title)) %>%
    top_n(max_words, p) %>%
    arrange(p) %>%
    mutate(stem = fct_inorder(stem)) %>%
    ggplot() +
    geom_col(aes(stem, p)) +
    coord_flip()
}

plot_word_cloud("Jules Gabriel Verne", "W osiemdziesiąt dni dookoła świata", 150)
plot_word_bar("Jules Gabriel Verne", "W osiemdziesiąt dni dookoła świata", 40)






###############
#### CZĘŚĆ ####
###############


#### popularność słów wg rozdziałów ----
books_words_par_cnt <- books_words %>%
  filter(!word %in% stop_words_pl) %>%
  count(author, title, part, word)

plot_popular_word_per_part <- function(f_title) {
  books_words_par_cnt %>%
    filter(title == f_title) %>%
    group_by(part, word) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    group_by(part) %>%
    mutate(p = 100*n/sum(n)) %>%
    arrange(desc(p)) %>%
    mutate(r = row_number()) %>%
    filter(r == 1) %>%
    ungroup() %>%
    arrange(desc(word)) %>%
    mutate(word = fct_inorder(word)) %>%
    ggplot() +
    geom_tile(aes(part, word, fill = r), show.legend = FALSE)
}

plot_popular_word_per_part("Potop")
plot_popular_word_per_part("W osiemdziesiąt dni dookoła świata")
plot_popular_word_per_part("Lalka")
plot_popular_word_per_part("Ziemia obiecana")
plot_popular_word_per_part("Przedwiośnie")
plot_popular_word_per_part("W pustyni i w puszczy")




###############
#### CZĘŚĆ ####
###############


books_biwords_cnt <- books_biwords %>%
  filter(!is.na(word)) %>%
  count(author, title, word) %>%
  mutate(book = paste0(author, "\n", title)) %>%
  group_by(book) %>%
  mutate(book_words = sum(n)) %>%
  ungroup() %>%
  mutate(p = 100*n/book_words)

# po 10 najpopularniejszych słów wg książki - wartości bezwzględne
books_biwords_cnt %>%
  filter(n > 1) %>%
  group_by(book) %>%
  top_n(5, n) %>%
  ungroup() %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~book, scales = "free")

# pozbędziemy się bigramów ze stop-wordami
books_biwords_cnt_nostop <- books_biwords %>%
  filter(!is.na(word)) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words_pl) %>%
  filter(!word2 %in% stop_words_pl) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(author, title, bigram) %>%
  mutate(book = paste0(author, "\n", title)) %>%
  group_by(book) %>%
  mutate(book_words = sum(n)) %>%
  ungroup() %>%
  mutate(p = 100*n/book_words)

books_biwords_cnt_nostop %>%
  filter(n > 1) %>%
  group_by(book) %>%
  top_n(5, n) %>%
  ungroup() %>%
  ggplot(aes(bigram, p)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~book, scales = "free")





###############
#### CZĘŚĆ ####
###############


# grafy
bigram_popularity <- books_biwords_cnt_nostop %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2)

bigram_popularity <- readRDS("data/bigram_popularity.RDS")


# ograniczamy liczbę elementów - żeby graf był czytelniejszy
bigram_popularity <- bigram_popularity %>% filter(nn > 13)

# budujemy graf
bigram_graph <- graph_from_data_frame(bigram_popularity, directed = TRUE)


E(bigram_graph)$weight <- bigram_popularity$nn
V(bigram_graph)$community <- walktrap.community(bigram_graph)$membership

plot(bigram_graph,
     vertex.size = 0,
     vertex.label.cex = 1.2,
     vertex.label.color = V(bigram_graph)$community,
     edge.width = 5*E(bigram_graph)$weight/max(E(bigram_graph)$weight),
     edge.arrow.size = 0.6*E(bigram_graph)$weight/max(E(bigram_graph)$weight),
     edge.color = "grey90",
     layout = layout_with_kk)

