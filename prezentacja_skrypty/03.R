library(tidyverse)
library(tidytext)
library(wordcloud)
library(igraph)

rm(list = ls())

setwd(here::here())

books <- readRDS("data/books_all.RDS")

# podzielenie na slowa
books_words <- books %>%
  unnest_tokens(word, text, token = "words")

nrow(books_words)

# słownik sentymentu
senti_dict <- read_csv("dicts/nawl-analysis.csv") %>%
  # wybieramy tylko potrzebne kolumny
  .[, 1:7] %>%
  set_names(c("word", "category", "happiness", "anger", "sadness", "fear", "disgust"))

# stemming - wczytujemy słownik
stemm_dict <- readRDS("dicts/polimorfologik.RDS")

books_words <- books_words %>%
  # łączymy ze słownikiem stemmingu
  left_join(stemm_dict, by = "word") %>%
  # dla słów brakujących wpisujemy słowo oryginalne
  mutate(stem = if_else(is.na(stem), word, stem))


# łączymy słowa ze słownikiem
books_words_setni <- inner_join(books_words %>% select(author, title, part, word),
                                senti_dict,
                                by = "word")

nrow(books_words_setni)


#### sentyment według autora ----
# średnia wartość poszczególnej emocji wg autora
# im większa wartość tym mocniejsza cecha w tekście
books_words_setni %>%
  group_by(author) %>%
  summarise_at(.vars = c("happiness", "anger", "sadness", "fear", "disgust"), .funs = mean) %>%
  ungroup() %>%
  gather(key = "sentiment", value = "value", happiness:disgust) %>%
  arrange(author) %>%
  mutate(author = fct_inorder(author)) %>%
  ggplot() +
  geom_point(aes(author, value, color = author), show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free")



#### sentyment według książki ----
# j/w dla poszególnych książek
books_words_setni %>%
  group_by(author, title) %>%
  summarise_at(.vars = c("happiness", "anger", "sadness", "fear", "disgust"), .funs = mean) %>%
  ungroup() %>%
  gather(key = "sentiment", value = "value", happiness:disgust) %>%
  arrange(desc(value)) %>%
  mutate(title = fct_inorder(title)) %>%
  ggplot() +
  geom_col(aes(title, value, fill = author), show.legend = FALSE) +
  coord_flip() +
  facet_grid(author~sentiment, scales = "free_y")


#### sentyment w trakcie książki ----
book_sentiment_v1 <- function(f_title) {
  books_words_setni %>%
    filter(title == f_title) %>%
    group_by(part) %>%
    summarise_at(.vars = c("happiness", "anger", "sadness", "fear", "disgust"), .funs = mean) %>%
    ungroup() %>%
    gather(key = "sentiment", value = "value", happiness:disgust) %>%
    ggplot() +
    geom_line(aes(part, value, color = sentiment))
}


book_sentiment_v2 <- function(f_title) {
  books_words_setni %>%
    filter(title == f_title) %>%
    count(part, category) %>%
    ungroup() %>%
    group_by(part) %>%
    mutate(p = 100*n/sum(n)) %>%
    ungroup() %>%
    filter(!category %in% c("N", "U")) %>%
    ggplot() +
    geom_col(aes(part, p, fill = category), show.legend = FALSE) +
    facet_wrap(~category, ncol = 1)
}

book_sentiment_v1("W pustyni i w puszczy")
book_sentiment_v2("W pustyni i w puszczy")

books %>% filter(title == "W pustyni i w puszczy", part == 11) %>% pull(text) %>% paste(collapse = "\n") %>% cat()
books_words_setni %>% filter(title == "W pustyni i w puszczy", part == 11) %>% filter(!category %in% c("N", "U")) %>% select(-author, -title)


book_sentiment_v1("W osiemdziesiąt dni dookoła świata")
book_sentiment_v2("W osiemdziesiąt dni dookoła świata")



book_sentiment_v1("Quo vadis")
book_sentiment_v2("Quo vadis")

book_sentiment_v1("Ziemia obiecana")
book_sentiment_v2("Ziemia obiecana")
