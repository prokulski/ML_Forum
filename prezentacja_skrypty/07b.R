library(tidyverse)
library(tidytext)
library(lsa)

rm(list = ls())
setwd(here::here())


books <- readRDS("data/books_all.RDS")

# słowniki
stop_words_pl <- read_lines("dicts/polish_stopwords.txt")

ksiazka_testowa <- "Siłaczka"
# ksiazka_testowa <- "Antek"
# ksiazka_testowa <- "Janko Muzykant"
# ksiazka_testowa <- "Chłopi"
# ksiazka_testowa <- "Trzej muszkieterowie"



book_mat <- books %>%
  # oznaczamy książkę "nieznanego" autora
  mutate(author = if_else(title == ksiazka_testowa, "nieznany", author)) %>%
  # zostawiamy sobie tylko potrzebne kolumny
  select(author, title, text) %>%
  # dzielimy wszystkie książki na słowa
  unnest_tokens(word, text, token = "words") %>%
  # usuwamy stop words
  filter(!word %in% stop_words_pl) %>%
  # usuwamy liczby
  filter(is.na(as.numeric(word))) %>%
  # zliczamy słowa wg autora
  count(author, word) %>%
  # budujemy TDM
  cast_dtm(word, author, n) %>%
  # potrzebujemy jej jako macierzy
  as.matrix()



# liczymy odległośc kosinusowa pomiedzy wektrami autorow w przestrzeni slow (wow!)
book_mat_cos <- cosine(book_mat)

# wynik
book_mat_cos["nieznany", ] %>% sort(decreasing = TRUE)

# wynik w postaci wykresu
tibble(score = 100*book_mat_cos["nieznany", ],
       author = names(book_mat_cos["nieznany", ])) %>%
  filter(author != "nieznany") %>%
  mutate(author = reorder(author, score)) %>%
  ggplot() +
  geom_col(aes(author, score)) +
  coord_flip() +
  labs(x = "")
