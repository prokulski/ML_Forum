library(tidyverse)
library(tidytext)

rm(list = ls())
setwd(here::here())

books <- readRDS("data/books_all.RDS")
stop_words_pl <- read_lines("dicts/polish_stopwords.txt")

# podzielenie na slowa
books_words_cnt <- books %>%
  unnest_tokens(word, text, token = "words") %>%
  count(author, title, word) %>%
  mutate(book = paste0(author, "\n", title)) %>%
  group_by(book) %>%
  mutate(book_words = sum(n)) %>%
  ungroup() %>%
  mutate(p = 100*n/book_words) %>%
  select(-book)




###############
#### CZĘŚĆ ####
###############

cor_author <- function(autor_potencjalny, autor_nieznany_words) {
  corr_df <- books_words_cnt %>%
    filter(author == autor_potencjalny) %>%
    select(word, p) %>%
    inner_join(autor_nieznany_words, by = "word") %>%
    set_names(c("word", "znany", "nieznany")) %>%
    filter(!word %in% stop_words_pl)

  cor(corr_df$znany, corr_df$nieznany)

}



ksiazka_testowa <- "Syzyfowe prace" # "Siłaczka"
# ksiazka_testowa <- "Antek"
# ksiazka_testowa <- "Janko Muzykant"
# ksiazka_testowa <- "Chłopi"
# ksiazka_testowa <- "Trzej muszkieterowie"

autor_nieznany <- books_words_cnt %>% filter(title == ksiazka_testowa) %>% select(word, p)
autor_znany <- books_words_cnt %>% filter(title != ksiazka_testowa)



autorzy <- unique(autor_znany$author)

korelacja_tab <- tibble()

for(i in 1:length(autorzy)) {
  korelacja_tab <- bind_rows(korelacja_tab,
                             tibble(autor = autorzy[[i]],
                                    korelacja = cor_author(autorzy[[i]], autor_nieznany)))

}


korelacja_tab %>%
  mutate(autor = reorder(autor, korelacja)) %>%
  ggplot(aes(autor, korelacja)) +
  geom_col() +
  coord_flip()
