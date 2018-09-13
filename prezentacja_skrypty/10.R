rm(list = ls())

library(tidyverse)
library(tidytext)
library(rtweet)
library(lubridate)

stop_words_pl <- read_lines("dicts/polish_stopwords.txt")

Sys.setenv(TWITTER_PAT="/home/lemur/RProjects/!tokens/twitter_token.rdata")

n_tweets <- 500

# najnowsze tweety z kilku tagów
rstatpl_tweets <- search_tweets("Sąd Najwyższy",
                              type = "recent",
                              n = n_tweets,
                              include_rts = FALSE,
                              retryonratelimit = TRUE) %>%
   filter(created_at >= today())


rstatpl_tweets %>%
   select(created_at, screen_name, text) %>%
   arrange(created_at) %>%
   knitr::kable()



rstatpl_tweets %>%
   mutate(created_at = floor_date(created_at, unit = "minutes")) %>%
   count(created_at) %>%
   ggplot() +
   geom_point(aes(created_at, n))


rstatpl_words <- rstatpl_tweets %>%
   select(screen_name, created_at, text) %>%
   unnest_tokens(word, text, token = "tweets") %>%
   filter(!word %in% stop_words_pl)


rstatpl_words_cnt <- rstatpl_words %>%
   count(word) %>%
   arrange(desc(n))


rstatpl_words_cnt %>%
   filter(n > 1) %>%
   top_n(20, n) %>%
   arrange(n) %>%
   mutate(word = fct_inorder(word)) %>%
   ggplot() +
   geom_col(aes(word, n)) +
   coord_flip()


wordcloud(rstatpl_words_cnt$word,
          rstatpl_words_cnt$n,
          max.words = 150,
          min.freq = 2,
          scale = c(2, 0.5),
          colors = colorRampPalette(c("#313695", "#a50026"))(15))


rstatpl_words %>%
   mutate(created_at = floor_date(created_at, unit = "minutes")) %>%
   count(created_at, word) %>%
   filter(n > 1) %>%
   group_by(word) %>%
   mutate(tot = sum(n)) %>%
   ungroup() %>%
   # filter(tot > quantile(tot, 0.25)) %>%
   arrange(tot) %>%
   mutate(word = fct_inorder(word)) %>%
   ggplot() +
   geom_point(aes(created_at, word, size = n), show.legend = FALSE)
