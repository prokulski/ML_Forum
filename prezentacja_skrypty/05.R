library(tidyverse)
library(tidytext)
library(topicmodels)

rm(list = ls())

setwd(here::here())

books <- readRDS("data/books_all.RDS")

# wartosci wzgledne bez stop words
stop_words_pl <- read_lines("dicts/polish_stopwords.txt")

# podzielenie na slowa
books_words <- books %>%
  unnest_tokens(word, text, token = "words") %>%
  filter(!word %in% stop_words_pl) %>%
  count(author, title, word) %>%
  ungroup() %>%
  arrange(author, title, desc(n), word)


# ile słów jest z danej ksiąki?
total_words <- books_words %>%
  group_by(author, title) %>%
  summarize(total = sum(n)) %>%
  ungroup()

books_words <- left_join(books_words, total_words, by = c("author", "title"))


# tf-idf
book_words <- books_words %>%
  mutate(book = paste(author, title)) %>%
  select(-author, -title) %>%
  bind_tf_idf(word, book, n)


# słowa, które występują najczęściej w dokumentach
book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))


book_words %>%
  arrange(tf_idf) %>%
  mutate(word = fct_inorder(word)) %>%
  group_by(book) %>%
  top_n(10, tf_idf) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 5, scales = "free") +
  coord_flip()


# najpierw DTM
book_dtm <- book_words %>%
  cast_dtm(book, word, n)

book_dtm




# LDA, k = liczba tematów - tutaj tyle ile autorów (14)
k <- length(unique(books$author))
book_lda <- LDA(book_dtm, k = k, control = list(seed = 1234))

book_lda <- readRDS("data/book_lda.RDS")



#  Word-topic probabilities - jakie słowa określają temat?
book_topics <- tidy(book_lda, matrix = "beta")

book_topics




# po 10 najpopularniejszych słów określających każdy z tematów
ap_top_terms <- book_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()






# Document-topic probabilities - jakie dokumenty pasują do tematu?
ap_documents <- tidy(book_lda, matrix = "gamma")

ap_documents


ap_documents %>%
  ggplot() +
  geom_boxplot(aes(as.factor(topic), gamma)) +
  facet_wrap(~document, ncol = 5)


ap_documents %>%
  group_by(document, topic) %>%
  summarise(m_gamma = mean(gamma)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(as.factor(topic), document, size = m_gamma))



ap_documents %>%
  filter(topic == 3, gamma > 0.9)

ap_top_terms %>%
  filter(topic == 3) %>%
  arrange(desc(beta))


ap_documents %>%
  filter(document == "Bolesław Prus Antek") %>%
  arrange(desc(gamma))

ap_top_terms %>%
  filter(topic %in% c(5, 8, 10)) %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, desc(beta))
