library(tidyverse)
library(tidytext)
library(topicmodels)

###############
#### CZĘŚĆ ####
###############


#### TF-IDF ----
doc1 <- "to jest przykład"
doc2 <- "a to jest inne to zdanie"

documents <- tibble(doc = c("A","B"), text = c(doc1, doc2))

documents %>%
  unnest_tokens(word, text) %>%
  count(word)

documents %>%
  unnest_tokens(word, text) %>%
  count(doc, word) %>%
  bind_tf_idf(word, doc, n)


#### DTM ----

dtm <- documents %>%
  unnest_tokens(word, text) %>%
  count(doc, word) %>%
  bind_tf_idf(word, doc, n) %>%
  cast_dtm(doc, word, n)

dtm

as.matrix(dtm)





###############
#### CZĘŚĆ ####
###############


### LDA ---
lda <- LDA(dtm, k = 2, control = list(seed = 1234))



#### LDA beta ----
tidy(lda, matrix = "beta")


tidy(lda, matrix = "beta") %>%
  ggplot() +
  geom_point(aes(term, beta, color = as.factor(topic)), size = 5) +
  labs(title = paste0("Doc A = ", doc1, "\nDoc B = ", doc2))




#### LDA gamma ----
tidy(lda, matrix = "gamma")

tidy(lda, matrix = "gamma") %>%
  ggplot() +
  geom_point(aes(document, gamma, color = as.factor(topic)), size = 5) +
  labs(title = paste0("Doc A = ", doc1, "\nDoc B = ", doc2))
