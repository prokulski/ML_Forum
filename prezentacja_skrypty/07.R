library(tidyverse)
library(tidytext)
library(tm)
library(e1071)
library(xgboost)
library(randomForest)

rm(list = ls())
setwd(here::here())


books <- readRDS("data/books_all.RDS")

# słowniki
stop_words_pl <- read_lines("dicts/polish_stopwords.txt")

ksiazka_testowa <- "Syzyfowe prace"
# ksiazka_testowa <- "Antek"
# ksiazka_testowa <- "Janko Muzykant"
# ksiazka_testowa <- "Chłopi"
# ksiazka_testowa <- "Trzej muszkieterowie"

autor_nieznany <- books %>% filter(title == ksiazka_testowa)
autor_znany <- books %>% filter(title != ksiazka_testowa) %>%
  group_by(author) %>% sample_frac(0.25) %>% ungroup()


prepare_DTM <- function(f_book) {
  # podzielenie na slowa
  f_book %>%
    unnest_tokens(word, text, token = "words") %>%
    filter(!word %in% stop_words_pl) %>% # usuwamy stop words
    filter(is.na(as.numeric(word))) %>% # usuwamy liczby
    mutate(book = paste0(author, "_", part, par)) %>%
    count(author, book, word) %>%
    group_by(author) %>%
    mutate(p = 100*n/sum(n)) %>%
    ungroup() %>%
    select(book_id = book, word, p) %>%
    cast_dtm(book_id, word, p) %>%
    removeSparseTerms(0.998) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column("book_id") %>%
    separate(book_id, c("book_author", "author_remove"), sep = "_") %>%
    select(-author_remove) %>%
    mutate(book_author = factor(book_author))
}


autor_nieznany_dtm <- prepare_DTM(autor_nieznany)
dim(autor_nieznany_dtm)

autor_znany_dtm <- prepare_DTM(autor_znany)
dim(autor_znany_dtm)


# niekoniecznie w obu macierzach musimu mieć takie same kolumny
# zostawiamy tylko wspólne (występujące w "nieznany")
wspolne_kolumny <- intersect(colnames(autor_nieznany_dtm),
                             colnames(autor_znany_dtm))
length(wspolne_kolumny)



autor_nieznany_dtm <- autor_nieznany_dtm[, wspolne_kolumny]
dim(autor_nieznany_dtm)

autor_znany_dtm <- autor_znany_dtm[, wspolne_kolumny]
dim(autor_znany_dtm)



# zmiana liczby kolumn - SVD
autor_nieznany_dtm_svd <- svd(autor_nieznany_dtm %>%
                                select(-book_author) %>%
                                as.matrix() %>%
                                t(), nv = 500)
autor_nieznany_dtm_temp <- as.data.frame(autor_nieznany_dtm_svd$v)
autor_nieznany_dtm_temp$book_author <- autor_nieznany_dtm$book_author
autor_nieznany_dtm <- autor_nieznany_dtm_temp

rm(autor_nieznany_dtm_temp, autor_nieznany_dtm_svd)
dim(autor_nieznany_dtm)



autor_znany_dtm_svd <- svd(autor_znany_dtm %>%
                             select(-book_author) %>%
                             as.matrix() %>%
                             t(), nv = 500)
autor_znany_dtm_temp <- as.data.frame(autor_znany_dtm_svd$v)
autor_znany_dtm_temp$book_author <- autor_znany_dtm$book_author
autor_znany_dtm <- autor_znany_dtm_temp

rm(autor_znany_dtm_temp, autor_znany_dtm_svd)
dim(autor_znany_dtm)




saveRDS(autor_nieznany_dtm, "data/autor_nieznany_dtm.RDS")
saveRDS(autor_znany_dtm, "data/autor_znany_dtm.RDS")


autor_nieznany_dtm <- readRDS("data/autor_nieznany_dtm.RDS")
autor_znany_dtm <- readRDS("data/autor_znany_dtm.RDS")



autor_znany_dtm %>%
  gather("key", "val", -book_author) %>%
  group_by(book_author, key) %>%
  summarise(m = mean(val)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(key, m, color = book_author), size = 0.1) +
  facet_wrap(~book_author)

autor_nieznany_dtm %>%
  gather("key", "val", -book_author) %>%
  group_by(book_author, key) %>%
  summarise(m = mean(val)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(key, m))






###############
#### CZĘŚĆ ####
###############


#### Naive Bayes ----
model_nb <- naiveBayes(book_author ~ .,
                       data = autor_znany_dtm)

saveRDS(model_nb, "data/model_nb.RDS")

model_nb <- readRDS("data/model_nb.RDS")

pred_bayes <- predict(model_nb,
                      newdata = autor_nieznany_dtm %>% select(-book_author))

bayes_tab <- pred_bayes %>%
  table() %>%
  as.data.frame() %>%
  set_names(c("author", "freq")) %>%
  mutate(proc = 100*freq/sum(freq)) %>%
  arrange(proc, author) %>%
  mutate(author = fct_inorder(author))

ggplot(bayes_tab, aes(author, proc)) +
  geom_col() +
  coord_flip() +
  labs(title = paste0(ksiazka_testowa, " - kto jest autorem? NaiveBayes"),
       x = "", y = "Prawdopodobieństwo")






###############
#### CZĘŚĆ ####
###############


#### Random Forest ----

library(randomForest)

model_rf <- randomForest(book_author ~ .,
                         data = autor_znany_dtm)


saveRDS(model_rf, "data/model_rf.RDS")

model_rf <- readRDS("data/model_rf.RDS")


pred_fr <- predict(model_rf,
                   newdata = autor_nieznany_dtm %>% select(-book_author))


rf_tab <- pred_fr %>%
  table() %>%
  as.data.frame() %>%
  set_names(c("author", "freq")) %>%
  mutate(proc = 100*freq/sum(freq)) %>%
  arrange(proc, author) %>%
  mutate(author = fct_inorder(author))

ggplot(rf_tab, aes(author, proc)) +
  geom_col() +
  coord_flip() +
  labs(title = paste0(ksiazka_testowa, " - kto jest autorem? Random Forest"),
       x = "", y = "Prawdopodobieństwo")





###############
#### CZĘŚĆ ####
###############



#### SVM ----
model_svm <- svm(book_author ~ .,
                 data = autor_znany_dtm,
                 type = "C-classification")


saveRDS(model_svm, "data/model_svm.RDS")

model_svm <- readRDS("data/model_svm.RDS")


pred_svm <- predict(model_svm,
                    newdata = autor_nieznany_dtm %>% select(-book_author))

svm_tab <- pred_svm %>%
  table() %>%
  as.data.frame() %>%
  set_names(c("author", "freq")) %>%
  mutate(proc = 100*freq/sum(freq)) %>%
  arrange(proc, author) %>%
  mutate(author = fct_inorder(author))

ggplot(svm_tab, aes(author, proc)) +
  geom_col() +
  coord_flip() +
  labs(title = paste0(ksiazka_testowa, " - kto jest autorem? SVM"),
       x = "", y = "Prawdopodobieństwo")







###############
#### CZĘŚĆ ####
###############



#### XGBoost ----
xgb_train_X <- autor_znany_dtm %>% select(-book_author) %>% as.matrix()
xgb_train_Y <- as.numeric(as.factor(autor_znany_dtm$book_author))-1


xgb_params <- list(objective = "multi:softprob",
                   eval_metric = "mlogloss",
                   num_class = length(unique(autor_znany_dtm$book_author)),
                   eta = 0.2,
                   gamma = 0.3,
                   min_child_weight = 50,
                   subsample = 0.8,
                   colsample_bytree = 0.6,
                   max_depth = 14)

model_xgb <- xgb.train(data = xgb.DMatrix(xgb_train_X, label = xgb_train_Y),
                       params = xgb_params,
                       nrounds = 25,
                       verbose = 1)


saveRDS(model_xgb, "data/model_xgb.RDS")

model_xgb <- readRDS("data/model_xgb.RDS")


xgb_test <- autor_nieznany_dtm %>% select(-book_author) %>% as.matrix()

pred_xgb <- predict(model_xgb, xgb_test) %>%
  matrix(ncol = length(unique(autor_znany_dtm$book_author)))


xgb_tab <- apply(pred_xgb, 1, which.max) %>%
  table() %>%
  tibble(score = .) %>%
  mutate(score = gsub(" ", "", score) %>%
           as.numeric()) %>%
  mutate(proc = 100 * score / apply(pred_xgb, 1, which.max) %>% table() %>% sum(),
         author = unique(autor_znany_dtm$book_author)) %>%
  arrange(proc) %>%
  mutate(author = fct_inorder(author))

ggplot(xgb_tab, aes(author, proc)) +
  geom_col() +
  coord_flip() +
  labs(title = paste0(ksiazka_testowa, " - kto jest autorem? XGBoost"),
       x = "", y = "Prawdopodobieństwo")







###############
#### CZĘŚĆ ####
###############



#### porównanie modeli ----
pred_sum <- inner_join(bayes_tab %>% select(author, naivebayes = proc),
                       svm_tab %>% select(author, svm = proc),
                       by = "author") %>%
  inner_join(xgb_tab %>% select(author, xgboost = proc),
             by = "author") %>%
  inner_join(rf_tab %>% select(author, randomforest = proc),
             by = "author") %>%
  gather(key = "method", value = "proc", -author)


ggplot(pred_sum, aes(author, proc)) +
  geom_col(aes(fill = method), position = position_dodge()) +
  coord_flip() +
  labs(title = paste0(ksiazka_testowa, " - kto jest autorem? Zestawienie metod"),
       x = "", y = "Prawdopodobieństwo")



pred_sum %>%
  group_by(author) %>%
  summarise(proc = mean(proc)) %>%
  ungroup() %>%
  mutate(author = fct_reorder(author, proc)) %>%
  ggplot(aes(author, proc)) +
  geom_col() +
  coord_flip() +
  labs(title = paste0(ksiazka_testowa, " - kto jest autorem? Zestawienie metod"),
       x = "", y = "Prawdopodobieństwo")

