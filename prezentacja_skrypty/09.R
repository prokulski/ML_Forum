library(tidyverse)
library(rvest)
library(glue)
library(lubridate)

rm(list = ls())
setwd(here::here())

###############
#### CZĘŚĆ ####
###############


# pobranie listy linków do wypowiedzi
max_page_no <- 63

links_all <- vector()

for(i in 1:max_page_no) {
  url <- glue("http://www.prezydent.pl/aktualnosci/wypowiedzi-prezydenta-rp/wystapienia/page,{i}.html")
  page <- read_html(url)

  links <- page %>%
    html_nodes("div.news-list") %>%
    html_node("a") %>%
    html_attr("href") %>%
    paste0("http://www.prezydent.pl", .)
  links_all <- c(links_all, links)
}


links_all <- links_all[str_detect(links_all, "http://www.prezydent.pl/aktualnosci/wypowiedzi-prezydenta-rp/wystapienia/art,")]
links_all <- unique(links_all)


# funkcja zwracajaca datę z polskiego ciągu
parse_pl_date <- function(f_data) {
  # slownik miesiecy
  miesiace <- c("stycznia", "lutego", "marca",
                "kwietnia", "maja", "czerwca",
                "lipca", "sierpnia", "września",
                "października", "listopada", "grudnia")

  # rozbicie daty na dzien - miesiac - rok
  data_tab <- f_data %>%
    str_split(",") %>%
    unlist() %>%
    .[[2]] %>%
    trimws() %>%
    str_split(" ") %>%
    unlist()

  return(make_date(year = data_tab[3], # rok
                   month = which(miesiace == data_tab[2]),  # numer miesiaca
                   day = data_tab[1])) # dzien
}


# pobranie wypowiedzi
wypowiedzi <- tibble()

for(i in 1:length(links_all)) {

  page <- read_html(links_all[[i]])

  # tresc wystapienia
  speech <- page %>%
    html_node("div.description") %>%
    html_text() %>%
    str_replace_all("\\s+", " ")

  # data wystapienia
  data <- page %>%
    html_node("div.webreader") %>%
    html_node("div.date") %>%
    html_text() %>%
    trimws() %>%
    parse_pl_date()

  wypowiedzi <- bind_rows(wypowiedzi, tibble(text = speech, date = data))
}


wypowiedzi <- wypowiedzi %>%
  arrange(date) %>%
  mutate(id = row_number(),
         text = trimws(text))

saveRDS(wypowiedzi, "data/wypowiedzi_Duda.RDS")






###############
#### CZĘŚĆ ####
###############



library(tidytext)
library(widyr)
library(wordcloud)
library(ggrepel)
library(ggridges)

wypowiedzi <- readRDS("data/wypowiedzi_Duda.RDS")

stop_words_pl <- read_lines("dicts/polish_stopwords.txt")


# podzielenie na slowa z zachowaniem czasu
wypowiedzi_words <- wypowiedzi %>%
  unnest_tokens(word, text, token = "words") %>%
  filter(!word %in% stop_words_pl) %>%
  filter(is.na(as.numeric(word))) %>%
  count(id, date, word) %>%
  group_by(id) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup()


# agregacja po słowach - uwalniamy się od czasu
wypowiedzi_cloud <- wypowiedzi_words %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  ungroup()


wordcloud(wypowiedzi_cloud$word,
          wypowiedzi_cloud$n,
          max.words = 150,
          scale = c(2, 0.5),
          colors = colorRampPalette(c("#313695", "#a50026"))(15))






###############
#### CZĘŚĆ ####
###############


# stemming
stem_dict <- readRDS("dicts/polimorfologik.RDS")

wypowiedzi_cloud_stem <- wypowiedzi_cloud %>%
  filter(n > 2) %>%
  left_join(stem_dict, by = "word") %>%
  mutate(stem = if_else(is.na(stem), word, stem)) %>%
  group_by(stem) %>%
  summarise(n = sum(n)) %>%
  ungroup()

wordcloud(wypowiedzi_cloud_stem$stem,
          wypowiedzi_cloud_stem$n,
          max.words = 150,
          scale = c(2, 0.5),
          colors = colorRampPalette(c("#313695", "#a50026"))(15))





###############
#### CZĘŚĆ ####
###############



# Kaczyński, Szydło czy Morawiecki?
wypowiedzi_cloud_stem %>%
  rename(word = stem) %>%
  filter(str_detect(word, "kaczyński|szydło|morawiecki|macierewicz")) %>%
  mutate(kolor = str_sub(word, 1, 6),
         word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n, fill = kolor), show.legend = FALSE) +
  coord_flip()


wypowiedzi_words %>%
  filter(str_detect(word, "kaczyński|szydło|morawiecki|macierewicz")) %>%
  mutate(kolor = str_sub(word, 1, 6)) %>%
  group_by(date, kolor) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_ridgeline(aes(date, kolor, height = n, fill = kolor),
                 scale = 0.09, alpha = 0.8, show.legend = FALSE) +
  geom_vline(xintercept = as_date(c("2015-11-16", # początek rządu Szydło
                                  "2017-12-11", # koniec rządu Szydło
                                  "2018-01-09"))) + # Maciarewicz przestaje być szefem MON
  scale_y_discrete(labels = c("szydło" = "Szydło", "morawi" = "Morawiecki",
                              "macier" = "Macierewicz", "kaczyń" = "Kaczyński")) +
  labs(x = "", y = "Liczba wspomnień w wystąpieniach")





###############
#### CZĘŚĆ ####
###############

# DTM jako macierz
podobienstwo_mat <- wypowiedzi_words %>%
  cast_dtm(word, id, n) %>%
  as.matrix()

dim(podobienstwo_mat)

# odległosc kosinusowa pomiedzy dokumentami
podobienstwo_mat_dist <- lsa::cosine(podobienstwo_mat)

saveRDS(podobienstwo_mat_dist, "data/podobienstwo_mat_dist.RDS")

podobienstwo_mat_dist <- readRDS("data/podobienstwo_mat_dist.RDS")

dim(podobienstwo_mat_dist)


podobienstwo_dist_df <- podobienstwo_mat_dist %>%
  as.data.frame() %>%
  rownames_to_column("doc1") %>%
  gather("doc2", "dist", -doc1) %>%
  filter(doc1 > doc2)


ggplot(podobienstwo_dist_df) + geom_tile(aes(doc1, doc2, fill = dist))


ggplot(podobienstwo_dist_df, aes(dist)) + geom_density(fill = "lightgreen")







###############
#### CZĘŚĆ ####
###############



# najbadziej podobne wypowiedzi
podobienstwo_dist_df %>% filter(dist == max(dist))


WybA = 79
WybB = 268

wypowiedzi %>% filter(id == WybA) %>% pull(text)
wypowiedzi %>% filter(id == WybB) %>% pull(text)


# lista współwystępujących słów
intersect(wypowiedzi_words %>% filter(id == WybA) %>% pull(word),
          wypowiedzi_words %>% filter(id == WybB) %>% pull(word))



par(mfrow = c(1,2))

wordcloud(wypowiedzi_words %>% filter(id == WybA) %>% pull(word),
          wypowiedzi_words %>% filter(id == WybA) %>% pull(n),
          max.words = 150,
          scale = c(2, 0.5),
          colors = colorRampPalette(c("#313695", "#a50026"))(15))


wordcloud(wypowiedzi_words %>% filter(id == WybB) %>% pull(word),
          wypowiedzi_words %>% filter(id == WybB) %>% pull(n),
          max.words = 150,
          scale = c(2, 0.5),
          colors = colorRampPalette(c("#313695", "#a50026"))(15))

par(mfrow = c(1,1))


# popularność współwystępujących słów w podobnych wypowiedziach
wypowiedzi_words %>%
  filter(id %in% c(WybA, WybB)) %>%
  group_by(id) %>%
  top_n(20, p) %>%
  ungroup() %>%
  arrange(p) %>%
  mutate(word = fct_inorder(word)) %>%
  ggplot() +
  geom_col(aes(word, p, fill = as.factor(id)), position = position_dodge()) +
  coord_flip()



wypowiedzi_words %>%
  filter(id %in% c(WybA, WybB)) %>%
  filter(word %in% intersect(wypowiedzi_words %>% filter(id == WybA) %>% pull(word),
                             wypowiedzi_words %>% filter(id == WybB) %>% pull(word))) %>%
  select(word, id, p) %>%
  spread(id, p)




###############
#### CZĘŚĆ ####
###############



pca <- prcomp(t(podobienstwo_mat), scale. = TRUE)

plot(pca)


pca_df <- pca$x[, c(1,2)] %>%
  as_tibble() %>%
  set_names(c("PC1", "PC2")) %>%
  mutate(n = row_number())

pca_df$cluster <- kmeans(pca$x, 3)$cluster # 3 bo trzech rzeczników prasowych do tej pory

ggplot(pca_df, aes(PC1, PC2)) +
  geom_point(aes(color = as.factor(cluster)), size = 0.5) +
  geom_text(aes(PC1, PC2, label = if_else(PC1 > -100, "", as.character(n))), hjust = "left")


wypowiedzi %>%
  filter(id %in% c(37, 326, 282)) %>%
  pull(text) %>%
  str_sub(1, 1000)


wypowiedzi %>%
  mutate(cluster = pca_df$cluster) %>%
  ggplot(aes(date, as.factor(cluster), color = as.factor(cluster))) +
  geom_point()
