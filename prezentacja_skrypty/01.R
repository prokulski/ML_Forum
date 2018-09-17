# Skrypt pobiera książki z wolnelektury.pl wskazane w pliku csv


library(tidyverse)
library(rvest)
library(fs)



#### Funkcja pobierająca książkę (plik epub)
download_book <- function(epub_url, book_title, book_author) {

  # robimy folder tymczasowy
  dir_create("temp")

  temp_zip_file <- str_split(epub_url, "/") %>%
    unlist() %>%
    .[[length(.)]] %>%
    gsub(".epub", ".zip", ., fixed = TRUE)

  # pobrac plik z sieci i zapisac jako zip
  download.file(epub_url, destfile = paste0("temp/", temp_zip_file), quiet = TRUE)

  # rozpakować
  unzip(zipfile = paste0("temp/", temp_zip_file), exdir = "temp")

  # z folderu OPS wujmujemy wszystkie pliki part*.html
  book_parts <- paste0("temp/OPS/", list.files("temp/OPS", "part.*\\.html"))

  whole_book <- tibble()

  # z nich wyciagamy same teksty (usuwamy linki itp)
  for(n_part in seq_along(book_parts)) {

    book_part <- read_html(book_parts[[n_part]])

    # z kolejnych paragrafów usywamy linki, a póżniej tagi htmla
    part_paragraphs <- book_part %>%
      html_nodes("p")

    # jeśli nie ma paragrafów to porzucamy tą częśc książki
    if(length(part_paragraphs) == 0) next

    # wyłuskujemy tekst
    part_paragraphs <- part_paragraphs %>%
      as.character() %>%
      gsub("<a.*>.*</a>", "", .) %>%
      gsub("<.*?>", "", .)

    # składamy linie w ramkę
    whole_book <- bind_rows(whole_book,
                            tibble(
                              part = book_parts[[n_part]] %>%
                                gsub("temp/OPS/part|\\.html", "", .) %>%
                                as.numeric(),
                              paragraph_number = 1:length(part_paragraphs),
                              text = part_paragraphs
                            ))
  }

  # dodajemu tytuł i autora książki oraz sortujemy w odpowiedniej kolejności
  whole_book <- whole_book %>%
    mutate(title = book_title,
           author = book_author) %>%
    arrange(part, paragraph_number)

  # ponownie numerujemy części - żeby pozbyć się dziur
  whole_book$part <- group_indices(whole_book, part)

  whole_book <- select(whole_book, author, title, part, par=paragraph_number, text)

  # usuwamy niepotrzebne pliki z całym folderem
  file_delete("temp")

  # zwracamy data frame z ksiazka
  return(whole_book)
}


# lista książek
book_list <- read_csv2("data/book_links.csv")

# wciągamy wszystkie książki do jednej dużej tabeli
books_all <- tibble()

for(book_number in 1:nrow(book_list)) {

  cat(paste0(book_number, ": ", book_list$autor[[book_number]], " - \"", book_list$tytul[[book_number]], "\""))

  # pobieramy kolejne ksiazki i dodajemy je do duzej tabeli
  book_tmp <- download_book(epub_url = book_list$link[[book_number]],
                            book_author = book_list$autor[[book_number]],
                            book_title = book_list$tytul[[book_number]])

  books_all <- bind_rows(books_all, book_tmp)

  cat("\n")
}

# poprawka apostrofów w "Trzech muszkieterach"
books_all$text <- str_replace_all(books_all$text, "‘", "’")


# zapisujemy komplet danych
saveRDS(books_all, "data/books_all.RDS")
