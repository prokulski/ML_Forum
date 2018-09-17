# Analiza języka naturalnego w R


Repozytorium zawiera materiały omawiane w ramach warsztatu **Analiza języka naturalnego w R**, prowadzonego podczas konferencji **Machine Learning@Enterprise 2018** (18 września 2018 r.).

## Agenda:

* Do czego może przydać się analiza tekstu
* Skąd wziąć dane tekstowe
* Przygotowanie i podstawowa analiza tekstu
* Zagadka kryminalna: kto napisał książkę

## Co użytkownik zyska:

Dowiesz się jak przetworzyć tekst w języku R z użyciem stosownych pakietów, wyszukać najpopularniejsze słowa (tf-idf). Spróbujemy znaleźć tematy kilku lektur szkolnych (LDA), porównać język ich autorów i znajdziemy autora nieznanego tekstu.

## Wymagania wobec uczestnika:

* Zainteresowanie tematyką przetwarzania tekstu
* Podstawowa znajomość programowania (R)
* Przydatna będzie znajomość pakierów tidyverse i tidytext
* Własny komputer z zainstalowanym R/RStudio – dla chcących aktywnie uczestniczyć w warsztacie


# Zawartość repo:

* **prezentacja** - folder z prezentacją (treść i pliki)
* **prezentacja_skrypty** - skrypty omawiane podczas prezentacji
* **data** miejsce na dane - ściągnięte albo obliczone
* **dicts** - potrzebne słowniki


# Potrzebne pakiety:

* manipulacja danymi, wykresy: 
    + tidyverse (wraz z zależnościami, szczególnie dplyr i ggplot2)
    + widyr
    + lubridate
    + glue
* analiza i manipulacja danymi tekstowymi:
    + tidytext
    + tm
    + topicmodels
    + text2vec
    + lsa
    + wordcloud
* modele:
    + e1071
    + randomForest
    + xgboost
* pobieranie danych: 
    + rvest
    + rtweet
* grafy:
    + igraph
* inne przydatne:
    + fs
    + ggrepel
    + ggridges


# Do poczytania przy okazji

**Kilka tekstów związanych z tematem warsztatu:**

* [Kto napisał tę książkę?](http://prokulski.net/index.php/2017/06/30/kto-napisal-te-ksiazke/)
* [Analiza tekstów z wiadomości](http://prokulski.net/index.php/2017/09/14/analiza-tekstow-z-wiadomosci/)
* [Analiza twórczości J.K.Rowling](http://prokulski.net/index.php/2017/07/03/analiza-tworczosci-j-k-rowling/)
* [Diagnoza, Wataha czy Belfer?](http://prokulski.net/index.php/2017/12/29/diagnoza-wataha-czy-belfer/)
* [Oscary 2017 – kto wygrał? Bez czytania wiadomości!](http://prokulski.net/index.php/2017/02/27/oscary-2017-kto-wygral-bez-czytania-wiadomosci/)
