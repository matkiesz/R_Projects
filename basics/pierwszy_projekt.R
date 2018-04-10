library(devtools) #wczytanie biblioteki do instalacji pakietow
install.packages("caroline") #biblioteka do pracy z baza danych
install_github("rstudio/shiny") # biblioteka do budowania aplikacji wizualizowanych wynikow
install.packages("shiny")

#CSV
# wczytaj plik, pierwszy argument (file) to sciezka pliku, 
# drugi (sep) to czym rozdzielone sa kolmny (separator), 
# trzeci(dec) to jak zapisane sa liczby zmienno przecinkowe (z przecinkiem czy kropka)
# czwarty (header) to informacja czy w pliku sa naglo³wki czy ich nie ma
wnioski_csv <- read.table(file = "wnioski.csv", sep = ",", dec=".", header=TRUE) 
wnioski_csv # wyswietl zawartosc data frame
print(wnioski_csv) # to samo co wyzej
head(wnioski_csv) #wyswietl kilka pierwszych rekordow

# XLSX
install.packages("openxlsx")
library(openxlsx) # zaladuj biblioteke openxlsx. Kod linijke wyzej tylko zainstalowac‚ pakiet, trzeba go uaktywnic‡
wnioski_xlsx <- read.xlsx(xlsxFile = "wnioski.xlsx", sheet = 1) # wczytaj plik, pierwszy argument to sciezka, drugi to ktory arkusz
head(wnioski_xlsx)

# HTML
install.packages("XML") # pobieramy konieczne pakiety
install.packages("RCurl")
library(XML) #wczytujeme pakiety
library(RCurl)
link <- "http://www.x-rates.com/table/?from=USD&amount=1" # definiujemy URL
typeof(link)
dane_ze_strony <- getURL(link)
dane_ze_strony
poberana_tabelka <- readHTMLTable(dane_ze_strony, stringAsFactors = FALSE) # probuje sparsowac HTMl do tabeli
poberana_tabelka
length(poberana_tabelka) # ile tabel udalo sie uzyskac?
poberana_tabelka[1] # wyswietl 1wszy element z wektora
poberana_tabelka[2] # wyswietl 2gi element z wektora

top10 <- poberana_tabelka[[1]] # zapisz 1wszy element wektora do zmiennej top10
top10
typeof(top10)

pelna_lista <- poberana_tabelka[[2]] 
pelna_lista
typeof(pelna_lista)

# JSON
install.packages("rjson") 
library("rjson")
json_link <- "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"
kraje <- fromJSON(file=json_link) # wczytujemy JSON
kraje 
kraje[[2]][[3]]$name # wyswietlam tylko nazwe dla trzeciego kraju z listy 
typeof(kraje)

# BAZA DANYCH
install.packages("RPostgreSQL")
library("RPostgreSQL")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "pg_2", host = "localhost", port = 5432,
  user = "postgres", 
  password = "postgres")
dbExistsTable(con, "wnioski") #sprawdz czy istnieje tabela na bazie danych
dbExistsTable(con, "wnioski2") #nie powinno zadziaæ‚ jesli nie ma tabeli wnioski2 na bazie danych

wnioski_db <- dbGetQuery(con, "SELECT * from wnioski") # pobierz dane z tabeli wnioski do data frame
typeof(wnioski_db)
class(wnioski_db)
summary(wnioski_db) # wyswietl statystki calego data frame

# Filtrowanie i sortowanie danych
library(dplyr) # biblioteka do filtrowania / sortowania / agregacji
przefiltrowane <- filter(wnioski_db, wnioski_db$kanal == 'bezposredni') # przefiltruj data frame wnioski db dla tylko bezposrednich wnioskow
posortowane <- arrange(przefiltrowane, partner, desc(data_utworzenia)) # posortuj przefiltrowane wnioski, najpierw po partnerze (rosnaco - domyslnie), potem po dacie utworzenia malejaco
podsumowanie <- summary(posortowane) # przypisz podsumowanie tabeli do nowego data frame

# Agregacje - bez grupowania
posortowane %>% # ten znak %>% oznacza, ze w kolejnej linii dalej beda pracowac na data frame posortowane
  summarise(sr_rekomp = mean(kwota_rekompensaty), 
            med = median(kwota_rekompensaty)) # tworzy data frame zawierajacy srednia… oraz mediane rekompensat

summarise(posortowane, sr_rekomp = mean(kwota_rekompensaty),
          med = median(kwota_rekompensaty)) # zrobi to samo co wyzej

# Grupowanie - srednia i mediana rekompensat oraz liczba wnioskow podrupowane wedlug typu wniosku
posortowane %>%
  group_by(typ_wniosku) %>% # pogrupuj dane wyzej po typach wniosku
  summarise(sr_rekomp = mean(kwota_rekompensaty),
            med = median(kwota_rekompensaty), 
            liczba = n()) # kolejna kolumna w data frame, mowiac ile jest tam rekordow


# ile jest wnioskow w kazdym stanie i jaka jest wartosc rekompensat?
posortowane %>%
  group_by(stan_wniosku) %>%
  summarise(liczba = n(), kasa = sum(kwota_rekompensaty))


# FUNKCJA PIERWIASTEK KWADRAT
wylicz_wartosc <- function(x,y) {
  print(x)
  print(y)
  print(sqrt(x^2+y^2))
  
  print("zakonczylem prace") 
  return(sqrt(x^2+y^2)) 
}

# funkcja, ktora bedzie zwracac data frame z danych wczytanych z okreslonej lokalizacji (link) z podanym typem (csv, xlsx, html, json)
pobierz_dane <- function(typ_wejscia, link) {
  if(identical(typ_wejscia,"csv")) { # identical odpowiada za sprawdzenie czy dwa tekstu sa takie same
    dane <- read.table(file = as.character(link),sep=",", dec=".", header=TRUE)
  } else if(identical(typ_wejscia,"xlsx")) {
    library(openxlsx)
    dane <- read.xlsx(xlsxFile = as.character(link), sheet = 1)
  } else if(identical(typ_wejscia,"html")) {
    library(XML) #wczytuje pakiety
    library(RCurl)
    xData <- getURL(link) # pobieram dane
    dane <- readHTMLTable(xData, stringsAsFactors = FALSE) # probujemy sparsowac HTMl do tabeli
  } else if(identical(typ_wejscia,"json")) {
    library("rjson") # uaktywniam konieczne pakiety
    dane <- fromJSON(file=link) # wczytuje JSON
  } else if(identical(typ_wejscia,"db")) {
    library("RPostgreSQL")
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = "pg_2", host = "localhost", port = 5432,
       user = "postgres", 
       password = "postgres")
    dane <- dbGetQuery(con, paste0("SELECT * from ",link))
  }
  return(dane)
}


