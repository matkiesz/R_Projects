library(devtools) #wczytanie biblioteki do instalacji pakietow
install.packages("tidyverse")
library(tidyverse)

library("RPostgreSQL")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "pg_2", host = "localhost", port = 5432,
                 user = "postgres", 
                 password = "postgres") 


# 1. geom_point
rekompensaty_db <- dbGetQuery(con, "SELECT * from szczegoly_rekompensat") 
ggplot(data = rekompensaty_db) + 
  geom_point(mapping = aes(x = konto, y = kwota))

ggplot(data = rekompensaty_db) + 
  geom_point(mapping = aes(
    x = data_utworzenia, 
    y = kwota, 
    color = konto))

# task1
wnioski_db <- dbGetQuery(con, "SELECT * from wnioski") 
ggplot(data = wnioski_db) + 
  ylim(0, 2000) +
  geom_point(mapping = aes(
    x = data_utworzenia, 
    y = kwota_rekompensaty, 
    color = typ_wniosku))

#task 2
ggplot(data = rekompensaty_db) + 
  geom_point(mapping = aes(
    x = data_utworzenia, 
    y = kwota), 
    color = "blue")

# 2. facets
ggplot(data = wnioski_db) + 
  geom_point(mapping = aes(
    x = data_utworzenia, 
    y = kwota_rekompensaty, 
    color = typ_wniosku)) +
  facet_wrap(~ jezyk, ncol = 3)

ggplot(data = wnioski_db) + 
  geom_point(mapping = aes(
    x = data_utworzenia, 
    y = kwota_rekompensaty, 
    color = jezyk)) +
  facet_grid(kanal ~ typ_wniosku)


# 3. geom_smooth
ggplot(data = wnioski_db) + 
  geom_point(mapping = aes(
    x = liczba_pasazerow, 
    y = kwota_rekompensaty)) +
  geom_smooth(mapping = aes(
    x = liczba_pasazerow, 
    y = kwota_rekompensaty))

ggplot(data = wnioski_db) + 
  geom_point(mapping = aes(
    x = liczba_pasazerow, 
    y = kwota_rekompensaty)) +
  geom_smooth(mapping = aes(
    x = liczba_pasazerow, 
    y = kwota_rekompensaty, 
    linetype = kanal))

# task v1
ggplot(data = wnioski_db) + 
  geom_point(mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty)) +
  geom_smooth(mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty)) +
  facet_wrap(~ jezyk, ncol = 3)

# task v2
ggplot(data = wnioski_db) + 
  xlim(0,20) +
  ylim(0,12000) +
  geom_point(mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty, color = jezyk)) +
  geom_smooth(mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty)) +
  facet_wrap(~ jezyk, ncol = 3)

#reproduce code
ggplot(data = wnioski_db) + 
  geom_point(mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty)) +
  geom_smooth(mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty)) +
  facet_wrap(~ jezyk, ncol = 3)

ggplot(data = wnioski_db, mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty)) + 
  geom_point() +
  geom_smooth() +
  facet_wrap(~ jezyk, ncol = 3)

#task1
ggplot(data = wnioski_db, mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty)) + 
  geom_point(mapping = aes(color = kanal)) +
  geom_smooth() +
  facet_wrap(~ jezyk, ncol = 3)

#filtering
ggplot(data = wnioski_db, mapping = aes(x = liczba_pasazerow, y = kwota_rekompensaty)) + 
  geom_point(mapping = aes(color = kanal)) +
  geom_smooth(data = filter(wnioski_db, kod_kraju == "PL"), aes(color = "PL")) +
  geom_smooth(data = filter(wnioski_db, kod_kraju == "ES"), aes(color = "ES")) +
  scale_colour_manual(name="legend", values=c("red", "blue", "black", "orange"))


#task2
ggplot(data = rekompensaty_db, mapping = aes(x = data_utworzenia, y = kwota)) + 
  geom_point() +
  geom_smooth()
ggplot(data = rekompensaty_db) + 
  geom_point(mapping = aes(x = data_utworzenia, y = kwota)) +
  geom_smooth(mapping = aes(x = data_utworzenia, y = kwota))

ggplot(data = rekompensaty_db, mapping = aes(x = data_utworzenia, y = kwota)) + 
  geom_point(data = filter(rekompensaty_db, konto == "BZWBK EUR"), mapping = aes(color = kwota)) +
  geom_smooth()

ggplot(data = filter(rekompensaty_db, konto == "BZWBK EUR"), mapping = aes(x = data_utworzenia, y = kwota)) + 
  geom_point(mapping = aes(color = kwota)) +
  geom_smooth(span = 0.75)

#geom_bar
ggplot(data = wnioski_db) + 
  geom_bar(mapping = aes(x = typ_wniosku))

ggplot(data = wnioski_db) + 
  geom_bar(mapping = aes(x = typ_wniosku), stat = "count")

library(scales)
ggplot(data = wnioski_db) + 
  geom_bar(mapping = aes(x = typ_wniosku, y=..prop.., group = 1))+
  scale_y_continuous(labels=percent_format())

#task1
ggplot(data = wnioski_db) + 
  geom_bar(mapping = aes(x = typ_wniosku, y=..prop.., group = 1))+
  scale_y_continuous(labels=percent_format()) +
  facet_wrap(~ jezyk)

#stat summary
ggplot(data = wnioski_db, mapping = aes(x = jezyk, y = kwota_rekompensaty)) +
  stat_summary(
    fun.y = median, 
    fun.ymax = max, 
    fun.ymin = min
  )

ggplot(data = wnioski_db, mapping = aes(x = jezyk, y = kwota_rekompensaty)) +
  stat_summary(
    fun.ymin = min, 
    geom = "point"
  )

ggplot(data = wnioski_db, mapping = aes(x = jezyk, y = kwota_rekompensaty)) +
  stat_summary(
    fun.ymin = min, 
    fun.ymax = max,
    geom = "pointrange"
  )

#task1
ggplot(data = rekompensaty_db) +
  stat_summary(
    mapping = aes(x = konto, 
                  y = kwota),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

#box plot
ggplot(data = rekompensaty_db, mapping = aes(x = konto, y = kwota)) +
  geom_boxplot()

ggplot(data = wnioski_db, mapping = aes(x = jezyk, y = kwota_rekompensaty)) +
  geom_boxplot() +
  coord_flip()

ggplot(data = wnioski_db, mapping = aes(x = jezyk, y = kwota_rekompensaty)) +
  ylim(0,5000) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  coord_flip()

#geom histogram
ggplot(data = wnioski_db, mapping = aes(x = data_utworzenia)) +
  geom_histogram()


#geom tile
ggplot(data = wnioski_db) +
  geom_tile(mapping = aes(jezyk, kanal, fill = n))


wnioski_db %>%
  count(kanal, jezyk) %>%
  ggplot() +
  geom_tile(mapping = aes(kanal, jezyk, fill = n))

wnioski_db %>%
  count(liczba_pasazerow, kwota_rekompensaty) %>%
  ggplot(wnioski_db, mapping = aes(liczba_pasazerow, kwota_rekompensaty)) +
  geom_raster(aes(fill = density))


