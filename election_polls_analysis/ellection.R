library(tidyverse)
library(rvest)
library(lubridate)
library(forecast)
library(XML)
library(RCurl)

link <- "https://docs.google.com/spreadsheets/d/1P9PG5mcbaIeuO9v_VE5pv6U4T2zyiRiFK_r8jVksTyk/htmlembed?single=true&gid=0&range=a10:o400&widget=false&chrome=false"
xData <- getURL(link) 
dane_z_html <- readHTMLTable(xData, stringsAsFactors = FALSE, skip.rows = c(1,3), encoding = "utf8")
df <- as.data.frame(dane_z_html)
colnames(df) <- df[1,]
df2 <- df[2:nrow(df),]

df2$PiS <- gsub(",", ".", df2$PiS)

for(col in 8:16){
  df2[, col] <- as.numeric(gsub(",", ".", df2[, col]))
}
class(df2$PiS)

# pobieramy tabele z tresci strony
page <- read_html(url)
sondaze_node <- page %>% html_nodes("table")
sondaze_tab <- sondaze_node %>% html_table(fill = TRUE)
sondaze_df <- as.data.frame(sondaze_tab)

# potrzebujemy tabeli bez gornych wierszy
sondaze_final <- sondaze[3:nrow(sondaze_df),c(2,4,5,6,8:16)]

# nadajemy nazwy kolumnom
colnames(sondaz1)[c(1, 2,5, 6, 7, 10,14)]=cbind("lp", "osrodek", "metoda_badania", "uwzgl_niezdec", "termin_badania", "K15","PARTIA_RAZEM" )

# poprawiamy typ daty
sondaze_final$Publikacja <- dmy(sondaze_final$Publikacja)

# zmieniamy przecinki na kropki, a pozniej ciag na liczbe™
for(col in 5:13) {
  sondaze_final[,col] <- as.numeric(gsub(",", ".", sondaze_final[,col])) 
}
class(sondaze_final$PiS)
rm(sondaze_final)


ggplot(data = sondaze_final) +
  geom_point(aes(Publikacja, PiS, group = Osrodek, color = Osrodek)) +
  geom_smooth(aes(Publikacja, PiS))


