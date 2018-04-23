library(shinydashboard)
library(plyr)
library(dplyr)
library(scales)
library(wordcloud)
library(syuzhet)
library(tidyverse)
library(devtools)
library(shiny)
library(RCurl)
library(XML)
library(lubridate)
library(stringr)
library(ggplot2)
library(DT)
library(readtext)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(rsconnect)
library(rtweet)
library(httpuv)

rsconnect::setAccountInfo(name='mateuszkieszkowski',
                          token='BCD3F1D6DBA88C5D1553BCC2AB96DEB9',
                          secret='g/Z43nuYx0r2q2g+PAzNwACcE376ikFd133CjGXa')

link <- "https://docs.google.com/spreadsheets/d/1P9PG5mcbaIeuO9v_VE5pv6U4T2zyiRiFK_r8jVksTyk/htmlembed?single=true&gid=0&range=a10:o400&widget=false&chrome=false"
xData <- getURL(link) # pobieramy dane
sondaz <- as.data.frame(readHTMLTable(xData, stringsAsFactors = FALSE, skip.rows = c(1,3), header = FALSE, encoding = "utf8"))
colnames(sondaz) = sondaz[1, ]
sondaz1 <-sondaz[2:nrow(sondaz),]

for(col in 8:16) {
  sondaz1[, col] <-  as.numeric(gsub(",", ".", sondaz1[, col]))
}
#zmiana nazw kolumn
colnames(sondaz1)[c(1, 2,5, 6, 7, 10,14)]=cbind("lp", "osrodek", "metoda_badania", "uwzgl_niezdec", "termin_badania", "K15","PARTIA_RAZEM" )

#zmiana formatu tekstowego na date
sondaz1$Publikacja <- dmy(sondaz1$Publikacja)

wordfunction <- function (){
  text <- readLines("parties_en.txt",encoding = "UTF-8")
  docs <- Corpus(VectorSource(text))
  docs <- tm_map(docs, tolower)
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, stemDocument)
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <<- data.frame(word = names(v), freq=v)
}





ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard",
                  tags$li(a(href = 'https://github.com/matkiesz',
                            icon("github","fa-2x"),
                            title = "My GitHub Profile"),
                          class = "dropdown"),
                  tags$li(a(href = 'https://www.linkedin.com/in/mateusz-kieszkowski-276a6412a/',
                            icon("linkedin","fa-2x"),
                            title = "My Linkedin Profile"),
                          class = "dropdown")
  ),
  
  
  dashboardSidebar(
    sidebarMenu(id = "tab",
                menuItem("Elect poll analysis",icon = icon("align-left"),
                         menuSubItem("Summary table",tabName = "electpoll"),
                         menuSubItem("Choose customer survey",tabName =  "electpoll1"),
                         menuSubItem("Party Plot",tabName = "wykresypartii"),
                         menuSubItem("Geom_tile_chart", tabName = "electpoll2"),
                         menuSubItem("Parties Support", tabName = "partieschart")),
                
                menuItem("Text Mining", tabName = "electpoll",icon = icon("font"),
                         menuSubItem("Top 50 words", tabName = "top50"),
                         menuSubItem("Bar frequencies", tabName = "freqbar"),
                         menuSubItem("Table Frequencies", tabName ="tabbar"),
                         menuSubItem("Associations", tabName = "assocs")),
                menuItem("Twitter Sentiment Analysis", tabName = "twittsent", icon = icon("twitter-square"))
                
    )),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "wykresypartii", 
              uiOutput("vy"),
                plotOutput("wybranapartia")),
      tabItem(tabName = "electpoll", h1("Wyniki wyborow"), tableOutput("epr")),
      tabItem(tabName = "electpoll1",h1("Wybierz sam jakiego osrodka sondaz chcesz zobaczyc"),
              uiOutput("wyborosrodka"),uiOutput("wyborzleceniodawcy"),tableOutput("tabelaosrodek")),
      tabItem(tabName = "electpoll2", plotOutput("w1")),
      tabItem(tabName = "partieschart",plotOutput("faceitchart")),
      tabItem(tabName = "top50", plotOutput("top50bar")),
      tabItem(tabName = "freqbar",
              sliderInput("freq","Minimum Frequency:", min = 1,  max = 50, value = 5),
              sliderInput("max", "Maximum Number of Words:", min = 1,  max = 100,  value = 10),
                plotOutput("plot")),
      tabItem(tabName = "tabbar", 
              numericInput("freq1",value = 1,step = 1,label = "Input your numer"),
                tableOutput("wordtable")),
      tabItem(tabName = "assocs", label = "Assotiations between words", 
              textInput("textassocs", label = "Select your word to associate"),
              numericInput("corlimit1", label = "Select your corr limit", value = 0.01, step = 0.01, max = 1),
                verbatimTextOutput("tabassocs")),
      tabItem(tabName = "tweetsent")
    )
  ))



server <- function(input, output){
  
  appname <- "Sentiment analysis matkie"
  key <-"l4RR2R5dcnARo8XsQTuXcLeCw"
  secret <- "MHRBJwjOlG2BMCFyM8FHkuUKOSn5otSFVIAhn7dOtwAfmLzfvI"
  twitter_token <- create_token(
    app = appname,
    consumer_key = key,
    consumer_secret = secret)
  
  
  output$vy <- renderUI({ # pierwszy element wybierany z listy
    selectInput(inputId = "Partie", "Prosze wybrac partie",choices = c("PiS","PO","K15","PARTIA_RAZEM"),selected = "PiS")
     })
  
  output$wybranapartia <- renderPlot({
    ggplot(data = sondaz1, mapping=aes(x = Publikacja, y = get(input$Partie)/100)) +
      ylab(as.character(input$Partie))+
      xlab("Termin publikacji")+
      geom_point(mapping=aes(color = osrodek))+
      geom_smooth()+
      scale_y_continuous(labels = percent_format()) +
      theme(axis.text.x=element_text(angle = 45,hjust = 1))
      })
  
  output$epr <- renderTable({  #drugi element wybierany z listy
    sondaz1
      }) 
  
  output$wyborosrodka <-renderUI({
    checkboxGroupInput(inputId = "osrodekx","Prosze wybrac osrodek",inline = TRUE,
                       choiceValues = unique(sondaz1$osrodek),
                       choiceNames = unique(sondaz1$osrodek))
      })
                       
    output$wyborzleceniodawcy <-renderUI({                     
    checkboxGroupInput(inputId = "zleceniodawcax", "Proszę wybrać zleceniodawcę", inline = TRUE,
                       choiceValues = unique(sondaz1$Zleceniodawca),
                       choiceNames = unique(sondaz1$Zleceniodawca))
      })

  
  output$tabelaosrodek <- renderTable({
    sondaz1[sondaz1$osrodek %in% input$osrodekx | sondaz1$Zleceniodawca %in% input$zleceniodawcax,]
    })
  
  output$w1 <- renderPlot({
  sondaz1 %>%
    count(osrodek,metoda_badania)%>%
      ggplot()+
        geom_tile(aes(osrodek, metoda_badania,fill = n)) +
        theme(axis.text.x=element_text(angle = 90,hjust = 1))
    })
  
  output$faceitchart <- renderPlot({
    przefilter <- filter(sondaz1, sondaz1$metoda_badania == 'CATI')
    head(przefilter)
    prze_trans <- gather(przefilter, "PiS":"N/Z", key = partia, value = poparcie, na.rm = FALSE)
    head(prze_trans)
    ggplot(data = prze_trans, aes(x = Publikacja, y = poparcie)) +
      geom_point(mapping = aes(x = Publikacja, y = poparcie, color = osrodek)) +
      geom_smooth() +
      facet_wrap(~partia, scales = "free_y")
    })


  output$top50bar <- renderPlot({
    wordfunction()
    ggplot(data = head(d,50))+
      geom_bar(mapping = aes(x = reorder(word,+freq),y = freq),stat = "identity")+
      coord_flip()+
      xlab("Words")+
      ylab("Frequency")
      })
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    wordfunction()
    wordcloud(words = d$word, freq = d$freq, scale=c(4,0.5),min.freq = input$freq, max.words=input$max, 
              random.order = TRUE,rot.per = 0.1,colors = brewer.pal(8,"Dark2"))
    
     })
  output$wordtable <-renderTable({
    wordfunction()
    filter(d, freq >= input$freq1)
     })
  
  output$tabassocs <-renderPrint({
    wordfunction()
    dtm <- TermDocumentMatrix(docs)
    findAssocs(dtm, terms = input$textassocs, corlimit = input$corlimit1)
     })
}



shinyApp(ui , server)
