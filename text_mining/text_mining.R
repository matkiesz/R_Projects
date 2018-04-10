install.packages("tm") # framework for text mining in R
install.packages("SnowballC")  # implements word stemming algorithm
install.packages("wordcloud")  # to plot a cloud of words shared across documents
install.packages("RColorBrewer") # provides color schemes for maps
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("tidyverse")
filePath <- "parties_en.txt"
text <- read.delim(filePath) 
text
class(text)

# convert text to vectors, then to Corpus object 
docs <- Corpus(VectorSource(text))
class(docs)
typeof(docs)
docs

inspect(docs) 
docs[[3]] # what are characteristics of 3rd line?
docs[[3]]$content # print 3rd line content
docs[[3]]$meta # print 3rd line metadata

# DATA CLEANING
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("coś", "cośinnego")) 

# STEMMING example
stem_doc <- stemDocument(c("win", "winning", "winner", "window"))
stem_doc
sample_text <- ("wins")
completion_results <- stemCompletion(stem_doc, sample_text)
completion_results

# STEMMING DOC - before removing punctuations we have to stem doc
docs <- tm_map(docs, stemDocument)
inspect(docs)
inspect(docs)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs
inspect(docs)

# WORD FREQUENCIES
dtm <- TermDocumentMatrix(docs2)
dtm
m <- as.matrix(dtm)
ncol(m)
# m # do not open this - because it's matrix with 8406 columns it may damage your computer :)

v <- sort(rowSums(m),decreasing=TRUE) # sum all word frequencies across columns (lines in whole text)
v
class(v)
names(v)
d <- data.frame(word = names(v), freq=v)
head(d, 10)

# plot word frequencies on bar charts
ggplot(data = filter(d, freq > 100), mapping = aes(x = word, y = freq)) +
  geom_bar(stat = "identity")
# reorder words from low to high
ggplot(data = filter(d, freq > 100), mapping = aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity")
# reorder words start from the highest 
ggplot(data = filter(d, freq > 100), mapping = aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity")
# flip coordinates
ggplot(data = filter(d, freq > 100), mapping = aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity") +
  xlab("Word") +
  ylab("Word frequency") +
  coord_flip()
# rotate label
ggplot(data = filter(d, freq > 100), mapping = aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity") +
  xlab("Word") +
  ylab("Word frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# find which words have frequency > X
findFreqTerms(dtm, lowfreq = 200)

# check assosiations between words
findAssocs(dtm, terms = "wine", corlimit = 0.2)
findAssocs(dtm, terms = "fish", corlimit = 0.3)

# SENTIMENT
install.packages("syuzhet")
library('syuzhet')
d$word
df_sentiment<-get_nrc_sentiment(as.String(d$word)) # sentiment from unique words
df_sentiment
class(df_sentiment)

df_sentiment_transposed <- t(df_sentiment) # transpose data frame from columns to rows
df_sentiment_final <- data.frame(sentiment=row.names(df_sentiment_transposed), 
                                 sent_value=df_sentiment_transposed, row.names=NULL) # prepare final data frame with emotions in 1st column, values in 2nd
df_emotions <- df_sentiment_final[1:8,]
df_sentiments <- df_sentiment_final[9:10,]

# plot emotions
ggplot(data = df_emotions, mapping = aes(x = sentiment, y = sent_value, color = sentiment, fill = sent_value)) +
  geom_bar(stat = "identity") +
  xlab("emotion") +
  ylab("words count") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

# WORDCLOUD
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=500, random.order=FALSE, rot.per=0.45, 
          colors=brewer.pal(8, "Dark2"))
