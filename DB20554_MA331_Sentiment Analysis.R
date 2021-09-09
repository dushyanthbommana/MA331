#install.packages('tm')
#install.packages("stringr")
#install.packages("wordcloud")
#install.packages("wordcloud2")
#install.packages("syuzhet")

library(stringr)
library(tm)
library(tidytext)
library(dplyr)
library(wordcloud)
library(wordcloud2)
library(ggplot2)
library(syuzhet)

################ Sentiment Analysis on Black Beauty Child book #################

#read the csv file from the path.
childbook_data <- read.csv("271_Black Beauty.csv", na.strings = c("","NA"), stringsAsFactors = F)

#to check the internal structure of an object.
str(childbook_data)

# omiting the empty and NA data.
childbook_data <- childbook_data %>% na.omit()
dim(childbook_data)

#corpus is a collection of documents containing natural language text. 
corpus_childbook <- iconv(childbook_data$text)

#To merge the complete text from different rows into a single vector.
corpus_childbook <- Corpus(VectorSource(corpus_childbook))

#The inspect() function opens an interactive window with a range of arguments that can be modified. It provides a number of graphic views for analyzing the series.
inspect(corpus_childbook[1:10])

#cleaning the text 
#converting total text in to lower case.
corpus_childbook_lowcase <- tm_map(corpus_childbook, tolower)
#inspect(corpus_childbook_lowcase)

# Removing Punctuation like() 
corpus_childbook_rmpunct <- tm_map(corpus_childbook_lowcase, removePunctuation)
#inspect(corpus_childbook_rmpunct)

# Removing number between the text
corpus_childbook_rmnum <- tm_map(corpus_childbook_rmpunct, removeNumbers)
#inspect(corpus_childbook_rmnum)

#Removing stop words in english like(i, me,my,myself,we...)
corpus_childbook_rmstpwrds <- tm_map(corpus_childbook_rmnum, removeWords, stopwords('english'))
#inspect(corpus_childbook_rmstpwrds)

#Removing white spaces between the text
corpus_childbook_rmvwhtspaces <- tm_map(corpus_childbook_rmstpwrds, stripWhitespace)
#inspect(corpus_childbook_rmvwhtspaces)

#using term document matrix we can represent the text or words in a matrix form
corpus_childbook_tdm <- TermDocumentMatrix(corpus_childbook_rmvwhtspaces)
# printing the values
#corpus_childbook_tdm 

corpus_childbook_tdm_matrix <- as.matrix(corpus_childbook_tdm)
corpus_childbook_tdm_matrix[1:10, 1:20]

#bar plot for most frequent words
child_words_plot <- rowSums(corpus_childbook_tdm_matrix)
#from the total words we are using subset function to get the words which are repeated more that 250 times
child_frequent_words <- subset(child_words_plot, child_words_plot >= 50)
child_frequent_words

# representing the frequently used words in bar plot
barplot(child_frequent_words, las = 2, col = rainbow(50))

# Removing Commonly used insignificant words which do not add any sentiment or emotion to text
corpus_childbook_rmvwrds <- tm_map(corpus_childbook_rmvwhtspaces, removeWords, c('made','side','said','will','shall','now','thing','sir','three'))
#inspect(corpus_childbook_rmvwrds)

#in barplot we observed that there are two bars with horse and horses, So replacing horses with horse
corpus_childbook_fnl <- tm_map(corpus_childbook_rmvwrds, gsub, 
                    pattern='horses', replacement ='horse')
#inspect(corpus_childbook_fnl)

#using term document matrix we can represent the text or words in a matrix form
corpus_childbook_tdm_fnl <- TermDocumentMatrix(corpus_childbook_fnl)
#corpus_childbook_tdm_fnl

corpus_childbook_tdm_matrix_fnl <- as.matrix(corpus_childbook_tdm_fnl)
corpus_childbook_tdm_matrix_fnl[1:10, 1:20]

#bar plot for most frequent words
child_words_plot_fnl <- rowSums(corpus_childbook_tdm_matrix_fnl)
#from the total words we are using subset function to get the words which are repeated more that 250 times
child_frequent_words_fnl <- subset(child_words_plot_fnl, child_words_plot_fnl >= 50)
child_frequent_words_fnl

# representing the frequently used words in bar plot
barplot(child_frequent_words_fnl, las = 2, col = rainbow(50))

# Bar plot to represent top 10 used words in book.
a <- sort(rowSums(corpus_childbook_tdm_matrix_fnl), decreasing = T)
a1 <- data.frame(word=names(a), freq=a)
barplot(a1[1:10,]$freq,names.arg = a1[1:10,]$word, col="orange", main = "Top 10 frequent words used in Black Beauty book")

set.seed(222) # for reproducibility.

child_wrd_cloud <- sort(rowSums(corpus_childbook_tdm_matrix_fnl), decreasing = T)
#word cloud helps in discovering associations and patterns in the text.
wordcloud(words = names(child_wrd_cloud),
          freq = child_frequent_words_fnl,
           max.words = 300,
           random.order = F,
           min.freq = 1,
           colors = brewer.pal(8, 'Dark2'),
           scale = c(5, 0.1), rot.per = 0.5)

# representational using word cloud2
#wordcloud is a text mining method to find the most frequently used words in a text.
child_wrd_cloud2 <- data.frame(names(child_frequent_words_fnl), child_frequent_words_fnl)
colnames(child_wrd_cloud2) <- c('word', 'freq')
wordcloud2(child_wrd_cloud2, size = 0.5, gridSize =  0, 
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# Now performing sentiment analysis
childbook_Emotions <- iconv(childbook_data$text)

#To understand the emotions and sentiments of entire data, get_nrc_sentiments function is used
childbook_sentiment_score <- get_nrc_sentiment(childbook_Emotions)
head(childbook_sentiment_score)
barplot(colSums(childbook_sentiment_score),las=2,col=rainbow(10)
        ,main = "Sentiment classification of Black Beauty(Child  book)")

################ Sentiment Analysis on Emma Adult book #################

#read the csv file from the path
adltbook_data <- read.csv("158_Emma.csv", na.strings = c("","NA"), stringsAsFactors = F)

#to check the internal structure of an object.
str(adltbook_data)

# omiting the empty and NA data
adltbook_data <- adltbook_data %>% na.omit()
dim(adltbook_data)
#corpus is a collection of documents containing natural language text. 
corpus_adltbook <- iconv(adltbook_data$text)

#To merge the complete text from different rows into a single vector.
corpus_adltbook <- Corpus(VectorSource(corpus_adltbook))

#The inspect() function opens an interactive window with a range of arguments that can be modified. It provides a number of graphic views for analyzing the series.
#inspect(corpus_adltbook)

#cleaning the text
#converting total text in to lower case.
corpus_adltbook_lowcase <- tm_map(corpus_adltbook, tolower)
#inspect(corpus_adltbook_lowcase)

# Removing Punctuation like() 
corpus_adltbook_rmpunct <- tm_map(corpus_adltbook_lowcase, removePunctuation)
#inspect(corpus_adltbook_rmpunct)

# Removing number between the text
corpus_adltbook_rmnum <- tm_map(corpus_adltbook_rmpunct, removeNumbers)
#inspect(corpus_adltbook_rmnum)

#Removing stop words in english like(i, me,my,myself,we...)
corpus_adltbook_rmstpwrds <- tm_map(corpus_adltbook_rmnum, removeWords, stopwords('english'))
#inspect(corpus_adltbook_rmstpwrds)

#Removing white spaces between the text
corpus_adltbook_rmvwhtspaces <- tm_map(corpus_adltbook_rmstpwrds, stripWhitespace)
#inspect(corpus_adltbook_rmvwhtspaces)

#using term document matrix we can represent the text or words in a matrix form
corpus_adltbook_tdm <- TermDocumentMatrix(corpus_adltbook_rmvwhtspaces)
#corpus_adltbook_tdm 

corpus_adltbook_tdm_matrix <- as.matrix(corpus_adltbook_tdm)
corpus_adltbook_tdm_matrix[1:10, 1:20]

#bar plot for most frequent words
words_plot <- rowSums(corpus_adltbook_tdm_matrix)
#from the total words we are using subset function to get the words which are repeated more that 250 times
frequent_words <- subset(words_plot, words_plot >= 100)
frequent_words

# representing the frequently used words in bar plot
barplot(frequent_words, las = 2, col = rainbow(50))

# Removing Commonly used insignificant words which do not add any sentiment or emotion to text
corpus_adltbook_rmvwrds <- tm_map(corpus_adltbook_rmvwhtspaces, removeWords, c('now','mrs','thing','said','say','will','can'))
#inspect(corpus_adltbook_rmvwrds)


#using term document matrix we can represent the text or words in a matrix form
corpus_adltbook_tdm_fnl <- TermDocumentMatrix(corpus_adltbook_rmvwrds)
#corpus_adltbook_tdm_fnl # printing the values

corpus_adltbook_tdm_matrix_fnl <- as.matrix(corpus_adltbook_tdm_fnl)
corpus_adltbook_tdm_matrix_fnl[1:10, 1:20]

#bar plot for most frequent words
adlt_words_plot_fnl <- rowSums(corpus_adltbook_tdm_matrix_fnl)
#from the total words we are using subset function to get the words which are repeated more that 50 times
adlt_frequent_words_fnl <- subset(adlt_words_plot_fnl, adlt_words_plot_fnl >= 50)
adlt_frequent_words_fnl

# representing the frequently used words in bar plot
barplot(adlt_frequent_words_fnl, las = 2, col = rainbow(50))

# Bar plot to represent top 10 used words in book.
d <- sort(rowSums(corpus_adltbook_tdm_matrix_fnl), decreasing = T)
d1 <- data.frame(word=names(d), freq=d)
barplot(d1[1:10,]$freq,names.arg = d1[1:10,]$word, col="#00ffff", 
        main = "Top 10 frequent words used in Emma Adult book")

#Representational using word cloud
adlt_wrd_cloud <- sort(rowSums(corpus_adltbook_tdm_matrix_fnl), decreasing = T)
wordcloud(words = names(adlt_wrd_cloud),
          freq = adlt_frequent_words_fnl,
          max.words = 1500,
          random.order = F,
          min.freq = 1,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(3, 0.3), rot.per = 0.7)

# representational using word cloud2
#wordcloud is a text mining method to find the most frequently used words in a text.
adlt_wrd_cloud2 <- data.frame(names(adlt_frequent_words_fnl), adlt_frequent_words_fnl)
colnames(adlt_wrd_cloud2) <- c('word', 'freq')
wordcloud2(adlt_wrd_cloud2, size = 0.5, gridSize =  0, 
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# To understand the emotions and sentiments of entire data, get_nrc_sentiments function is used
adultbook_Emotions <- iconv(adltbook_data$text)
adultbook_sentiment_score <- get_nrc_sentiment(adultbook_Emotions)
head(adultbook_sentiment_score)
barplot(colSums(adultbook_sentiment_score),las=2,col=rainbow(10),
        main = "Sentiment classification of Emma(Adult Book)")

