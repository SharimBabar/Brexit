
library(rvest)

#website=read_html('https://www.reddit.com/r/brexit/comments/e93w19/no_one_that_has_been_in_an_ae_lately_can_vote_for/')

#website_text= website%>%
#  html_nodes('p._1qeIAgB0cPwnLhDF9XSiJM')%>%
#  html_text()

#website_text

#brexit_talk= data.frame(website_text)

#write.csv(brexit_talk, 'Brexit_talk.csv')

setwd('C:/Users/Rimna/Desktop/Advanced Crime analysis/R projects/Brexit')

brexit_talk2= read.csv('Brexit_talk.csv', header = T)

#library(sentimentr)
#library(tm)

#creating a corpus 
#a corpus a collection of written texts
corpus= iconv(brexit_talk2$website_text)
corpus=Corpus(VectorSource(corpus))

inspect(corpus[1:5])
# cleaning the text data 
# Changing everything to lower case 
corpus=tm_map(corpus, tolower)

#removing punctuation 
corpus=tm_map(corpus, removePunctuation)

#removing numbers 
corpus=tm_map(corpus, removeNumbers)

#removing common words (stopwords) that do not add value
final_set=tm_map(corpus, removeWords, stopwords('english'))

View(final_set)

##########Converting a structure data################
#term document matrix 
brexit_tdm= TermDocumentMatrix(final_set)
# sparsity 87% 

brexit_tdm=as.matrix(brexit_tdm)
brexit_tdm[1:10,1:10]

########### Visualization:word cloud####################

#library(wordcloud)
brexit_sum_words=sort(rowSums(brexit_tdm),
                      decreasing =T )
set.seed(1995)

wordcloud(words = names(brexit_sum_words),
          freq = brexit_sum_words,
          max.words = 20,
          random.order = F,
          colors=brewer.pal(10,'Dark2'),
          scale=c(2, 0.5))
###################Sentiment Analysis##########
library(syuzhet)

brexit_text= iconv(brexit_talk2$website_text)

sentiment_brexit=get_nrc_sentiment(brexit_text)

#barplot of sentiment
barplot(colSums(sentiment_brexit),
        las=2,
        col=rainbow(10),
        ylab='Count',
        main='Brexit reddit sentiment')




