setwd('C:/Users/Rimna/Desktop/Advanced Crime analysis/R projects/Brexit')

library(tm)
csv_bre= read.csv('Brexit_talk.csv', header = T, stringsAsFactors = F)
View(csv_bre)

#creating a corpus 
#a corpus a collection of written texts
kcorpus= iconv(csv_bre$website_text)
View(kcorpus)
kcorpus=Corpus(VectorSource(kcorpus))
View(kcorpus)
# cleaning the text data 
# Changing everything to lower case 
kcorpus=tm_map(kcorpus, tolower)

#removing punctuation 
kcorpus=tm_map(kcorpus, removePunctuation)

#removing numbers 
kcorpus=tm_map(kcorpus, removeNumbers)

#removing common words (stopwords) that do not add value
kfinal_set=tm_map(kcorpus, removeWords, stopwords('english'))

View(kfinal_set)

##########Converting a structure data################
#term document matrix 
kbrexit_tdm= TermDocumentMatrix(kfinal_set)
# sparsity 87% 

kbrexit_tdm=as.matrix(kbrexit_tdm)
kbrexit_tdm[1:10,1:10]
View(kbrexit_tdm)

##############Kmean###############

results=kmeans(kbrexit_tdm, 2)

results$size

plot(results$cluster)
plot(kbrexit_tdm)

# kmean clustering may not be the most appropriate algorithm
