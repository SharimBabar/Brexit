library(ggplot2)
library(e1071)
library(caret)
library(quanteda)
library(irlba)
library(randomForest)

######################Stage 1: Loading and formatting###########
#Loading the text data, which is in csv file
brexit.raw= read.csv('Brexit_talk.csv',header = T)
View(brexit.raw)

#Remove the index column
brexit.raw=brexit.raw[,2:3]
View(brexit.raw)

# checking for missing data
length(which(!complete.cases(brexit.raw)))

#coverting the values in the label column into factors
brexit.raw$Label=as.factor(brexit.raw$Label)

#checkning the distribution of for and against
prop.table(table(brexit.raw$Label))

#Checking the length of the comments
brexit.raw$website_text=as.character(brexit.raw$website_text)
brexit.raw$TextLength=nchar(brexit.raw$website_text)
summary(brexit.raw$TextLength)

############### Partion #########################

# splitting the data into stratifiald split of 70/30 to avoid generalization
# 70% of the data as training set
# 30% as test set
# Seed for reproduction of the test 
set.seed(1995)
indexes=createDataPartition(brexit.raw$Label, times=1,
                            p=0.7, list = F)
training_set=brexit.raw[indexes,]
testing_set=brexit.raw[-indexes,]

# Checking whether the partition is correct
prop.table(table(training_set$Label))
prop.table(table(testing_set$Label))

############ Processing the text data
# tokenize the text and remove unwanted tokens
train.tokens=tokens(training_set$website_text,
                    what='word',
                    remove_numbers=T,
                    remove_punct=T,
                    remove_symbols=T,
                    remove_hyphens=T)

#Changing to lower case
train.tokens=tokens_tolower(train.tokens)

# Removing stop words
train.tokens=tokens_select(train.tokens, stopwords(),
                           selection = 'remove')
#Stemming words 
train.tokens=tokens_wordstem(train.tokens,language = 'english')

#Creating the bag-of-words model
train.tokens.dfm=dfm(train.tokens, tolower = F,remove=stopwords())

#transforming the dfm into martix 
train.tokens.martix=as.matrix(train.tokens.dfm)
dim(train.tokens.martix)
train.token.df=cbind(Label=training_set$Label, data.frame(train.tokens.dfm))

#Converting all column name syntactically valid names
names(train.token.df)=make.names(names(train.token.df))

# for we will convert the dfm into tf-idf
train.token.tfidf=dfm_tfidf(train.tokens.dfm,
                            scheme_tf ='count',
                            scheme_df = 'inverse',
                            base=10)

train.token.tfidf.martix=as.matrix(train.token.tfidf)
dim(train.token.tfidf.martix)
View(train.token.tfidf.martix)
train.token.tfidf.df=cbind(Label=training_set$Label,
                           data.frame(train.token.tfidf.martix))
dim(train.token.tfidf.df)
grep('Label', colnames(train.token.tfidf.df))
View(train.token.tfidf.df[1])

############# Cross validation ############

# create 10 fold stratifield sample, K=10
# repeat 3 times, times=3, for more robust results
# setting the up the data
set.seed(1995)
cv.folds=createMultiFolds(training_set$Label,
                             k=10,
                             times=3)

# building the method for cross validation
cv.control=trainControl(method='repeatedcv',
                        number=10,
                        repeats=3,
                        index=cv.folds)

# Training the model
# predict label given the rest of the data 
# tuneLength decide the best config to use
label=train.token.tfidf.df$Label

rpart.cv1=train(Label~.,
                data=train.token.tfidf.df,
                method="glm",
                trControl=cv.control,
                tunelength=5)
