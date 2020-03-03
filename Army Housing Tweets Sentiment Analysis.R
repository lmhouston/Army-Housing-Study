
#Examples of Background Data Work / Skip to START HERE

#Get Current Rate LImit
getCurRateLimitInfo()

# Authenticate Twitter
consumer_key <- "*********************"
consumer_secret <- "************************************"
access_token <- "********************************************"
access_secret <- "**********************************"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#GET TOKEN
curl -u '******************************' \
--data 'grant_type=client_credentials' \
'https://api.twitter.com/oauth2/token'

#Token=
************************************************************************************
  
  #API Call
  curl -o 30daycorvias.txt --request POST --url https://api.twitter.com/1.1/tweets/search/30day/Research30.json --header 'authorization: Bearer *******************************************************' --header 'content-type: application/json' --data '{
"query":"Corvias OR #Corvias",
"maxResults":"500",
"fromDate":"201902110000", 
"toDate":"201903110000"
}'

curl -o 30daycorvias3.txt --request POST --url https://api.twitter.com/1.1/tweets/search/30day/Research30.json --header 'authorization: Bearer ****************************************' --header 'content-type: application/json' --data '{
"query":"Corvias OR #Corvias",
"maxResults":"500",
"fromDate":"201902110000", 
"toDate":"201903110000",
"next":"*********************************"
}'


#Collate Data
jsonData3$results$text[1:10]
nrow(jsonData3$results)
results <- rbind_pages (
  list(jsonData1$results, jsonData2$results, jsonData3$results)
)
nrow(results)

#Input file / View structure
data <- fromJSON(file = "MilHouseNatl1copy.json")
data
str(data)

################ S T A R T    H E R E ################ S T A R T     H E R E #############

# Load Packages
library(tm)
library(ggplot2)
library(wordcloud)
library(twitteR)
library(rjson)
library(jsonlite)
library(gdata)
library(tidyr)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(purrr)
library(repurrrsive)
library(listviewer)
library(tidytext)
library(dplyr)
library(stringr)
library(NLP)

#Import Files 
HouseTweetsNatl <- read.csv("~/HouseTweetsNatl.csv", comment.char="#")
HouseTweetsLocal <- read.csv("~/HouseTweetsLocal.csv", comment.char="#")

#Select Only Text Column & Build Corpus
natlCorpus <- Corpus(VectorSource(HouseTweetsNatl$text))
localCorpus <- Corpus(VectorSource(HouseTweetsLocal$text))

# Clean text
natlCorpus <- tm_map(natlCorpus, FUN = content_transformer(tolower)
                     natlCorpus <- tm_map(natlCorpus, removePunctuation)
                     natlCorpus <- tm_map(natlCorpus, removeNumbers)
                     removeURL <- function(x) gsub("http[[:alnum:]]*", '', x)
                     natlCorpus <- tm_map(natlCorpus, content_transformer(removeURL))
                     natlCorpus <- tm_map(natlCorpus, removeWords, c("corvias", "military", "housing", "a", "about", "above", "across", "after", "again", "against", "all", "almost", "along", "also", "although", "among", "an", "and", "another", "any", "anybody", "anyone", "anything", "anywhere", "are", "around", "as", "at", "be", "because", "been", "being", "between", "but", "by", "came", "can", "cannot", "come", "could", "did", "do", "does", "during", "each", "even", "few", "for", "from", "get", "gets", "got", "had", "has", "have", "having", "here", "how", "i", "if", "in", "into", "is", "it", "its", "it’s", "itself", "just", "let", "lets", "like", "likely", "many", "me", "much", "my", "myself", "never", "non", "not", "of", "often", "on", "only", "or", "our", "over", "per", "perhaps", "put", "puts", "quite", "rather", "really", "so", "still", "such", "than", "that", "the", "th", "their", "them", "then", "there", "therefore", "these", "they", "this", "those", "though", "through", "thus", "to", "too", "took", "toward", "under", "until", "upon", "very", "was", "we", "went", "were", "what", "which", "while", "who", "whose", "with", "within", "without", "would", "yet","you", "your", "yours"))
                     natlClean <- tm_map(natlCorpus, stripWhitespace)
                     
                     localCorpus <- tm_map(localCorpus, content_transformer(tolower))
                     localCorpus <- tm_map(localCorpus, removePunctuation)
                     localCorpus <- tm_map(localCorpus, removeNumbers)
                     removeURL <- function(x) gsub("http[[:alnum:]]*", '', x)
                     localCorpus <- tm_map(localCorpus, content_transformer(removeURL))
                     localCorpus <- tm_map(localCorpus, removeWords, c("corvias", "military", "housing", "a", "about", "above", "across", "after", "again", "against", "all", "almost", "along", "also", "although", "among", "an", "and", "another", "any", "anybody", "anyone", "anything", "anywhere", "are", "around", "as", "at", "be", "because", "been", "being", "between", "but", "by", "came", "can", "cannot", "come", "could", "did", "do", "does", "during", "each", "even", "few", "for", "from", "get", "gets", "got", "had", "has", "have", "having", "here", "how", "i", "if", "in", "into", "is", "it", "its", "it’s", "itself", "just", "let", "lets", "like", "likely", "many", "me", "much", "my", "myself", "never", "non", "not", "of", "often", "on", "only", "or", "our", "over", "per", "perhaps", "put", "puts", "quite", "rather", "really", "so", "still", "such", "than", "that", "the", "th", "their", "them", "then", "there", "therefore", "these", "they", "this", "those", "though", "through", "thus", "to", "too", "took", "toward", "under", "until", "upon", "very", "was", "we", "went", "were", "what", "which", "while", "who", "whose", "with", "within", "without", "would", "yet","you", "your", "yours"))
                     localClean <- tm_map(localCorpus, stripWhitespace)
                     
                     # Term Document Matrix
                     natlTweetsTDM <-TermDocumentMatrix(natlClean)
                     natlTweetsTDM <-as.matrix(natlTweetsTDM)
                     
                     localTweetsTDM <-TermDocumentMatrix(localClean)
                     localTweetsTDM <-as.matrix(localTweetsTDM)
                     
                     # Bar Plot (using subset of words that appear 35 or more times) and margins for longer bottom words (par mar)
                     w <- rowSums(natlTweetsTDM)
                     w
                     w <- subset(w, w>=35)
                     w
                     par(mar=c(8.5,3,0.5, 0.3))
                     barplot (w, 
                              las = 2,
                              col=rgb(0.2,0.4,0.6,0.6), border="black")
                     
                     w <- rowSums(localTweetsTDM)
                     w
                     w <- subset(w, w>=35)
                     w
                     par(mar=c(8.5,3,0.5, 0.3))
                     barplot (w, 
                              las = 2,
                              col=rgb(0.2,0.4,0.6,0.6), border="black")
                     
                     # Wordcloud
                     library(wordcloud)
                     
                     n <- sort(rowSums(natlTweetsTDM), decreasing = TRUE)
                     set.seed(222)
                     wordcloud(words = names(n),
                               freq = w)
                     
                     l <- sort(rowSums(localTweetsTDM), decreasing = TRUE)
                     set.seed(222)
                     wordcloud(words = names(l),
                               freq = w)
                     
                     # Wordcloud Set Word Max, Random Order, Minimum Freq, Color, and Scale Size of Words
                     wordcloud(words = names(n),
                               freq = w,
                               max.words =60,
                               random.order = F, 
                               min.freq = 5,
                               colors = brewer.pal(8, 'Dark2'),
                               scale = c(6, 0.3))
                     
                     wordcloud(words = names(l),
                               freq = w,
                               max.words =60,
                               random.order = F, 
                               min.freq = 5,
                               colors = brewer.pal(8, 'Dark2'),
                               scale = c(6, 0.3))
                     
                     # Sentiment Analysis
                     library(syuzhet)
                     library(lubridate)
                     library(ggplot2)
                     library(scales)
                     library(reshape2)
                     library(dplyr)
                     
                     # Read Files
                     natlTweets <- iconv(HouseTweetsNatl$text, to = 'utf-8-mac')
                     localTweets <- iconv(HouseTweetsLocal$text, to = 'utf-8-mac')
                     
                     # Load Sentiment Scores
                     snatl <- get_nrc_sentiment(natlTweets)
                     slocal <- get_nrc_sentiment(localTweets)
                     
                     # View Head, Select Tweet, Value of Word
                     head(s)
                     Tweets[3]
                     get_nrc_sentiment('keen')
                     
                     # Bar Plot
                     barplot(colSums(snatl),
                             las = 2,
                             col=c("#660000", "#FF9900", "#990000", "#FFFF33", "#99CC66", "#003399", "#666666", "#666633", "#000000", "#CCCCCC"),
                             ylab = 'Count')
                     
                     barplot(colSums(slocal),
                             las = 2,
                             col=c("#660000", "#FF9900", "#990000", "#FFFF33", "#99CC66", "#003399", "#666666", "#666633", "#000000", "#CCCCCC"),
                             ylab = 'Count')
                     
                     # Machine Learning Sentiment Analysis
                     library(twitteR)
                     library(ROAuth)
                     library(tidyverse)
                     library(text2vec)
                     library(caret)
                     library(glmnet)
                     library(ggrepel)
                     library(text2vec)
                     
                     ### loading and preprocessing a training set of tweets

                     
                     ##### load tweets ######
                     tweets_classified <- read_csv('HouseTweetsAll.csv',
                                                   col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text'))
                     write.csv(tweets_classified, file = "HouseTweetsAll.csv")
                     # function for converting some symbols
                     Tweets_conv <- iconv(tweets_classified, "latin1", "ASCII", "")
                     
                     # data splitting on train and test
                     set.seed(2340)
                     trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8, 
                                                       list = FALSE, 
                                                       times = 1)
                     tweets_train <- tweets_classified[trainIndex, ]
                     tweets_test <- tweets_classified[-trainIndex, ]
                     
                     ##### doc2vec #####
                     # define preprocessing function and tokenization function
                     prep_fun <- tolower
                     tok_fun <- word_tokenizer
                     
                     it_train <- itoken(tweets_train$text, 
                                        preprocessor = prep_fun, 
                                        tokenizer = tok_fun,
                                        ids = tweets_train$id,
                                        progressbar = TRUE)
                     it_test <- itoken(tweets_test$text, 
                                       preprocessor = prep_fun, 
                                       tokenizer = tok_fun,
                                       ids = tweets_test$id,
                                       progressbar = TRUE)
                     
                     # creating vocabulary and document-term matrix
                     vocab <- create_vocabulary(it_train)
                     vectorizer <- vocab_vectorizer(vocab)
                     dtm_train <- create_dtm(it_train, vectorizer)
                     dtm_test <- create_dtm(it_test, vectorizer)
                     
                     # define tf-idf model
                     tfidf <- TfIdf$new()
                     
                     # fit the model to the train data and transform it with the fitted model
                     dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
                     dtm_test_tfidf <- fit_transform(dtm_test, tfidf)
                     
                     # train the model
                     t1 <- Sys.time()
                     glmnet_classifier <- cv.glmnet(x = dtm_train_tfidf, y = tweets_train[['sentiment']], 
                                                    family = 'multinomial', 
                                                    # L1 penalty
                                                    alpha = 1,
                                                    # interested in the area under ROC curve
                                                    type.measure = "auc",
                                                    # 5-fold cross-validation
                                                    nfolds = 5,
                                                    # high value is less accurate, but has faster training
                                                    thresh = 1e-3,
                                                    # again lower number of iterations for faster training
                                                    maxit = 1e3)
                     print(difftime(Sys.time(), t1, units = 'mins'))
              
                     plot(glmnet_classifier)
                     print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))