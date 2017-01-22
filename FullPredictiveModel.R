library(quanteda)
library(tm)
library(ggplot2)
library(knitr)
library(reshape2)
library(stringr)

load("filelengths.RData")

##TO DO: Remove ascii
##OR - justify it as when you remove the ngrams with low frequency that these are removed.
##TO DO: Add pentgrams

##Load subsetdata
subdata <- VCorpus(DirSource("./final/en_US/Subset"),readerControl = list(reader = readPlain, language = "en",load = TRUE))

##Clean the data
subdata <- tm_map(subdata, removePunctuation)
subdata <- tm_map(subdata, removeNumbers) 
subdata <- tm_map(subdata, stripWhitespace)
subdata <- tm_map(subdata, content_transformer(tolower))
profanity <- read.table(url("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"))
subdata <- tm_map(subdata, removeWords, as.character(profanity[,1]))
dtm.subdata <- DocumentTermMatrix(subdata)
##remove all words with numbers

##remove all non-english characters

##check lengths
linecount <- c(lengthblogs,lengthnews,lengthtwit)
termcount <- rowSums(as.matrix(dtm.subdata))
kable(data.frame(linecount=linecount,termcount=termcount))

##transform into dataframe
subdata.dtm.matrix = as.matrix(t(dtm.subdata))
subdata.dtm.df  = as.data.frame(subdata.dtm.matrix)
##put word as a column
subdata.dtm.df$word <- rownames(subdata.dtm.df)
##add totals
subdata.dtm.df$total <- subdata.dtm.df$subsetblogs.txt +
  subdata.dtm.df$subsetnews.txt +
  subdata.dtm.df$subsettwitter.txt
##order by descending total
subdata.dtm.df$word <- factor(subdata.dtm.df$word, levels = subdata.dtm.df$word[order(subdata.dtm.df$total)])
subdata.dtm.df.ord <- subdata.dtm.df[order(-subdata.dtm.df$total),]

##create subgrams of subdata1
tkn <- tokenize(as.character(subdata[[1]]), 
                removePunct = TRUE, simplify = TRUE)

##create bigram over 14 chunks of subdata[1]
subbigramraw <- character()
for (i in (1:13)*(length(tkn)/14))
{j <- i + length(tkn)/14
subbigramraw <- c(subbigramraw,ngrams(tkn[i:j],2))}

##create trigram over 14 chunks of subdata[1]
subtrigramraw <- character()
for (i in (1:13)*(length(tkn)/14))
{j <- i + length(tkn)/14
subtrigramraw <- c(subtrigramraw,ngrams(tkn[i:j],3))}

subquadgramraw <- character()
##create quadgram over 14 chunks of subdata[1]
for (i in (1:13)*(length(tkn)/14))
{j <- i + length(tkn)/14
subquadgramraw <- c(subquadgramraw,ngrams(tkn[i:j],4))}

##create subgrams of subdata2
##
##
tkn2 <- tokenize(as.character(subdata[[2]]), 
                 removePunct = TRUE, simplify = TRUE)

##create bigram over 14 chunks of subdata[2]
subbigramraw2 <- character()
for (i in (1:13)*(length(tkn2)/14))
{j <- i + length(tkn2)/14
subbigramraw2 <- c(subbigramraw2,ngrams(tkn[i:j],2))}

##create trigram over 14 chunks of subdata[2]
subtrigramraw2 <- character()
for (i in (1:13)*(length(tkn2)/14))
{j <- i + length(tkn2)/14
subtrigramraw2 <- c(subtrigramraw2,ngrams(tkn[i:j],3))}

subquadgramraw2 <- character()
##create quadgram over 14 chunks of subdata[2]
for (i in (1:13)*(length(tkn2)/14))
{j <- i + length(tkn2)/14
subquadgramraw2 <- c(subquadgramraw2,ngrams(tkn[i:j],4))}




##create subgrams of subdata3
##
##

tkn3 <- tokenize(as.character(subdata[[3]]), 
                 removePunct = TRUE, simplify = TRUE)

##create bigram over 14 chunks of subdata[2]
subbigramraw3 <- character()
for (i in (1:13)*(length(tkn3)/14))
{j <- i + length(tkn3)/14
subbigramraw3 <- c(subbigramraw3,ngrams(tkn[i:j],2))}

##create trigram over 14 chunks of subdata[2]
subtrigramraw3 <- character()
for (i in (1:13)*(length(tkn3)/14))
{j <- i + length(tkn3)/14
subtrigramraw3 <- c(subtrigramraw3,ngrams(tkn[i:j],3))}

subquadgramraw3 <- character()
##create quadgram over 14 chunks of subdata[2]
for (i in (1:13)*(length(tkn3)/14))
{j <- i + length(tkn3)/14
subquadgramraw3 <- c(subquadgramraw3,ngrams(tkn[i:j],4))}


##create total unigrams
totalunigramdf <- data.frame(totalunigramraw=subdata.dtm.df.ord$word, Freq=subdata.dtm.df.ord$total)

##create total bigrams
totalbigramraw <- c(sapply(subbigramraw,function(x) paste(unlist(x),collapse=" ")),
          sapply(subbigramraw2,function(x) paste(unlist(x),collapse=" ")),
          sapply(subbigramraw3,function(x) paste(unlist(x),collapse=" ")))

totalbigramdf <- as.data.frame(table(totalbigramraw))
totalbigramdf$firstwords <- word(totalbigramdf$totalbigramraw,1)
totalbigramdf$lastword <- word(totalbigramdf$totalbigramraw,-1)
totalbigramdf <- totalbigramdf[order(-totalbigramdf$Freq),]
##removing the top NA row (to be improved)
totalbigramdf <- totalbigramdf[2:nrow(totalbigramdf),]

##create total trigrams
totaltrigramraw <- c(sapply(subtrigramraw,function(x) paste(unlist(x),collapse=" ")),
                    sapply(subtrigramraw2,function(x) paste(unlist(x),collapse=" ")),
                    sapply(subtrigramraw3,function(x) paste(unlist(x),collapse=" ")))

totaltrigramdf <- as.data.frame(table(totaltrigramraw))
totaltrigramdf$firstwords <- word(totaltrigramdf$totaltrigramraw,1,2)
totaltrigramdf$lastword <- word(totaltrigramdf$totaltrigramraw,-1)
totaltrigramdf <- totaltrigramdf[order(-totaltrigramdf$Freq),]
##removing the top NA row (to be improved)
totaltrigramdf <- totaltrigramdf[2:nrow(totaltrigramdf),]


##create total quadgrams
totalquadgramraw <- c(sapply(subquadgramraw,function(x) paste(unlist(x),collapse=" ")),
                      sapply(subquadgramraw2,function(x) paste(unlist(x),collapse=" ")),
                      sapply(subquadgramraw3,function(x) paste(unlist(x),collapse=" ")))

totalquadgramdf <- as.data.frame(table(totalquadgramraw))
totalquadgramdf$firstwords <- word(totalquadgramdf$totalquadgramraw,1,3)
totalquadgramdf$lastword <- word(totalquadgramdf$totalquadgramraw,-1)
totalquadgramdf <- totalquadgramdf[order(-totalquadgramdf$Freq),]
##removing the top NA row (to be improved)
totalquadgramdf <- totalquadgramdf[2:nrow(totalquadgramdf),]

##Create predictive function to return top 5 words and their score
inputstring <- "I think I'm going to the"

##create empty vectors for possible answers
topanswers <- vector()
topscores <- vector()
lambda <- 0.4

##find top quadgrams
topquadgrams <- head(totalquadgramdf[totalquadgramdf$firstwords==word(inputstring,-3,-1) , ],5)
##calculatescoresforquadgrams
if(length(topquadgrams)>0) {
  trigramfreq <- totaltrigramdf[totaltrigramdf$totaltrigramraw==word(inputstring,-3,-1) , ]$Freq
  for (i in 1:length(topquadgrams))
    {topanswers <- c(topanswers, topquadgrams[i,]$lastword)
    topscores <- c(topscores,topquadgrams[i,]$Freq/trigramfreq)}}

##find top trigrams
toptrigrams <- head(totaltrigramdf[totaltrigramdf$firstwords==word(inputstring,-2,-1) , ],5)
##remove any duplicates.
##calculatescoresfortrigrams
if(length(toptrigrams)>0) {
  bigramfreq <- totalbigramdf[totalbigramdf$totalbigramraw==word(inputstring,-2,-1) , ]$Freq
  for (i in 1:length(toptrigrams))
  {topanswers <- c(topanswers, toptrigrams[i,]$lastword)
  topscores <- c(topscores,lambda*toptrigrams[i,]$Freq/bigramfreq)}}

##find top bigrams
topbigrams <- head(totalbigramdf[totalbigramdf$firstwords==word(inputstring,-1) , ],5)
##remove any duplicates.
##calculatescoresforbigrams
if(length(topbigrams)>0) {
  unigramfreq <- totalunigramdf[totalunigramdf$totalunigramraw==word(inputstring,-1) , ]$Freq
  for (i in 1:length(topbigrams))
  {topanswers <- c(topanswers, topbigrams[i,]$lastword)
  topscores <- c(topscores,lambda*lambda*topbigrams[i,]$Freq/unigramfreq)}}

##Testing
head(totalquadgramdf[order(-totalquadgramdf$Freq),])
subdata.dtm.df.ord <- subdata.dtm.df[order(-subdata.dtm.df$total),]

