library(quanteda)
library(tm)
library(ggplot2)
library(knitr)
library(reshape2)
library(stringr)

##Load subsetdata
subdata <- VCorpus(DirSource("./final/en_US/Subset"),readerControl = list(reader = readPlain, language = "en",load = TRUE))
subdata <- tm_map(subdata, removePunctuation)
subdata <- tm_map(subdata, stripWhitespace)
subdata <- tm_map(subdata, content_transformer(tolower))

profanity <- read.table(url("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"))
subdata <- tm_map(subdata, removeWords, as.character(profanity[,1]))
dtm.subdata <- DocumentTermMatrix(subdata)

##check lengths
load("filelengths.RData")
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

##create total n-grams
totalquadgramdf
