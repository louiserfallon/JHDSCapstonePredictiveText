
library(stringr)

#loading totalquadgramdf, totaltrigramdf,
#totalbigramdf, totalunigramdf and totalfreq

load("ngramfiles.RData")

stringprediction <- function(inputstring)
{
  ##create empty vectors for possible answers
  topanswers <- vector()
  topscores <- vector()
  lambda <- 0.4
  
  if(str_count(inputstring, "\\S+")>=3){
  ##find top quadgrams
  topquadgrams <- head(totalquadgramdf[totalquadgramdf$firstwords==word(inputstring,-3,-1) , ],5)
  ##calculatescoresforquadgrams
  if(nrow(topquadgrams)>0) {
    trigramfreq <- totaltrigramdf[totaltrigramdf$totaltrigramraw==word(inputstring,-3,-1) , ]$Freq
    for (i in 1:nrow(topquadgrams))
    {topanswers <- c(topanswers, topquadgrams[i,]$lastword)
    topscores <- c(topscores,topquadgrams[i,]$Freq/trigramfreq)}}
  }
  
  if(str_count(inputstring, "\\S+")>=2){
  ##find top trigrams
  toptrigrams <- head(totaltrigramdf[totaltrigramdf$firstwords==word(inputstring,-2,-1) , ],5)
  ##remove any duplicates
  toptrigrams <- toptrigrams[!toptrigrams$lastword %in% topanswers, ]
  
  ##calculatescoresfortrigrams
  if(nrow(toptrigrams)>0) {
    bigramfreq <- totalbigramdf[totalbigramdf$totalbigramraw==word(inputstring,-2,-1) , ]$Freq
    for (i in 1:nrow(toptrigrams))
    {topanswers <- c(topanswers, toptrigrams[i,]$lastword)
    topscores <- c(topscores,lambda*toptrigrams[i,]$Freq/bigramfreq)}}
  }
  
  if(str_count(inputstring, "\\S+")>=1){
  ##find top bigrams
  topbigrams <- head(totalbigramdf[totalbigramdf$firstwords==word(inputstring,-1) , ],5)
  ##remove any duplicates
  topbigrams <- topbigrams[!topbigrams$lastword %in% topanswers, ]
  ##calculatescoresforbigrams
  if(nrow(topbigrams)>0) {
    unigramfreq <- totalunigramdf[totalunigramdf$totalunigramraw==word(inputstring,-1) , ]$Freq
    for (i in 1:nrow(topbigrams))
    {topanswers <- c(topanswers, topbigrams[i,]$lastword)
    topscores <- c(topscores,lambda*lambda*topbigrams[i,]$Freq/unigramfreq)}}
  }
  
  ##find top unigrams (always the same)
  topunigrams <- head(totalunigramdf,20)
  ##remove any duplicates
  topunigrams <- topunigrams[!topunigrams$lastword %in% topanswers, ]
    for (i in 1:5) #only need 5 to pad any remaining options
    {topanswers <- c(topanswers, paste(topunigrams[i,]$lastword,"*"))
    topscores <- c(topscores,lambda*lambda*lambda*topunigrams[i,]$Freq/totalfreq)}
  
  results <- data.frame(topanswers = topanswers, topscores=topscores)
  results <- results[order(results$topscores, decreasing=TRUE),]
  names(results) <- c("Predicted Word","Score")
  return(results)
}