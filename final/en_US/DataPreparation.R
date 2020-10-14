library(NLP)
library(tm)
library(stringi)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(RWeka)
library(textmineR)

## Read in the data
fileName = "en_US.blogs.txt"
con = file(fileName, open = "r")
lineBlogs = readLines(con, encoding='UTF-8') 
close(con)

fileName = "en_US.news.txt"
con = file(fileName, open = "r")
lineNews = readLines(con, encoding='UTF-8') 
close(con)

fileName = "en_US.twitter.txt"
con = file(fileName, open = "r")
lineTwitters = readLines(con, encoding='UTF-8') 
close(con)

## Data sampling
set.seed(404) # For reproducibility 
percentage = 0.005

sampleBlogs <- sample(lineBlogs, percentage*length(lineBlogs))
length(sampleBlogs)
head(sampleBlogs)

sampleNews <- sample(lineNews, percentage*length(lineNews))
length(sampleNews)
head(sampleNews)

percentage <- 0.004
sampleTwitters <- sample(lineTwitters, percentage*length(lineTwitters))
length(sampleTwitters)
head(sampleTwitters)

#############################################################################
sampleALL <- c(sampleBlogs, sampleNews, sampleTwitters)
sampleALL <- sample(sampleALL, 0.3*length(sampleALL))
## Transport to corpus vector modes
corpVecBlogs <- CleanedCorpVec(sampleBlogs)
corpVecNews <- CleanedCorpVec(sampleNews)
corpVecTwitters <- CleanedCorpVec(sampleTwitters)

#############################################################################
corpVecALL <- CleanedCorpVec(sampleALL)

## Create ngram tables
unigramBlogs <- NgramTables(1, corpVecBlogs)
bigramBlogs <- NgramTables(2, corpVecBlogs)
trigramBlogs <- NgramTables(3, corpVecBlogs)

unigramNews <- NgramTables(1, corpVecNews)
bigramNews <- NgramTables(2, corpVecNews)
trigramNews <- NgramTables(3, corpVecNews)

unigramTwitters <- NgramTables(1, corpVecTwitters)
bigramTwitters <- NgramTables(2, corpVecTwitters)
trigramTwitters <- NgramTables(3, corpVecTwitters)

#############################################################################

unigrams <- NgramTables(1, corpVecALL)
unigrams <- unigrams[which(unigrams$Frequency > 3),]
bigrams <- NgramTables(2, corpVecALL)
trigrams <- NgramTables(3, corpVecALL)

#############################################################################
saveRDS(unigramBlogs,"./ShinyApp/unigramBlogs.RData")
saveRDS(bigramBlogs,"./ShinyApp/bigramBlogs.RData")
saveRDS(trigramBlogs,"./ShinyApp/trigramBlogs.RData")

saveRDS(unigramNews,"./ShinyApp/unigramNews.RData")
saveRDS(bigramNews,"./ShinyApp/bigramNews.RData")
saveRDS(trigramNews,"./ShinyApp/trigramNews.RData")

saveRDS(unigramTwitters,"./ShinyApp/unigramTwitters.RData")
saveRDS(bigramTwitters,"./ShinyApp/bigramTwitters.RData")
saveRDS(trigramTwitters,"./ShinyApp/trigramTwitters.RData")
##############################################################################

saveRDS(unigrams,"unigrams.RData")
saveRDS(bigrams,"bigrams.RData")
saveRDS(trigrams,"trigrams.RData")
