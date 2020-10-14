## Clean the sample lines and turn it into text corpVec
CleanedCorpVec <- function(linetext){
        linetext <- iconv(linetext, 'UTF-8', 'ASCII', "byte") ## Remove all weird characters
        sourceVec <- VectorSource(linetext) # turn character into source object
        corpVec <- VCorpus(sourceVec) # turn source object into corpus
        corpVec <- tm_map(corpVec, tolower) # covert to lower case
        ## corpVec <- tm_map(corpVec, removeWords, stopwords("english")) # remove 'a', 'an', etc.
        corpVec <- tm_map(corpVec, removePunctuation)
        corpVec <- tm_map(corpVec, removeNumbers)
        corpVec <- tm_map(corpVec, stripWhitespace)
        corpVec <- tm_map(corpVec, PlainTextDocument)
        return(corpVec)
}

## Get ngram frequency tables
NgramTables <- function(ngram, corpVec) {
        ngramTokenizer <- function(x){NGramTokenizer(x, Weka_control(min = ngram, max = ngram))}
        tdm <- TermDocumentMatrix(corpVec, control = list(tokenize = ngramTokenizer))
        term_matrix <- as.matrix(tdm)
        ngram_table <- rowSums(term_matrix)
        ngram_table <- as.data.frame(sort(ngram_table, decreasing=TRUE))
        ngram_table$term <- rownames(ngram_table)
        colnames(ngram_table) <- c("Frequency","term")
        rownames(ngram_table) <- NULL
        return(ngram_table)
}

###########################################

## Good Turing count species table
CountSpecies <- function(ngram_table){
        count_of_species <- table(ngram_table$Frequency)
        count_of_species <- as.data.frame(count_of_species)
        colnames(count_of_species) <- c("ngram_frequency", "count_of_species")
        return(count_of_species)
}

## Good Turing discount
discount <- function(count, count_of_species) {
        if(nrow(count_of_species[which(count_of_species$ngram_frequency == count + 1),]) == 0){
                good_turing_count <- (count + 1)/
                        count_of_species[which(count_of_species$ngram_frequency == count),]$count_of_species
        }
        else{
                good_turing_count <- (count + 1) *
                        count_of_species[which(count_of_species$ngram_frequency == count) + 1,]$count_of_species /
                        count_of_species[which(count_of_species$ngram_frequency == count),]$count_of_species
        }
        computed_discount <- good_turing_count / count
        ifelse(computed_discount < 1, computed_discount, 1)
}

###########################################

## Get Observed trigram frequecy
ObsTrigs <- function(bigPre, trigrams) {
        obs_trigrams <- data.frame(term = vector(mode = 'character'),
                                 Frequency = vector(mode = 'integer'))
        regex <- sprintf("%s%s%s", "^", bigPre, " ")
        trigram_indices <- grep(regex, trigrams$term)
        if(length(trigram_indices) > 0) {
                obs_trigrams <- trigrams[trigram_indices, ]
        }
        
        return(obs_trigrams)
}

#########################################
## Get Observed trigram probability

ObsTriProbs <- function(obsNgrams) {
        if(nrow(obsNgrams) < 1) return(NULL)
        obsCount <- sum(obsNgrams$Frequency)
        count_of_species <- CountSpecies(obsNgrams)
        obsNgrams["discount"] <- 0.75
        obsNgramsProbs <- mutate(obsNgrams, Frequency = discount* (Frequency/ obsCount))
        colnames(obsNgramsProbs) <- c("Probability", "term", "discount")
        return(obsNgramsProbs)
}
########################################################
ObsNProbs <- function(obsNgrams) {
        if(nrow(obsNgrams) < 1) return(NULL)
        obsCount <- sum(obsNgrams$Frequency)
        count_of_species <- CountSpecies(obsNgrams)
        obsNgrams["discount"] <- NA
        for (i in 1:nrow(obsNgrams)){
                obsNgrams$discount[i] <- discount(obsNgrams$Frequency[i], count_of_species)
        }
        obsNgramsProbs <- mutate(obsNgrams, Frequency = discount * (Frequency/ obsCount))
        colnames(obsNgramsProbs) <- c("Probability", "term", "discount")
        return(obsNgramsProbs)
}
#########################%
## Get observed trigram tails
UnobsTrigTails <- function(obs_trigrams, unigs) {
        obs_trig_tails <- NA
        for (i in 1:nrow(obs_trigrams)){
                obs_trig_tails[i] <- strsplit(obs_trigrams$term, " ")[[i]][3]    
        }
        unobs_trig_tails <- unigs[!(unigs$term %in% obs_trig_tails), ]$term
        return(unobs_trig_tails)
}
## Get observed bigram tails
UnobsBigTails <- function(obs_bigrams, unobs_trig_tails) {
        obs_big_tails <- NA
        for (i in 1:nrow(obs_bigrams)){
                obs_big_tails[i] <- strsplit(obs_bigrams$term, " ")[[i]][2]    
        }
        unobs_big_tails <- unobs_trig_tails[!(unobs_trig_tails %in% obs_big_tails) ]
        return(unobs_big_tails)
}

##get unobserved unigram
unObsUnigrams <- function(unobs_big_tails, unigrams){
        unobs_unigrams <- data.frame(term = vector(mode = 'character', length = 0),
                                     Frequency = vector(mode = 'integer', length = 0))
        unobs_unigrams <- unigrams[unigrams$term %in% unobs_big_tails,]
        return(unobs_unigrams)
}
        

## If x,y,z unobserved, find y,w as bigram
unObsTrigsinBigrams <- function(bigPre, bigrams, unobsTrigTails) {
        bigPre_tail <- strsplit(bigPre, " ")[[1]][2]
        unobs_trigrams_in_bi <- data.frame(term = vector(mode = 'character', length = 0),
                                   Frequency = vector(mode = 'integer', length = 0))
        regex <- paste(bigPre_tail, unobsTrigTails, sep = " ")
        
        unobs_trigrams_in_bi <- bigrams[bigrams$term %in% regex,]
        return(unobs_trigrams_in_bi)
}

predictNEXT <- function(bigPre, unigrams, bigrams, trigrams){
        obs_trigrams <- ObsTrigs(bigPre, trigrams)
        if (nrow(obs_trigrams) != 0){
                obs_trigrams_prob <- ObsTriProbs(obs_trigrams)
                unobs_tri_tail <- UnobsTrigTails(obs_trigrams, unigrams)
                obs_bigrams <- unObsTrigsinBigrams(bigPre, bigrams, unobs_tri_tail)
                if (nrow(obs_bigrams) != 0){
                        obs_bigrams_prob <- ObsNProbs(obs_bigrams)
                        unobs_bi_tail <- UnobsBigTails(obs_bigrams, unobs_tri_tail)
                        obs_unigrams <- unObsUnigrams(unobs_bi_tail, unigrams)
                        obs_unigrams_prob <- ObsNProbs(obs_unigrams)
                        ############################################
                        beta2 <- 1- sum(obs_bigrams_prob$Probability)
                        alpha2 <- beta2/sum(obs_unigrams_prob$Probability)
                        obs_unigrams_prob$Probability <- obs_unigrams_prob$Probability * alpha2
                        beta3 <- 1- sum(obs_trigrams_prob$Probability)
                        alpha3 <- beta3/(sum(obs_bigrams_prob$Probability) + sum(obs_unigrams_prob$Probability))
                        obs_bigrams_prob$Probability <- obs_bigrams_prob$Probability * alpha3
                        obs_unigrams_prob$Probability <- obs_unigrams_prob$Probability * alpha3
                }
                else{
                        obs_unigrams <- unObsUnigrams(unobs_tri_tail, unigrams)
                        obs_unigrams_prob <- ObsNProbs(obs_unigrams)
                        ###########################################
                        beta3 <- 1- sum(obs_trigrams_prob$Probability)
                        alpha3 <- beta3/sum(obs_unigrams_prob$Probability)
                        obs_unigrams_prob$Probability <- obs_unigrams_prob$Probability * alpha3
                }
        }
        else{
                unobs_tri_tail <- unigrams$term
                bigPre_tail <- strsplit(bigPre, " ")[[1]][2]
                obs_bigrams <- ObsTrigs(bigPre_tail, bigrams)
                if (nrow(obs_bigrams) != 0){
                        obs_bigrams_prob <- ObsNProbs(obs_bigrams)
                        unobs_bi_tail <- UnobsBigTails(obs_bigrams, unobs_tri_tail)
                        obs_unigrams <- unObsUnigrams(unobs_bi_tail, unigrams)
                        obs_unigrams_prob <- ObsNProbs(obs_unigrams)
                        ###########################################
                        beta2 <- 1- sum(obs_bigrams_prob$Probability)
                        alpha2 <- beta2/sum(obs_unigrams_prob$Probability)
                        obs_unigrams_prob$Probability <- obs_unigrams_prob$Probability * alpha2
                }
                else{
                        obs_unigrams <- unigrams
                        obs_unigrams_prob <- ObsNProbs(obs_unigrams)
                }
                
        }
        
        ## Get the final list of unigrams with probability
        finalTriTail <- data.frame(word = vector(mode = 'character', length = nrow(obs_trigrams)),
                                   prob = vector(mode = 'numeric', length = nrow(obs_trigrams)))
        finalBiTail <- data.frame(word = vector(mode = 'character', length = nrow(obs_bigrams)),
                                  prob = vector(mode = 'numeric', length = nrow(obs_bigrams)))
        finalUniTail <- data.frame(word = vector(mode = 'character', length = nrow(obs_unigrams)),
                                   prob = vector(mode = 'numeric', length = nrow(obs_unigrams)))
        
        if (nrow(obs_trigrams) != 0){
                for (i in 1:nrow(obs_trigrams)){
                        finalTriTail$word[i] <- strsplit(obs_trigrams_prob$term, " ")[[i]][3]
                }
                finalTriTail$prob <- obs_trigrams_prob$Probability
                
        }

        
        if (nrow(obs_bigrams) != 0){
                for (i in 1:nrow(obs_bigrams_prob)){
                        finalBiTail$word[i] <- strsplit(obs_bigrams_prob$term, " ")[[i]][2]
                }
                finalBiTail$prob <- obs_bigrams_prob$Probability
                
        }
        
        finalUniTail$word <- obs_unigrams_prob$term
        finalUniTail$prob <- obs_unigrams_prob$Probability

        final <- rbind(finalTriTail, finalBiTail, finalUniTail)
        final <- final[order(-final$prob),]
        return(final[1:10,])
}







