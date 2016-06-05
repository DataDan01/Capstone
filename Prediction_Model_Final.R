## Loading pruned data and pruning further, only counts of 5 or more
pr.sent.1.gram <- read.csv("./data/pr.sent.1.gram.csv", stringsAsFactors = F)
pr.sent.2.gram <- read.csv("./data/pr.sent.2.gram.csv", stringsAsFactors = F)
pr.sent.3.gram <- read.csv("./data/pr.sent.3.gram.csv", stringsAsFactors = F)
pr.sent.4.gram <- read.csv("./data/pr.sent.4.gram.csv", stringsAsFactors = F)
pr.sent.5.gram <- read.csv("./data/pr.sent.5.gram.csv", stringsAsFactors = F)
pr.sent.6.gram <- read.csv("./data/pr.sent.6.gram.csv", stringsAsFactors = F)
  
pr.sent.1.gram <- pr.sent.1.gram[pr.sent.1.gram$Freq >= 5, 2:3]
pr.sent.2.gram <- pr.sent.2.gram[pr.sent.2.gram$Freq >= 5, 2:3]
pr.sent.3.gram <- pr.sent.3.gram[pr.sent.3.gram$Freq >= 5, 2:3]
pr.sent.4.gram <- pr.sent.4.gram[pr.sent.4.gram$Freq >= 5, 2:3]
pr.sent.5.gram <- pr.sent.5.gram[pr.sent.5.gram$Freq >= 5, 2:3]
pr.sent.6.gram <- pr.sent.6.gram[pr.sent.6.gram$Freq >= 5, 2:3]

## Looking at size of total data set
print(
object.size(c(pr.sent.1.gram,pr.sent.2.gram,pr.sent.3.gram,
              pr.sent.4.gram,pr.sent.5.gram,pr.sent.6.gram)),
              units = "MB")

## Robust backoff function for prediction and novelty counts
backoff <- function(string, end = FALSE, last = FALSE) {
  
  split.string <- strsplit(string, split = " ")[[1]]
  len <- length(split.string)
  
  if(last == TRUE)
    return(split.string[len])
  
  if(len == 0)
    return("")
  
  if(len == 1)
    return(NA)
  
  if(end == FALSE)
    bo.string <- paste(split.string[2:len], collapse = " ")

  if(end == TRUE)
    bo.string <- paste(split.string[1:len-1], collapse = " ")
  
  return(bo.string)
}

## Compiling for speed
library(compiler)

boff.cmp <- cmpfun(backoff)

## Creating the novelty counts
## Novelty counts look at the number of n-grams that end in a particular word
library(parallel)

cluster <- makeCluster(detectCores())

clusterExport(cluster, varlist = c("boff.cmp"),
              envir = .GlobalEnv)

novelty <- function(n_gram_table) {
  
  ends <- unlist(parLapply(cluster, n_gram_table$N.Gram, boff.cmp, last = TRUE))
  
  counts <- as.data.frame(table(ends), stringsAsFactors = FALSE)
  
  counts <- counts[order(-counts$Freq),]
  
  row.names(counts) <- 1:nrow(counts)
  
  return(counts)
}

# Novelty counts of 1-grams = their frequency
nov.1 <- pr.sent.1.gram
colnames(nov.1) <- c("ends","Freq")

nov.2 <- novelty(pr.sent.2.gram)
nov.3 <- novelty(pr.sent.3.gram)
nov.4 <- novelty(pr.sent.4.gram)
nov.5 <- novelty(pr.sent.5.gram)
nov.6 <- novelty(pr.sent.6.gram)

## Merging and cleaning up the novelty frequency counts
nov.table <- merge(nov.1, nov.2, by = "ends", all = TRUE)
nov.table <- merge(nov.table, nov.3, by = "ends", all = TRUE)
nov.table <- merge(nov.table, nov.4, by = "ends", all = TRUE)
nov.table <- merge(nov.table, nov.5, by = "ends", all = TRUE)
nov.table <- merge(nov.table, nov.6, by = "ends", all = TRUE)

colnames(nov.table) <- c("Word", "1-grams", "2-grams", "3-grams",
                         "4-grams", "5-grams", "6-grams")

nov.table <- nov.table[order(-nov.table$`1-grams`),]

row.names(nov.table) <- 1:nrow(nov.table)

rm(nov.1, nov.2, nov.3, nov.4, nov.5, nov.6)

## Updating pruned data to include novelty counts
nov.extract <- function(n_gram) {
  
  nov.count <- NA
  
  n_size <- length(unlist(strsplit(n_gram, split = " ")))
  
  word <- boff.cmp(n_gram, last = TRUE)
  
  index <- which(word == nov.table$Word)
  
  ## Only assign value if the word's novelty was actually found
  if(length(index) > 0)
    nov.count <- nov.table[index, n_size+1]
  
  nov.count <- setNames(nov.count, word)
  
  return(nov.count)
}

## Updating n-gram tables to include novelty counts in parallel
library(parallel)
cluster <- makeCluster(detectCores())

clusterExport(cluster, varlist = c("nov.extract", "boff.cmp", "nov.table"),
              envir = .GlobalEnv)

nov.count <- function(n_gram_table) {
  
  Nov <- unlist(parLapply(cluster, n_gram_table$N.Gram, nov.extract))
  
  new_table <- cbind(n_gram_table, Nov)
  
  return(new_table)
}

adj.1.grams <- nov.count(pr.sent.1.gram)
adj.2.grams <- nov.count(pr.sent.2.gram)
adj.3.grams <- nov.count(pr.sent.3.gram)
adj.4.grams <- nov.count(pr.sent.4.gram)
adj.5.grams <- nov.count(pr.sent.5.gram)
adj.6.grams <- nov.count(pr.sent.6.gram)

## Cleaning up
rm(nov.table, pr.sent.1.gram, pr.sent.2.gram, pr.sent.3.gram,
   pr.sent.4.gram, pr.sent.5.gram, pr.sent.6.gram)

## Trimming down data after novelty counts
## Depending on the frequency prine choice, some NAs make it through
adj.1.grams <- adj.1.grams[!is.na(adj.1.grams$Nov),]
adj.2.grams <- adj.2.grams[!is.na(adj.2.grams$Nov),]
adj.3.grams <- adj.3.grams[!is.na(adj.3.grams$Nov),]
adj.4.grams <- adj.4.grams[!is.na(adj.4.grams$Nov),]
adj.5.grams <- adj.5.grams[!is.na(adj.5.grams$Nov),]
adj.6.grams <- adj.6.grams[!is.na(adj.6.grams$Nov),]

save.image()

## Calculating unadjusted probabilies - slow
plain.prob <- function(n_gram) {
  
  len <- length(unlist(strsplit(n_gram, split = " ")))
  
  numerator <- which(adj.list[[len]]$N.Gram == n_gram)
  numerator <- adj.list[[len]]$Freq[numerator]
  
  denominator <- which(adj.list[[len-1]]$N.Gram == boff.cmp(n_gram, end = T))
  denominator <- adj.list[[len-1]]$Freq[denominator]  
  
  prob <- numerator/denominator
  
  return(prob)
}

library(compiler)

plain.prob <- cmpfun(plain.prob)

library(parallel)

cluster <- makeCluster(detectCores())

clusterExport(cluster, varlist = c("plain.prob", "boff.cmp", "adj.list"),
              envir = .GlobalEnv)

## Updating the tables with probabilities
all.probs.plain <- function(n_gram_table) {
  
  Prob <- unlist(parLapply(cluster, n_gram_table$N.Gram, plain.prob))
  
  new_table <- cbind(n_gram_table, Prob)
  
  return(new_table)
}

## Dealing with the special case of the 1-grams
adj.1.grams$Prob <- adj.1.grams$Freq/sum(adj.1.grams$Freq)

adj.2.grams <- all.probs.plain(adj.2.grams)
adj.3.grams <- all.probs.plain(adj.3.grams)
adj.4.grams <- all.probs.plain(adj.4.grams)
adj.5.grams <- all.probs.plain(adj.5.grams)
adj.6.grams <- all.probs.plain(adj.6.grams)

## Combining data into a list for the prediction function later
adj.list <- list(adj.1.grams, adj.2.grams, adj.3.grams,
                 adj.4.grams, adj.5.grams, adj.6.grams)

stopCluster(cluster)
save.image()

## Write CSVs one final time and cleaning up
write.csv(adj.1.grams, "./data/adj.1.grams.csv")
write.csv(adj.2.grams, "./data/adj.2.grams.csv")
write.csv(adj.3.grams, "./data/adj.3.grams.csv")
write.csv(adj.4.grams, "./data/adj.4.grams.csv")
write.csv(adj.5.grams, "./data/adj.5.grams.csv")
write.csv(adj.6.grams, "./data/adj.6.grams.csv")

rm(adj.1.grams,adj.2.grams,adj.3.grams,
   adj.4.grams,adj.5.grams,adj.6.grams)

## Adjusted probabilities based on novelty, to be used in main function
decay.adj <- function(t = FALSE, result_table) {
  
  if(t == FALSE)
    return(result_table)
  
  # Probability we have left to work with
  prob.space <- 1 - result_table$Prob 
  
  # Optimal function structure after lots of testing
  adj <- (pi*10)^(-(log(result_table$Nov+t))/(pi*exp(1)))
  
  # Moving the probability towards 1 based on the novelty and previous prob
  result_table$Adj.Prob <- (prob.space * adj) + result_table$Prob
  
  result_table <- result_table[order(-result_table$Adj.Prob),]
  
  return(result_table)
}

## Adjusted prediction function
predict.ngram <- function(sentence, tune) {
  
  # Create a variable in the gloval environment to know what the original
  # input was because the function is recursive
  if(exists("original.string", envir = .GlobalEnv) == FALSE) {
    
    rm(original.string)
    original.string <<- sentence
  }
  
  len <- length(unlist(strsplit(sentence, split = " ")))
  
  # If the input is longer than 5 words, backoff since the biggest table has 6-grams
  if(len > 5)
    return(predict.ngram(boff.cmp(sentence), tune = tune))
  
  # Search the beginning of the n+1 grams for the n word input
  test.gram <- paste0("^(", sentence, ") +")
  
  match.index <- grep(pattern = test.gram, x = adj.list[[len+1]]$N.Gram)
  
  # If there aren't any matches and the function has backed off to just one word,
  # return the top 10 most frequent 1-grams
  if(length(match.index) == 0 & len == 1) {
    
    `Result Table` <- head(adj.list[[1]], n = 10) 
    
    results <- list(`Original Input` = original.string,
                    `Input Used` = "NO PREDICTION - Outputting 1-Grams",
                    `Back-Offs` = length(strsplit(original.string, split = " ")[[1]]) 
                    - length(strsplit(sentence, split = " ")[[1]]) + 1,
                    `Seen Before?` = "No",
                    `Best Predictions` = `Result Table`$N.Gram[1:5],
                    `Result Table` = `Result Table`)
    
    rm(original.string, envir = .GlobalEnv)
    
    return(results)    
  }
  
  # If there aren't any matches, backoff and try the function again
  if(length(match.index) == 0)
    return(predict.ngram(boff.cmp(sentence), tune = tune))
  
  # If there are matches, return an adjusted list of results
  match.index <- head(match.index, n = 10)
  
  `Result Table` <- adj.list[[len+1]][match.index,]
  
  `Result Table` <- decay.adj(t = tune, result_table = `Result Table`)
  
  if(length(match.index) <= 5)
    `Best Predictions` <- unlist(lapply(`Result Table`$N.Gram, boff.cmp, last = TRUE))
  
  if(length(match.index) > 5)
    `Best Predictions` <- unlist(lapply(`Result Table`$N.Gram[1:5], boff.cmp, last = TRUE))
  
  results <- list(`Original Input` = original.string,
                  `Input Used` = sentence,
                  `Back-Offs` = length(strsplit(original.string, split = " ")[[1]]) 
                  - length(strsplit(sentence, split = " ")[[1]]),
                  `Seen Before?` = "Yes",
                  `Best Predictions` = `Best Predictions`,
                  `Result Table` = `Result Table`)
  
  rm(original.string, envir = .GlobalEnv)
  
  return(results)  
}
