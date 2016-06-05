## Loading and sampling the data
US_blogs <- readLines("./Data/en_US.blogs.txt")

US_news <- readLines("./Data/en_US.news.txt")

US_twitter <- readLines("./Data/en_US.twitter.txt")

## Separate text into distinct chunks of words based on punctuation
splitter <- function(text) {
  
  # Splitting on different symbols that are usually placed between distinct word sequences
  text_vect <- strsplit(text, "(*)[!]|[\"]|[#]|[$]|[%]|[&]|[(]|[)]|[*]|[+]|[,]|[-]|[.]|[/]|[:]|[;]|[<]|[=]|[>]|[?]|[\\]|[@]|[[]|[]]|[_]|[{]|[|]|[}]|[~](*)")
  
  no.whitespace <- function(text_line) {
    
    return(gsub("^\\s+|\\s+$", "", text_line))
  }
  
  # Removing whitespaces
  text_vect <- unlist(lapply(text_vect, no.whitespace))
  text_vect <- text_vect[text_vect != ""]
  text_vect <- tolower(text_vect)
  
  return(text_vect)
}

US_blogs <- splitter(US_blogs)
US_news <- splitter(US_news)
US_twitter <- splitter(US_twitter)

all_sent <- c(US_blogs, US_news, US_twitter)
rm(US_blogs, US_news, US_twitter)

## Setting the seed for reproducibility
set.seed(101)

## Function to sample the data, percent of the sample = perc
sampler <- function(chunk, perc) {
  
  perc <- round(length(chunk)*perc)
  sample.index <- sample(1:length(chunk), perc)
  
  return(sample.index)
}

sample.index <- sampler(chunk = all_sent, perc = 0.95)

all_sent.test <- all_sent[-sample.index]
all_sent <- all_sent[sample.index]

rm(sample.index)

save.image()

## Creating an n-gram from a sentence
n.gram <- function(sentence, n) {
  
  sent <- strsplit(sentence, split = " ")
  
  if(length(sent[[1]]) < n)
    return(NULL)
  
  if(length(sent[[1]]) == n)
    return(sentence)
  
  ns <- vector(mode = "character", length = length(sent[[1]])-n+1)
  
  # Loop through the list of words and return groups of size n
  for(i in 1:(length(sent[[1]])-n+1)) {
    
    ns[i] <- paste((sent[[1]][i:(i+n-1)]), collapse = " ")
  }
  
  return(ns)
}

## Compiling n-gram function to make it faster
library(compiler)

n.gram <- cmpfun(n.gram)

## Creating n-gram tables in parallel, exporting function
library(parallel)

cluster <- makeCluster(detectCores())

clusterExport(cluster, varlist = c("n.gram"), envir = .GlobalEnv)

## Parallel function 
n.gram.table <- function(word_list, n) {
  
  n.grams <- parLapply(cluster, word_list, n.gram, n = n)
  
  n.grams <- as.data.frame(table(unlist(n.grams)),
                                 stringsAsFactors = FALSE)
  
  colnames(n.grams)[1] <- "N.Gram"
  
  n.grams <- n.grams[order(-n.grams$Freq),]
  
  row.names(n.grams) <- 1:nrow(n.grams)
  
  return(n.grams)
}

## Tables become large so they must be written to disk and dealt with one by one
sent.1.gram <- n.gram.table(all_sent, n = 1)
write.csv(sent.1.gram, "./data/sent.1.gram.csv")
rm(sent.1.gram)

sent.2.gram <- n.gram.table(all_sent, n = 2)
write.csv(sent.2.gram, "./data/sent.2.gram.csv")
rm(sent.2.gram)

sent.3.gram <- n.gram.table(all_sent, n = 3)
write.csv(sent.3.gram, "./data/sent.3.gram.csv")
rm(sent.3.gram)

sent.4.gram <- n.gram.table(all_sent, n = 4)
write.csv(sent.4.gram, "./data/sent.4.gram.csv")
rm(sent.4.gram)

sent.5.gram <- n.gram.table(all_sent, n = 5)
write.csv(sent.5.gram, "./data/sent.5.gram.csv")
rm(sent.5.gram)

sent.6.gram <- n.gram.table(all_sent, n = 6)
write.csv(sent.6.gram, "./data/sent.6.gram.csv")
rm(sent.6.gram)

## Removing the raw sentences to free up some memory
rm(all_sent)
save.image()

## Loading the n-gram data and cutting off by freq
## Speparated into distinct steps because this operation uses a lot of memory/cpu
library(data.table)

pr.sent.1.gram <- fread("./data/sent.1.gram.csv", data.table = FALSE)
pr.sent.1.gram <- pr.sent.1.gram[,2:3]
pr.sent.1.gram <- pr.sent.1.gram[pr.sent.1.gram$Freq >= 5,]

pr.sent.2.gram <- fread("./data/sent.2.gram.csv", data.table = FALSE)
pr.sent.2.gram <- pr.sent.2.gram[,2:3]
pr.sent.2.gram <- pr.sent.2.gram[pr.sent.2.gram$Freq >= 5,]

pr.sent.3.gram <- fread("./data/sent.3.gram.csv", data.table = FALSE)
pr.sent.3.gram <- pr.sent.3.gram[,2:3]
pr.sent.3.gram <- pr.sent.3.gram[pr.sent.3.gram$Freq >= 5,]

pr.sent.4.gram <- fread("./data/sent.4.gram.csv", data.table = FALSE)
pr.sent.4.gram <- pr.sent.4.gram[,2:3]
pr.sent.4.gram <- pr.sent.4.gram[pr.sent.4.gram$Freq >= 5,]

pr.sent.5.gram <- fread("./data/sent.5.gram.csv", data.table = FALSE)
pr.sent.5.gram <- pr.sent.5.gram[,2:3]
pr.sent.5.gram <- pr.sent.5.gram[pr.sent.5.gram$Freq >= 5,]

pr.sent.6.gram <- fread("./data/sent.6.gram.csv", data.table = FALSE)
pr.sent.6.gram <- pr.sent.6.gram[,2:3]
pr.sent.6.gram <- pr.sent.6.gram[pr.sent.6.gram$Freq >= 5,]

## Filtering out numbers, curses, and spell checking

## Create a list of profanities from several sources and spell check
# https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en
bad.words.1 <- readLines("./Data/Bad_Word_List.1.txt")
bad.words.1 <- bad.words.1[-length(bad.words.1)]

# https://gist.githubusercontent.com/jamiew/1112488/raw/7ca9b1669e1c24b27c66174762cb04e14cf05aa7/google_twunter_lol
bad.words.2 <- readLines("./Data/Bad_Word_List.2.txt")
bad.words.2 <- bad.words.2[-1]

# Dealing with some formatting nuances
bad.words.2 <- substr(x = bad.words.2, start = 1, stop = nchar(bad.words.2)-3)
double.quote.index <- grep(pattern = "\"", x = bad.words.2)

bad.words.2[double.quote.index] <- substr(x = bad.words.2[double.quote.index],
                                          start = 2, 
                                          stop = nchar(
                                            bad.words.2[double.quote.index])-1)

# Return unique words from both lists and spell check them
all.bad.words <- c(bad.words.1, bad.words.2)
all.bad.words <- unique(tolower(all.bad.words))

library(hunspell)

all.bad.words <- all.bad.words[hunspell_check(all.bad.words)]

rm(bad.words.1, bad.words.2, double.quote.index)

## Checking for numbers
num.check <- function(n_gram_table) {
  
  no.number.index <- unlist(lapply(n_gram_table$N.Gram, 
                            function(x) !grepl("[[:digit:]]", x)))
  
  return(n_gram_table[no.number.index,])
}

pr.sent.1.gram <- num.check(pr.sent.1.gram)
pr.sent.2.gram <- num.check(pr.sent.2.gram)
pr.sent.3.gram <- num.check(pr.sent.3.gram)
pr.sent.4.gram <- num.check(pr.sent.4.gram)
pr.sent.5.gram <- num.check(pr.sent.5.gram)
pr.sent.6.gram <- num.check(pr.sent.6.gram)

## Checking for profanities
profanity.check <- function(n_gram_table) {
  
  n.gram.check <- function(n_gram) {
    
    n_gram <- strsplit(n_gram, split = " ")[[1]]
    
    freq.table <- table(c(all.bad.words, n_gram))
    
    if(any(freq.table > 1))
      return(FALSE)
    else
      return(TRUE)
  }
  
  no.profanity.index <- unlist(lapply(n_gram_table$N.Gram, n.gram.check))
  
  return(n_gram_table[no.profanity.index,])
}

pr.sent.1.gram <- profanity.check(pr.sent.1.gram)
pr.sent.2.gram <- profanity.check(pr.sent.2.gram)
pr.sent.3.gram <- profanity.check(pr.sent.3.gram)
pr.sent.4.gram <- profanity.check(pr.sent.4.gram)
pr.sent.5.gram <- profanity.check(pr.sent.5.gram)
pr.sent.6.gram <- profanity.check(pr.sent.6.gram)

## Recounting row numbers to prepare for next function
rownames(pr.sent.1.gram) <- 1:nrow(pr.sent.1.gram)
rownames(pr.sent.2.gram) <- 1:nrow(pr.sent.2.gram)
rownames(pr.sent.3.gram) <- 1:nrow(pr.sent.3.gram)
rownames(pr.sent.4.gram) <- 1:nrow(pr.sent.4.gram)
rownames(pr.sent.5.gram) <- 1:nrow(pr.sent.5.gram)
rownames(pr.sent.6.gram) <- 1:nrow(pr.sent.6.gram)

## Base spell checking function
n.gram.check.2 <- function(n_gram) {
  
  result <- unlist(hunspell(n_gram))
  
  if(length(result) != 0)
    return(FALSE)
  
  if(length(result) == 0)
    return(TRUE)
}

## Checking tables against a dictionary in parallel
library(parallel)

cluster <- makeCluster(detectCores())

clusterExport(cluster, varlist = c("n.gram.check.2"),
              envir = .GlobalEnv)

clusterEvalQ(cluster, library(hunspell))

dic.check <- function(n_gram_table) {
  
  dictionary.index <- unlist(parLapply(cluster, n_gram_table$N.Gram, n.gram.check.2))
  
  return(n_gram_table[dictionary.index,])
}

pr.sent.1.gram <- dic.check(pr.sent.1.gram)
pr.sent.2.gram <- dic.check(pr.sent.2.gram)
pr.sent.3.gram <- dic.check(pr.sent.3.gram)
pr.sent.4.gram <- dic.check(pr.sent.4.gram)
pr.sent.5.gram <- dic.check(pr.sent.5.gram)
pr.sent.6.gram <- dic.check(pr.sent.6.gram)

## Backing up finished n-gram tables
write.csv(pr.sent.1.gram, "./data/pr.sent.1.gram.csv")
write.csv(pr.sent.2.gram, "./data/pr.sent.2.gram.csv")
write.csv(pr.sent.3.gram, "./data/pr.sent.3.gram.csv")
write.csv(pr.sent.4.gram, "./data/pr.sent.4.gram.csv")
write.csv(pr.sent.5.gram, "./data/pr.sent.5.gram.csv")
write.csv(pr.sent.6.gram, "./data/pr.sent.6.gram.csv")

save.image()

## Preparing test set, similar functions to the table cleaning functions
## Checking for numbers
num.check.sent <- function(sent) {
  
  no.number.index <- unlist(lapply(sent, 
                                   function(x) !grepl("[[:digit:]]", x)))
  
  return(sent[no.number.index])
}

all_sent.test <- num.check.sent(all_sent.test)

## Checking for profanities
profanity.check.sent <- function(sent) {
  
  sent.check <- function(sent) {
    
    sent <- strsplit(sent, split = " ")[[1]]
    
    freq.table <- table(c(all.bad.words, sent))
    
    if(any(freq.table > 1))
      return(FALSE)
    else
      return(TRUE)
  }
  
  no.profanity.index <- unlist(lapply(sent, sent.check))
  
  return(sent[no.profanity.index])
}

all_sent.test <- profanity.check.sent(all_sent.test)

## Checking against a dictionary
dic.check.sent <- function(sent) {
  
  result <- unlist(hunspell(sent))
  
  if(length(result != 0))
    return(FALSE)
  else
    return(TRUE)
}

library(parallel)

cluster <- makeCluster(detectCores())

clusterExport(cluster, varlist = c("dic.check.sent"),
              envir = .GlobalEnv)

clusterEvalQ(cluster, library(hunspell))

dic.check.all <- function(sent) {
  
  dictionary.index <- unlist(parLapply(cluster, sent, dic.check.sent))
  
  return(sent[dictionary.index])
}

all_sent.test <- dic.check.all(all_sent.test)

## Trimming down test set to anything longer than one word
sent.lengths <- unlist(lapply(all_sent.test, 
              function(x) length(unlist(strsplit(x, split = " "))))) != 1 

all_sent.test <- all_sent.test[sent.lengths]

rm(sent.lengths)
