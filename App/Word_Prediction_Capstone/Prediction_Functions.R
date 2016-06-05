load("./data/data.RData")

## Robust backoff function for prediction
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

boff.cmp <- cmpfun(backoff); rm(backoff)

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

## Checking the inputs
library(hunspell)

input.check <- function(sentence) {
  
  sentence <- tolower(sentence)
  
  mistakes <- unlist(hunspell(sentence))
  
  names(sentence) <- "Input Used"
  
  if(length(mistakes) == 0) {
    
    header <- "No errors detected"
    
    names(header) <- "Message"
    
    output.list <- c(as.list(header), as.list(sentence))
    
    output.list$Message <- noquote(output.list$Message)
    
    return(output.list)
  }
  
  header <- noquote("Please consider changing the word(s):")
  
  names(header) <- "Warning"
  
  suggestions <- sapply(mistakes, hunspell_suggest)
  
  output.list <- c(header, suggestions, sentence)
  
  return(output.list)
}
