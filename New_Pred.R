############################################################
# New prediction function that always finds 5 predictions,
# regardless of their n-gram size. How do I properly interpolate?
# To be worked on later.

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
  
  ## Keep backing off an modifying result table until there are 5 results
  complete <- function(match.index) {
  
    if(nrow(`Result Table`) < 5) {
      
      sentence <- boff.cmp(sentence)
      
      len <- length(unlist(strsplit(sentence, split = " "))) 
      
      test.gram <- paste0("^(", sentence, ") +")
      
      match.index <- grep(pattern = test.gram, x = adj.list[[len+1]]$N.Gram)
      
      original.tails <- unlist(lapply(`Result Table`$N.Gram, boff.cmp, last = TRUE))
      
      new.table <- adj.list[[len+1]][match.index,]
      
      new.tails <- unlist(lapply(new.table$N.Gram, boff.cmp, last = TRUE))
      
      new.index <- which(new.tails %in% original.tails)
      
      `Result Table` <<- rbind(`Result Table`, new.table[-new.index,])
    }
    
  }
  
  if(nrow(`Result Table` >= 5)) 
    `Best Predictions` <- unlist(lapply(`Result Table`$N.Gram[1:5], boff.cmp, last = TRUE))
  
  
    
  results <- list(`Original Input` = original.string,
                  `Input Used` = sentence,
                  `Back-Offs` = length(strsplit(original.string, split = " ")[[1]]) 
                  - length(strsplit(sentence, split = " ")[[1]]),
                  `Seen Before?` = "Yes",
                  `Best Predictions` = `Best Predictions`,
                  `Result Table` = `Result Table`)
  
  rm(original.string, `Result Table`,envir = .GlobalEnv)
  
  return(results)  
}
