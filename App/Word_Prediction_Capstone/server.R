## Loading in the data once and necessary libraries
source("./Prediction_Functions.R", local = TRUE)
require(compiler)
require(hunspell)

## Reactive code
shinyServer(function(input, output) {
  
  output$check <- renderUI({
  
  checked.input <- input.check(input$input.text)
      
    if(length(checked.input) == 2) {
      
      str1 <- "No errors detected.<br/>"
      str2 <- paste("Input Passed: ", checked.input$`Input Used`)
      
      output.1 <- HTML(paste(str1, str2, sep = "<br/>"))
      
      return(output.1)
    }
    
    if(length(checked.input) > 2) {
      
      str1 <- "Potential errors detected."
      
      str2 <- checked.input$Warning
      
      str3 <- "<br/>"
      
      for(i in 3:length(checked.input)-1) {
        
        str3 <- paste(
          str3,
          paste0(names(checked.input)[i], " -->"),
          paste(checked.input[[i]], collapse = " | "),
          "<br/>"
        )
      }
      
      str4 <- paste("Input Passed: ", checked.input$`Input Used`)
      
      output.2 <- HTML(paste(str1, str2, str3, str4, sep = "<br/>"))
      
      return(output.2) 
    }
    
    
  })
  
  output$summary <- renderUI({
    
    checked.input <- input.check(input$input.text)
    
    pred <- predict.ngram(tune = 30,
                          sentence = input$input.text)
    
    output$best.pred <- renderText({
      
      output.0 <- pred$`Best Predictions`[1]

      return(output.0)
      })
    
    str1 <- paste0("Original Input: ", pred$`Original Input`)
    
    str2 <- paste0("Input Used: ", pred$`Input Used`)
    
    str3 <- paste0("Back-Offs: ", pred$`Back-Offs`)
    
    str4 <- paste0("Seen Before? ", pred$`Seen Before?`)
    
    str5 <- "<br/>Best Prediction(s): "
    
    str6 <- paste(pred$`Best Predictions`, collapse = " | ")
    
    str5 <- paste(str5, str6)        
    
    output.3 <- HTML(paste(str1, str2, str3, 
                           str4, str5, sep = "<br/>"))
    
    output$table <- renderTable({pred$`Result Table`})
    
    return(output.3)
  })  
  
})