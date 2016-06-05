shinyUI(fluidPage(column(width = 10, align = "left", offset = 4,
  headerPanel("Word Prediction Algorithm"),

  # Define the sidebar with one input
    textInput(inputId = "input.text", 
              label = "Your input: ", 
              value = "type here"),

    
    h3("Best Prediction"),
    column(verbatimTextOutput(outputId = "best.pred"),
           width = 2,
           offset = 0),
    h2("*"),
    h3("Input Check"),
    htmlOutput("check"),
    h3("Output Summary"),
    htmlOutput("summary"),
    h3("Output Table"),
    htmlOutput("table")
  
)))