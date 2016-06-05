This is the Capstone Project for the [Johns Hopkin's Data Science specialization on Coursera](https://www.coursera.org/specializations/jhu-data-science?utm_source=gg&utm_medium=sem&utm_campaign=data_science_search_us&campaignid=313639147&device=c&keyword=john%20hopkins%20data%20science&matchtype=e&network=g&devicemodel=&adpostion=1t1&hide_mobile_promo&gclid=CIneqve3jM0CFVVZhgodwJYIQQ). The aim of this project is to create an application that predicts the next word of a sentence. The [training data](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) are a collection of text from US news sources, Twitter, and US blogs. The data were cleaned and split into [n-grams](https://en.wikipedia.org/wiki/N-gram) from sizes one to six.

The algorithm uses a simple back-off model with a probability adjustment based on novelty. The simple back-off model looks for an n-gram one word longer than the input. If no n-gram is found, the first word of the input is dropped and the process repeats itself until there is a match. Once at least one match is found, the algorithm then predicts the full n-gram(s) with the highest estimated probability. Estimated (Maximum Likelihood) probability is calculated as Freq(n-gram)/Freq(n-1-gram). These probabilities are then adjusted based on their novelty. N-gram novelty is a count of how many times the last word in the n-gram shows up in other unique n-grams, without looking at their frequencies. Low novelty increases the probability of an n-gram. 5% of the data were left out for testing and the algorithm has about 28% accuracy for a 5-option prediction.

Important Links:

[Slideshow](http://rpubs.com/daniel_alaiev/186996)

[Code](https://github.com/DataDan01/Capstone)
