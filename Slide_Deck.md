Data Science Capstone Project: Word Prediction
========================================================
author: Daniel Alaiev
date: June 2016
autosize: true

Introduction
========================================================

[This application](https://datadan01.shinyapps.io/Word_Prediction_Capstone/) was created to satisfy the Capstone requirement for the Data Science Specialization from Johns Hopkins. The application:

- Takes a series of words as an input
- Checks spelling and suggests alternatives if errors are found
- Uses the original input to predict the next word

[All of the code is on GitHub](https://github.com/DataDan01/Capstone).

Getting and Cleaning Data
========================================================

[The data](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) are a collection of unstructured sentences from US blogs, Twitter, and US news sources. The data were:

- Split into distinct sentences based on punctuation (, : ; !)
- Filtered to exclude numbers, profanities, and misspellings
- Split into n-grams and tabulated by frequency counts
- Pruned to include only n-grams with frequency above 5

Algorithm Mechanics
========================================================
[The algorithm](https://github.com/DataDan01/Capstone) uses a simple back-off model with a probability adjustment based on novelty. The simple back-off model looks for an n-gram one word longer than the input, recursively. Once at least one match is found, the algorithm then predicts the full n-gram(s) with the highest novelty adjusted MLE probability: Freq(n-gram)/Freq(n-1-gram) * Nov Adj. N-gram novelty is a count of how many times the last word in the n-gram shows up in other unique n-grams, without looking at frequencies. The algorithm has about 28% accuracy for a 5-option prediction.

Instructions and Future Plans
========================================================
Using the application is easy. Just [open the link](https://datadan01.shinyapps.io/Word_Prediction_Capstone/) and type your sentence into the input box. The application will automatically process the input and report the results. It returns: the best prediction, an input check, and an output summary & table.

Future plans include:
- An interpolated model that uses lower order n-grams too
- Probability adjustments based new parameters
