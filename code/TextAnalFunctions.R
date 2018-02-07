"
This R program has three functions: 
a) one reads in a text file and converts it into a vector of words
b) second one takes the vector and prints out n-most frequent words in it.
c) third one plots the dispersion plot for a word in a text
"
library(stringr)

"This function takes in a text file, and returns all words in this file
as a vector, after lowercasing, and removing punctuation
note: you can use gsub to substitute punctuation, if you want.
"
get_words_vector <- function(file_path)
{
  fulltext <- scan(file_path, what = "character", sep = "\n")
  fulltext_as_string <- paste(fulltext, collapse = " ")
  words_vector <- unlist(strsplit(tolower(fulltext_as_string), "\\W+"))
  return (words_vector)
}

"
This function takes a vector of words from a text, 
and returns you n most frequent words in the text and their frequencies
note: modify the code to print in a neater way, whichever way you want!
"
get_freq_words <- function(wordsvector, n)
{
  sorted_freqs <- sort(table(wordsvector), decreasing = TRUE)
  return(sorted_freqs[1:n])
}

"
this function takes a word vector and a word as input, and plots the 
dispersion plot for that word in the word vector. 
Note: This function does not return a value. It directly shows the plot on screen. 
"
get_dispersion_plot <- function(wordsvector, word)
{
  progress <- seq(1:length(wordsvector))
  word_presence <- which(wordsvector == word)
  length(word_presence)
  word_progression <- rep(NA, length(progress))
  word_progression[word_presence] <- 1
  plot(word_progression, main="Dispersion plot for the given word in the given vector",
       xlab="position in text", ylab=word, type="h", ylim=c(0,1), yaxt = 'n')
}

"
usage example:
source('this file.R')
words <- get_words_vector('somefile.txt')
get_dispersion_plot(words, 'someword')
Note: You can also just save the plot into some output file instead of just printing
here. Figuring out how is left as an exercise.
"
