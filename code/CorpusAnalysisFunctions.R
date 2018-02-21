library(ngram)

"This R file contains several functions, showing different corpus analyses we did in the past 6 weeks.
Here is a full list:
get_words_vector(file_path): this function takes a file path and returns all the words in the file as a vector.
get_freq_words(wordsvector,n): takes a words vector, and a number n, and returns n most frequent words as output.
get_lex_variety(wordsvector): takes a words vector, and returns a vector containing three measures of lexical variety.
getKwic(wordsvector, word, context): takes a words vector, a word, and a number, 
and returns wherever the word appears in the vector, showing some context.
getngrams(wordvector, ngramsize, n_frequent): use this if you want to print the n_frequent most frequent ngrams, 
with the given ngram size.
get_line_plot(wordsvector,n): plots the n most frequent words in that vector
get_dispersion_plot(wordsvector,word): plot the dispersion plot for the word, using that vector.
"

"This function takes in a text file, and returns all words in this file
as a vector, after lowercasing, and removing punctuation
note: you can use gsub to substitute punctuation, if you want.
"
get_words_vector <- function(file_path)
{
  fulltext <- scan(file_path, what = "character", sep = "\n")
  fulltext_as_string <- tolower(paste(fulltext, collapse = " "))
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

"This function takes a vector of words and returns the following:
Type-token ratio (i.e., 100 * number of unique words/total num. of words)
Avg. word frequency (i.e., total num words/num. unique words)
Hapax percentage (i.e., percentage of words that appear only once in the text)
This function returns a vector with these three values.
"
get_lex_variety <- function(wordsvector)
{
  words_freqs <- table(wordsvector)
  ttr <- 100 * length(words_freqs)/sum(words_freqs)
  awf <- sum(words_freqs)/length(words_freqs)
  hapax <- 100*sum(words_freqs==1)/length(words_freqs)
  result = c(ttr,awf,hapax)
  names(result) = c("TypeTokenRatio","avgWordFreq","hapaxPercent")
  return(result)
}

#A function to get the context in which a given word appears in a word vector
#Takes three arguments: a vector of words, a word to search for, a context window
#I converted this function to return a vector of strings, instead of printing them directly.
getKwic <- function(wordsvector, word, context)
{
  final_result = c()
  word_index <- which(wordsvector == word)
  #If the word is not found in this file: then, there is no point in going further down into the for loop.
  #It will also throw an error: "Error in if (start < 1) { : missing value where TRUE/FALSE needed"
  if(length(word_index) > 0)
  {
    for(i in 1:length(word_index)) 
    {
      start <- word_index[i] - context
      if(start < 1)
      {
        start <- 1
      }
      end <- word_index[i] + context
      if(end >= length(wordsvector))
      {
        end <- length(wordsvector)
      }
      # cat(start, end) #This prints only positions, not actual words.
      kwic <- paste(wordsvector[start:end], collapse = " ")
      final_result <- c(final_result,kwic)#, sep=" ")) 
    }
  }
  #Since the word is not in the file, I am just printing that message and moving on.
  else{
    final_result <- c("Word is not found in this file.")
  }
  return(final_result)
}

#Takes a word vector, a ngram size, and a n_frequent -3 arguments
#and returns the n_frequent most frequent n-grams of that ngram size
get_ngrams <- function(wordsvector,ngramsize,n_frequent)
{
  #ngrams library expects a string, so, first convert this vector to a string.
  text_string <- paste(wordsvector, collapse=" ")
  ngrams <- ngram(text_string,n=ngramsize)
  top10ngrams <- head(get.phrasetable(ngrams), n = n_frequent)
  return(top10ngrams)
}

"this function takes a word vector, and a number n, and plots
a graph showing the frequencies of n-most frequent words in the wordsvector
"
get_line_plot <- function(wordsvector,n)
{
  words_i_need <- get_freq_words(wordsvector,n)
  plot(words_i_need) #change this to whichever kind of plot you want. Look at ?plot for options. 
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
