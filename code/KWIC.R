#Goes through text files in a folder, searches for a given word in a file, and returns its context (neighboring words)
#If the word is not seen in a while, just prints a message mentioning that. 
#Just a demonstration - change it as you want, adding properly structured functions. 

#This is a function to do some file preprocessing and convert it into a words vector.
processfile <- function(file_name)
{
  text <- scan(file_name, what = "character", sep = "\n") 
  text_as_string <- tolower(paste(text, collapse = " "))
  text_as_string <- gsub("([[:punct:]])", "\\ ", text_as_string)
  text_as_string <-  gsub(" +", " ", text_as_string)
  return(unlist(strsplit(text_as_string, " ")))
}

#A function to get the context in which a given word appears in a word vector
#Takes three arguments: a vector of words, a word to search for, a context window
getKwic <- function(wordsvector, word, context)
{
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
    cat(paste(wordsvector[start:end]))#, sep=" ")) 
    cat("\n")
  }
 }
  #Since the word is not in the file, I am just printing that message and moving on.
  else{
    print("Word is not found in this file.")
  }
}

#Get input from user
word <- readline("Enter the word for which you want the context: ")
context <- strtoi(readline("Enter the number of words before and after you want to print: "))

#get all .txt files in the working directory
file.names <- dir(getwd(), pattern = "\\.txt")

#For all text files in the working directory, get the words vector and use it with KWIC function
for (f in file.names) {
  x <- processfile(f)
  getKwic (x, word, context)
  print("***END OF ONE FILE***")
}
