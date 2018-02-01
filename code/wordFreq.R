#set your working directory to where this file is, or give full path
library(stringr)
english <- scan("DollsHouse-Esperanto.txt", what = "character", sep = "\n")
english.start <- which(str_detect(english,"START OF THIS PROJECT"))
english.end <- which(str_detect(english,"END OF THIS PROJECT"))
actual_english <- english[english.start+1:english.end]
actual_english_string <- paste(actual_english, collapse = " ")
english_lower <- tolower(actual_english_string)
english_words <- strsplit(english_lower, "\\W+")
sorted_freqs_english <- sort(table(english_words), decreasing = TRUE)
plot(sorted_freqs_english[1:11], type="b")

