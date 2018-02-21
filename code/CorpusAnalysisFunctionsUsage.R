
#Change the path here according to your computers!!
source('~/Dropbox/ClassroomSlides-BothCourses/LING410X/22FEB2018Tutorial/CorpusAnalysisFunctions.R')

file.names <- dir(getwd(), pattern = "\\.txt")
my_final_vector <- c()
for (f in file.names) {
  x <- get_words_vector(f)
  my_final_vector <- rbind(my_final_vector, get_lex_variety(x))
}


#Q3 answer:
text6words <- get_words_vector("text6.txt")
text5words <- get_words_vector("text5.txt")
text650 <- get_ngrams(text6words,3,50)
text550 <- get_ngrams(text5words,3,50)
intersect(unlist(text550$ngrams),unlist(text650$ngrams))


write.csv(my_final_vector,"../temp.csv")