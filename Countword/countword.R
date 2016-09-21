setwd("C:~")
library("methods")
a                                                         = readLines("example.txt")

neg                                                       = readLines("negative-words.txt")
pos                                                       = readLines("positive-words.txt")

# function score.sentiment
score.sentiment                                           = function(sentence, pos.words, neg.words)
{
  # remove punctuation
  sentence                                                = gsub("[[:punct:]]", "", sentence)
  # remove control characters
  sentence                                                = gsub("[[:cntrl:]]", "", sentence)
  # remove digits?
  sentence                                                = gsub('\\d+', '', sentence)
  
  # define error handling function when trying tolower
  tryTolower                                              = function(x)
  {
    # create missing value
    y                                                     = NA
    # tryCatch error
    try_error                                             = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y                                                   = tolower(x)
    # result
    return(y)
  }
  # use tryTolower with sapply 
  sentence                                                = sapply(sentence, tryTolower)
  
  # split sentence into words with str_split (stringr package)
  word.list                                               = strsplit(sentence, "\\s+")
  words                                                   = unlist(word.list)
  
  # compare words to the dictionaries of positive & negative terms
  pos.matches                                             = match(words, pos.words)
  neg.matches                                             = match(words, neg.words)
  
  # get the position of the matched term or NA
  # we just want a TRUE/FALSE
  pos.matches                                             = !is.na(pos.matches)
  neg.matches                                             = !is.na(neg.matches)
  
  # final score
  score                                                   = sum(pos.matches) - sum(neg.matches)
  pos                                                     = sum(pos.matches)
  neg                                                     = sum(neg.matches)
  num                                                     = length(pos.matches)
  sent                                                    = matrix(c(pos,neg,score,num),nrow=1)
  scores.df                                               = data.frame(score=sent)
  
  names(scores.df)                                        = c("pos","neg","opt","totalwords")
  scores.df
}

#sent = paste(rep(sentences,3),collapse="")
score.sentiment(a, pos, neg) 



