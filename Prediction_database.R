#Building the next word prediction

library(tm)
library(stringr)

#Load the n-gram databases 

bigram<-readRDS("bigram.RData")
trigram<-readRDS("trigram.RData")
quagram<-readRDS("quagram.RData")
quigram<-readRDS("quigram.RData")

PNW <- function(sentence) {
  
  inputtxt <- gsub('[[:punct:]]|[[:digit:]]', "", sentence) ## Remove numbers and punctuations
  inputtxt <- unlist(strsplit(inputtxt, "\\s+")) # split the input string by white spaces
  
  req(inputtxt)
  
  if (length(inputtxt)>= 4) {
    
    outputtxt <- paste(tail(inputtxt, 4), collapse = " ") # tail() targets last 4 words only
    predictedtxt<- quigram$word[grepl(pattern= paste("^",outputtxt), x=quigram$word, ignore.case = T)]
    guess_word <- word(predictedtxt[1],-1)
          
    if(length(predictedtxt)==0){
      
      outputtxt <- paste(tail(inputtxt, 3), collapse = " ")
      predictedtxt<- quagram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=quagram$word, ignore.case = T)] 
      guess_word <- word(predictedtxt[1],-1)
      
      if(length(predictedtxt)==0){
        
        outputtxt <- paste(tail(inputtxt, 2), collapse = " ")
        predictedtxt<- trigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=trigram$word, ignore.case = T)]
        guess_word <- word(predictedtxt[1],-1)
        
        if(length(predictedtxt)==0){
          
          outputtxt <- paste(tail(inputtxt, 1), collapse = " ")
          predictedtxt<- bigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=bigram$word, ignore.case = T)]
          guess_word <- word(predictedtxt[1],-1)
          
          if(length(predictedtxt)==0){
            guess_word<-"the"
          }
        }
      }
    }
  } else if (length(inputtxt)==3) {
    
    outputtxt <- paste(inputtxt, collapse = " ")
    predictedtxt<- quagram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=quagram$word, ignore.case = T)]
    guess_word <- word(predictedtxt[1],-1)
    
    if(length(predictedtxt)==0){
      
      outputtxt <- paste(tail(inputtxt, 2), collapse = " ")
      predictedtxt<- trigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=trigram$word, ignore.case = T)]
      guess_word <- word(predictedtxt[1],-1)
      
      if(length(predictedtxt)==0){
        
        outputtxt <- paste(tail(inputtxt, 1), collapse = " ")
        predictedtxt<- bigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=bigram$word, ignore.case = T)]
        guess_word <- word(predictedtxt[1],-1)
        
        if(length(predictedtxt)==0){
          guess_word<-"the"
        }
      }
    }
  } else if (length(inputtxt)==2) {
    
    outputtxt <- paste(inputtxt, collapse = " ")
    predictedtxt<- trigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=trigram$word, ignore.case = T)]
    guess_word <- word(predictedtxt[1],-1)
    
    if(length(predictedtxt)==0){
      
      outputtxt <- paste(tail(inputtxt, 1), collapse = " ")
      predictedtxt<- bigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=bigram$word, ignore.case = T)]
      guess_word <- word(predictedtxt[1],-1)
      
      if(length(predictedtxt)==0){
        guess_word<-"the"
      }
    }
  } else if (length(inputtxt)==1) {
    
    outputtxt <- paste(inputtxt, collapse = " ")
    predictedtxt<- bigram$word[grepl(pattern= paste("^",outputtxt, sep=""), x=bigram$word, ignore.case = T)]
    guess_word <- word(predictedtxt[1],-1)
    
    if(length(predictedtxt)==0){
      guess_word <-"the"
    }
  } 
  paste(guess_word)
 }
  
 