library(utils)
library(remotes)
library(stringr)
library(mboxr)
library(qdapRegex)
library(qdapTools)
library(textstem)
library(tm)
library(easyPubMed)

#Change the Working Directory to current place
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Read the data from the .mbox file to a dataframe 
data <- tryCatch({read_mbox("data.mbox")},
                 error = function(e){print(e)}) 
#print("Data Imported")

#Select just the date, from, subject and content columns from the dataframe
data <- data[c("date","from","subject","content")]
#print("Columns Filtered")

#Extract the name of the sender(customize it according to your needs)
x<- rm_between(data[["from"]], "'", "'", extract=TRUE)
from_name <- c()
for(i in 1:length(x)){
  if(is.na(x[[i]][[1]]) == FALSE){
    t <- x[[i]][[1]]
  }else{
    t <- unlist(strsplit(data[["from"]][[i]], split='<', fixed=TRUE))[[1]]
    t <- removeNumbers(gsub("\r?\n|\r|[[:punct:]]", "", t))
    t <- str_replace_all(t, "[^[:alpha:]]", "")
    t <- stripWhitespace(t)
    t <- unlist(strsplit(t, split=" "))
    d <- c()
    for(j in t){
      if(str_length(j) < 15){
        d <- append(d,j)
      }
    }
    d <- str_c(d,collapse=' ')
    d <- lemmatize_words(d)
    t <- NULL
    t <- stripWhitespace(d)
  }
  if(t == ""){
    t <- "nil"
  }
  from_name <- append(from_name,tolower(str_replace_all(t, "[^[:alpha:]]", "")))
}
#print("Names Extracted")

#Extract the sender emails from the 'from' column
y <- rm_between(data[["from"]], '<', '>', extract=TRUE)
from_email <- c()
for(i in 1:length(y)){
  from_email <- append(from_email,tolower(y[[i]][[1]]))
}

#PreProcess the Subject: 
#       Remove \n, \r and other punctuation marks
#       Remove Numbers
#       Convert into lowercase
#       Remove Words less then 3 letters
#       Remove Extra Spaces
#       Lemmatize the string
subject <- removeNumbers(gsub("\r?\n|\r|[[:punct:]]", " ", data[["subject"]]))
subject <- str_replace_all(subject, "[^[:alpha:]]", " ")
subject <- tolower(subject)
subject <- lemmatize_strings(stripWhitespace(gsub('\\b\\w{1,2}\\b','',subject)))
content <- c()

#PreProcess the Content: 
#       Remove html code
#       Remove \n, \r and other punctuation marks
#       Remove Numbers
#       Remove Words less then 3 letters
#       Remove Extra Spaces
#       Remove Words greater than equal to 15 letters
#       Note: A single cell in Excel can only contain 32,767
#       If the content is more than 32000 characters then filter out duplicate words
#       If still the content is more than 32000 characters then truncate the characters after 32000
#       Convert into lowercase
#       Remove Extra Spaces
#       Lemmatize the string
for(i in 1:nrow(data)){
  z <- gsub("<.*?>", "", data[["content"]][[i]])
  z <- removeNumbers(gsub("\r?\n|\r|[[:punct:]]", "", z))
  z <- gsub('\\b\\w{1,2}\\b','',z)
  z <- str_replace_all(z, "[^[:alpha:]]", " ")
  z <- stripWhitespace(z)
  z <- unlist(strsplit(z, split=" "))
  d <- c()
  for(j in z){
    if(str_length(j) < 15){
      d <- append(d,j)
    }
  }
  d <- str_c(d,collapse=' ')
  d <- lemmatize_words(d)
  if(str_length(d)>32000){
    k <- NULL
    k <- unlist(strsplit(d, split=" "))
    d <- NULL
    d <- paste0(unique(k), collapse = ' ')
  }
  if(str_length(d)>32000){
    d <- substring(d,1,32000)
  }
  d <- tolower(d)
  d <- stripWhitespace(d)
  content <- append(content,d)
  #print(paste("row :",i," done: ",str_length(d)))
}

#Assign all the labels to either Yes/No. 
Label_Spam <- rep(c("no"), nrow(data))

#Create a new dataframe that merges all the above talked above elements
df <- as.data.frame(list( date = data[["date"]] ,fromName = from_name, 
                          fromEmail = from_email, subject = subject,
                          content = content, Label_Spam = Label_Spam))

#Write the dataframe into a .csv file
write.csv(df, file = "naivebayes.csv", col.names = TRUE,row.names = FALSE)
