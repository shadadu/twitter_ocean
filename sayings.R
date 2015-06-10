sayings<-function(tdata){
  
  corpus<-Corpus(VectorSource(tdata))
  corpus<-tm_map(corpus,stripWhitespace)
  corpus<-tm_map(corpus,content_transformer(tolower))
  corpus<-tm_map(corpus,PlainTextDocument)
  
  corpus<-tm_map(corpus,removeWords,stopwords("english"))
  
  corpus<-tm_map(corpus,stemDocument)
  tdm<-TermDocumentMatrix(corpus)
  
  #inspect(tdm)
  #print(dimnames(tdm)$Terms[1:5])
  return(corpus)
    
}