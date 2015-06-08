scoop_the_tweets<-function(k,file_names,mdfcolnames){
  
  
  filepath<-paste0("Ocean Social Data/",file_names[k])
  ob<-stream_in(file(filepath))
  
  ids<-ob$interaction$author$id
  usernames<-ob$interaction$author$username
  noms<-ob$interaction$author$name
  genders<-ob$demographic
  words<-ob$interaction$content
  klouts<-ob$klout$score
  sentiments<-ob$salience$content$sentiment
  topicwords<-ob$salience$content$topics 
  
  words<-iconv(words,"latin1", "ASCII", sub="byte")
  #corpus<-Corpus(VectorSource(words))   
  #corpus<-tm_map(corpus,stripWhitespace)
  #corpus<-tm_map(corpus,content_transformer(tolower))
  #corpus<-tm_map(corpus,PlainTextDocument)
  #corpus<-tm_map(corpus,removeWords, stopwords("english"))
  #corpus<-tm_map(corpus,tokenize)
  #corpus<-tm_map(corpus,stemDocument, language="english")
  
  df<-data.frame(ids,usernames,words,klouts,sentiments)
  
  df2<-by_username(df,ob,mdfcolnames)
  #  print(head(df)) 
  #  str(df)
  return(df2)
  
  
} # closing brace for function scoop_the_tweets