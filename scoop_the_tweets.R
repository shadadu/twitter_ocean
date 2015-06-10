scoop_the_tweets<-function(k,file_names,mdfcolnames){
  
  
  filepath<-paste0("Ocean Social Data/",file_names[k])
  ob<-stream_in(file(filepath))
  
  if(is.null(ob$interaction$author$id)){
  ids<-NA  
  }else{
  ids<-ob$interaction$author$id
  maxL<-length(ids)
  }
  
  if(is.null(ob$interaction$author$username)){
    usernames<-NA  
  }else{
    usernames<-ob$interaction$author$username
    if(maxL < length(usernames))
    {maxL=length(usernames)}
  }
  
  if(is.null(ob$demographic)){
    genders<-NA  
  }else{
    genders<-ob$demographic
    if(maxL < length(genders))
    {maxL=length(genders)}
  }
  
  if(is.null(ob$interaction$content)){
    words<-NA  
  }else{
    words<-ob$interaction$content
    if(maxL < length(words))
    {maxL=length(words)}
  }
  
  if(is.null(ob$klout$score)){
    klouts<-NA  
  }else{
    klouts<-ob$klout$score
    if(maxL < length(klouts))
    {maxL=length(klouts)}
  }
  
  if(is.null(ob$salience$content$sentiment)){
    sentiments<-NA  
  }else{
    sentiments<-ob$salience$content$sentiment
    if(maxL < length(sentiments))
    {maxL=length(sentiments)}
  }
  
  ### append 'short' vectors with NAs ; should be rare occurrence
  
  if(length(ids)<maxL){
    sl<-length(ids)
    ids<-c(ids,c(rep(NA,(maxL-sl))))
  }
  
  
  if(length(usernames)<maxL){
    sl<-length(usernames)
    usernames<-c(usernames,c(rep(NA,(maxL-sl))))
  }
  
  if(length(genders)<maxL){
    sl<-length(genders)
    genders<-c(genders,c(rep(NA,(maxL-sl))))
  }
  
  if(length(words)<maxL){
    sl<-length(words)
    words<-c(words,c(rep(NA,(maxL-sl))))
  }
  
  if(length(klouts)<maxL){
    sl<-length(klouts)
    words<-c(klouts,c(rep(NA,(maxL-sl))))
  }
  
  if(length(sentiments)<maxL){
    sl<-length(sentiments)
    sentiments<-c(sentiments,c(rep(NA,(maxL-sl))))
  }
  
  #rnames<-ob$interaction$author$name
  
    
  
  
  words<-iconv(words,"latin1", "ASCII", sub="byte")
    
  df<-data.frame(ids,usernames,words,klouts,sentiments)
  
  df2<-by_username(df,ob,mdfcolnames)
 
  return(df2)
  
  
} # closing brace for function scoop_the_tweets