ocean<-function(){
  library(tm)
  library(gptk)
  library(jsonlite)
  file_names <-list.files("Ocean Social Data")
  
  bigdfcolnames<-c("user_names","words","klouts","saliences")
  bigdf<-data.frame(NA,NA,NA,NA)
  # for(n in 1:(length(bigdfcolnames)) ){
  #  bigdf<-cbind(bigdf,NA)
  #}
  names(bigdf)<-bigdfcolnames  
  
  
  nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
  }
  
  by_username<-function(df1,ob){
      
    user_tweets<-tapply(df1[,"words"],df1[,"user_names"], function(x) paste(x,collapse=" "))
    uniqnames<-names(user_tweets)
    # print(uniqnames)
    user_avg_salience<-tapply(df1[,"saliences"],df1[,"user_names"], function(x) mean(x,na.rm=TRUE))
    user_avg_klout<-tapply(df1[,"klouts"],df1[,"user_names"], function(x) mean(x,na.rm=TRUE))
    rm(list=c("df1","ob"))
    rowlength=length(uniqnames)
    collength=4
    
    
    df1<-data.frame(matrix(c(rep(NA,rowlength*collength)),nrow=rowlength,ncol=collength),row.names=uniqnames)
    
    names(df1)<-bigdfcolnames 
    for(i in seq_along(uniqnames)){
      df1[uniqnames[i],"user_names"]<-uniqnames[i]
      df1[uniqnames[i],"words"]<-user_tweets[uniqnames[i]]
      df1[uniqnames[i],"klouts"]<-user_avg_klout[uniqnames[i]]
      df1[uniqnames[i],"saliences"]<-user_avg_salience[uniqnames[i]]
    }
    df1
  } # closing brace for by_username function
  
  scoop_the_tweets<-function(k){
    filepath<-paste0("Ocean Social Data/",file_names[k])
    ob<-stream_in(file(filepath))
    
    ids<-ob$interaction$author$id
    user_names<-ob$interaction$author$username
    noms<-ob$interaction$author$name
    genders<-ob$demographic
    words<-ob$interaction$content
    klouts<-ob$klout$score
    saliences<-ob$salience$content$sentiment
    topicwords<-ob$salience$content$topics 
    genders<-sapply(genders,nulltoNA)
    
    
    words<-iconv(words,"latin1", "ASCII", sub="byte")
    corpus<-Corpus(VectorSource(words))   
    corpus<-tm_map(corpus,stripWhitespace)
    corpus<-tm_map(corpus,content_transformer(tolower))
    corpus<-tm_map(corpus,PlainTextDocument)
    corpus<-tm_map(corpus,removeWords, stopwords("english"))
    #corpus<-tm_map(corpus,tokenize)
    #corpus<-tm_map(corpus,stemDocument, language="english")
    
    df<-data.frame(ids,user_names,noms,genders,words,klouts,saliences)
        
    df=by_username(df,ob)
    #  print(head(df)) 
    #  str(df)
    return(df)
    
    
  } # closing brace for function scoop_the_tweets
  
  scoop_survey<-function(){
    con<-file("Ocean Survey Data.txt","r")
    surveysdata<-readLines(con)
    close(con)
    surveys<-gsub(","," ",surveysdata)
    surveys<-gsub("\t",",",surveys)  # convert surveys to comma separated variables
    write(z,"foo.csv") # 
    #rm(list=ls("surveys")) # free the memory
    surveys<-read.csv("foo.csv", header=TRUE)
    usrnames_survey<-surveys[,"s7r2oe"]
    agreeable<-surveys[,"avgAgreeableness"]
    conscientious<-surveys[,"avgConscientiousness"]
    extraversion<-surveys[,"avgExtraversion"]
    neuroticism<-surveys[,"avgNeuroticism"]
    openness<-surveys[,"avgOpennessIntellect"] 
    
    dfs<-data.frame(usrnames_survey,agreeable,conscientious,extraversion,neuroticism,openness)
    names(dfs)<-c("user_names", "avgAgreeableness", "avgExtraversion", "avgNeuroticism", "avgOpennessIntellect")
    dfs
  }
  
  
  
  for(k in 1:10){
    print(k)
    print(file_names[k])
    adf=scoop_the_tweets(k)
    #print(dim(adf)) 
    # print("binding")
    #print(dim(bigdf))
    bigdf<-rbind(bigdf,adf)
  }
  bigdf<-by_username(bigdf,ob)
  str(bigdf) 
  dfs<-scoop_survey()
 
  
  usrnames<-row.names(bigdf)
  bigdfcols<-names(bigdf)
  
  for(i in seq_along(usrnames) ){
    for(j in seq_along(bigdfcols)){
      dfs[usrnames[i],bigdfcols[j]]<-bigdf[usrnames[i],bigdfcols[j]]
    }
  }
  
  
  return(length((bigdf[,"user_names"] %in% dfs[,"usrnames_survey"])))
  
  
  
} #closing brace for function ocean
