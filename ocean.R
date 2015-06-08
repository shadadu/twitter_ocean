ocean<-function(){
  
  rm(list=ls())
  require(tm)
  require(stringr)
  require(GPFDA)
  require(jsonlite)
  source("scoop_the_tweets.R")
  source("by_username.R")
  source("scoop_the_survey.R")
  source("sayings.R")
  source("makefeatures.R")

  file_names <-list.files("Ocean Social Data")  # collect the names of the 3211 files in this folder
  
  # the variables of the model as column vectors variables will be in this mdf data frame
  mdfcolnames<-c("ids","usernames", "words","klouts","sentiments","#tweets.by","#hashtags","#links","@mentions")  
  mdf<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA)                              
  # for(n in 1:(length(mdfcolnames)) ){
  #  mdf<-cbind(mdf,NA)
  #}
  names(mdf)<-mdfcolnames  
  
    
  for(k in 3000:3100){
    print(k)
    print(file_names[k])
    adf<-scoop_the_tweets(k,file_names,mdfcolnames)
    mdf<-rbind(mdf,adf)
  }
    
  mdf<-by_username(mdf,ob,mdfcolnames)
  
  dfs<-scoop_the_survey()
  
   
  #usrnames<-row.names(mdf)
  
  mergedata<-merge(mdf,dfs)

  #(mergedata[,"usernames"])
  #str(mergedata)
  #for(i in seq_along(usrnames) ){
   # for(j in seq_along(mdfcolnames)){
    #  dfs[ids[i],bigdfcolnames[j]]<-mdf[usrnames[i],mdfcolnames[j]]
    #}
  #}
  #corpus.list<-lapply(mergedata[,"words"],sayings)
  #j=10
  #dimnames(corpus.list[[j]])$Terms
 # inspect(corpus.list[]
 corpus.list<-lapply(mergedata[,"words"],sayings)
 wordvar.list<-data.frame(NA)
 for(j in seq_along(row.names(mergedata))){
 freqz<-as.data.frame(inspect(corpus.list[[j]]))
 
 wordvar.list[j]<-lapply(freqz, makefeatures)
  
 }
 str(wordvar.list ) 
  #names(wordvar.vector)<-row.names(corpus.list)
 # str(mergedata)
  
 #remove NAs from mergedata
 #convert mergedata to matrix
 yy<-"avgOpennessIntellect"
 Y<-matrix(mergedata$avgAgreeableness, mergedata$avgConscientious, mergedata$avgExtraversion,
           mergedata$avgNeuroticism, mergedata$avgOpennessIntellect)
 colnames(Y)<-("avgAgreebleness", "avgConscientious", "avgExtraversion",
               "avgNeuroticism", "avgOpennessIntellect")
           
 X<-matrix(mergedata$klouts, mergedata$sentiments, mergedata$mentions, mergedata$hashtags, 
           mergedata$links, mergedata$tweets.by )
 
 outY<- gpmodel(Y,X)
 
 
 
 
  
  #return(length((bigdf[,"user_names"] %in% dfs[,"usrnames_survey"])))
  
  
  
} #closing brace for function ocean
