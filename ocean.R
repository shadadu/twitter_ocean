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
  source("lm_model.R")
  
  file_names <-list.files("Ocean Social Data")  # collect the names of the 3211 files in this folder
  
  # the variables of the model as column vectors variables will be in this mdf data frame
  mdfcolnames<-c("ids","usernames", "words","klouts","sentiments","tweets.by","hashtags","links","mentions")  
   
  mdf<-data.frame(NA)
   for(n in 2:(length(mdfcolnames)) ){
    mdf<-cbind(mdf,NA)
  }
  names(mdf)<-mdfcolnames  
  
  
  for(k in seq_along(file_names)){
    print(k)
    adf<-scoop_the_tweets(k,file_names,mdfcolnames)
        
    mdf<-rbind(mdf,adf)
   
  }
  
  mdf<-by_username(mdf,ob,mdfcolnames)
  
  dfs<-scoop_the_survey()
  
    
  mergedata<-merge(mdf,dfs)
  
  mergedata<-mergedata[complete.cases(mergedata),]
  
  rownames(mergedata)<-make.names(mergedata[,"usernames"], unique=TRUE)
  
  
  
  L<-length(rownames(mergedata))
  
  #for(i in seq_along(usrnames) ){            # this nested loop could be used to merge mdf and dfs
  # for(j in seq_along(mdfcolnames)){
  #  dfs[ids[i],bigdfcolnames[j]]<-mdf[usrnames[i],mdfcolnames[j]]
  #}
  #}
  
  corpus.list<-lapply(mergedata[,"words"],sayings)
  
  freqzlist<-corpus.list[[1]][[1]] 
  freqz<-(termFreq(freqzlist))
  wordvar_matrix <-makefeatures(freqz)
 
  for(j in 2:L){
  freqzlist<-corpus.list[[j]][[1]] 
  freqz<-(termFreq(freqzlist))
  wordvar<-makefeatures(freqz)
  wordvar_matrix<-rbind(wordvar_matrix,wordvar)
  }
  rownames(wordvar_matrix)<-rownames(mergedata)
   
  #names(wordvar_matrix)<-row.names(corpus.list)
  #str(mergedata)
  
  #remove NAs from mergedata
  
  Y<-data.frame(mergedata$ids, mergedata$usernames, mergedata$avgAgreeableness, mergedata$avgConscientiousness,
                mergedata$avgExtraversion, mergedata$avgNeuroticism, mergedata$avgOpennessIntellect)
  
  

  Y<-as.matrix(Y)
  
  
  
  colnames(Y)<-c("ids","usernames","avgAgreeableness", "avgConscientiousness", "avgExtraversion",
                "avgNeuroticism", "avgOpennessIntellect")
rownames(Y)<-rownames(mergedata)
   
  
    head(Y)
  X<-data.frame(mergedata$klouts, mergedata$sentiments, mergedata$mentions, mergedata$hashtags, mergedata$links)
X<-as.matrix(X)
X<-cbind(X,wordvar_matrix)
head(X)

  #rownames(X)<-row.names(mergedata)
  
  ans<- lm_model(Y,X)
   print(length(rownames(mdf)))
  print(length(rownames(mergedata)))
  #pY<-predictmodel(outY,X)   # use the learned model to predict the ocean values 
                              # for the users in the supplied (training) data
  
  print(summary(ans[[1]]))
  print(summary(ans[[2]]))
  print(summary(ans[[3]]))
  print(summary(ans[[4]]))
  print(summary(ans[[5]]))
  
  return(length((mdf[,"usernames"] %in% dfs[,"usernames"])))
  
  
  
} #closing brace for function ocean
