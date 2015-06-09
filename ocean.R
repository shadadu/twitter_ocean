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
  source("gpmodel.R")
  source("predictmodel.R")
  
  file_names <-list.files("Ocean Social Data")  # collect the names of the 3211 files in this folder
  
  # the variables of the model as column vectors variables will be in this mdf data frame
  mdfcolnames<-c("ids","usernames", "words","klouts","sentiments","tweets.by","hashtags","links","mentions")  
  mdf<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA)                              
  # for(n in 1:(length(mdfcolnames)) ){
  #  mdf<-cbind(mdf,NA)
  #}
  names(mdf)<-mdfcolnames  
  
  
  for(k in 3000:3005){
    print(k)
    print(file_names[k])
    adf<-scoop_the_tweets(k,file_names,mdfcolnames)
    print(dim(adf))
    print("binding")
    mdf<-rbind(mdf,adf)
    print(dim(mdf))
  }
  
  mdf<-by_username(mdf,ob,mdfcolnames)
  
  dfs<-scoop_the_survey()
  
  
  
  #usrnames<-row.names(mdf)
  
  mergedata<-merge(mdf,dfs)
  
  rownames(mergedata)<-make.names(mergedata[,"usernames"], unique=TRUE)
  
  
  L<-length(row.names(mergedata))
  
  #(mergedata[,"usernames"])
  #str(mergedata)
  #for(i in seq_along(usrnames) ){
  # for(j in seq_along(mdfcolnames)){
  #  dfs[ids[i],bigdfcolnames[j]]<-mdf[usrnames[i],mdfcolnames[j]]
  #}
  #}
  
  corpus.list<-lapply(mergedata[,"words"],sayings)
 
  for(j in )
  freqz<-corpus.list[[1]][[1]] 
  aka<-(termFreq(freqz))
  
  
    
  #wordvar_matrix <-makefeatures(freqz)
  
  
  
  #for(j in 2:L){
    
   # freqz<-as.data.frame(inspect(corpus.list[[j]]))
    #wordvar<-sapply(freqz, makefeatures)
    #wordvar_matrix<-rbind(wordvar_matrix,wordvar)
    
  #}
  #rownames(wordvar_matrix)<-row.names(mergedata)
  #str(wordvar_matrix ) 
  #names(wordvar_matrix)<-row.names(corpus.list)
  #str(mergedata)
  
  #remove NAs from mergedata
  #convert mergedata to matrix
  #print(mergedata$avgAgreeableness)
  Y<-data.frame(mergedata$avgAgreeableness, mergedata$avgConscientiousness, mergedata$avgExtraversion,
             mergedata$avgNeuroticism, mergedata$avgOpennessIntellect)
  
  

  Y<-as.matrix(Y)
  
  colnames(Y)<-c("avgAgreeableness", "avgConscientiousness", "avgExtraversion",
                "avgNeuroticism", "avgOpennessIntellect")

   
  names(aka)
          # freqz
 # names(freqz)
  #X<-matrix(mergedata$klouts, mergedata$sentiments, mergedata$mentions, mergedata$hashtags, 
            # mergedata$links, mergedata$tweets.by,  )
  #rownames(X)<-row.names(mergedata)
  
  #outY<- gpmodel(Y,X)
  
  #pY<-predictmodel(outY,X)   # use the learned model to predict the ocean values 
  # for the users in the supplied (training) data
  
  
  
  
  #return(length((bigdf[,"user_names"] %in% dfs[,"usrnames_survey"])))
  
  
  
} #closing brace for function ocean
