by_username<-function(df1,ob,mdfcolnames){         
  
  # user_tweets is a vector collecting all the words/text per username  
  user_tweets<-tapply(df1[,"words"],df1[,"usernames"], function(x) paste(x,collapse=" "))
  uniqnames<-names(user_tweets)       # gives the unique usernames
  idoccurs<-aggregate(df1[,"ids"],by=list(df1[,"ids"]), FUN=length)
  names(idoccurs)<-c("ids","number.tweets")
  
  user_avg_sentiment<-tapply(df1[,"sentiments"],df1[,"usernames"], function(x) mean(x,na.rm=TRUE))
  user_avg_klout<-tapply(df1[,"klouts"],df1[,"usernames"], function(x) mean(x,na.rm=TRUE))
  user_ids<-tapply(df1[,"ids"],df1[,"usernames"], function(x) x )
  
  rm(list=c("df1","ob"))
  rowlength=length(uniqnames)
  collength=9
    
  df1<-data.frame(matrix(c(rep(NA,rowlength*collength)),nrow=rowlength,ncol=collength),row.names=uniqnames)
  
  names(df1)<-mdfcolnames 
  for(i in seq_along(uniqnames)){                       # saves model's variables into dataframe by corresponding 
    df1[uniqnames[i],"ids"]<-user_ids[uniqnames[i]]                 # userids; the dataframe's label is then usernames on rows 
    df1[uniqnames[i],"words"]<-user_tweets[uniqnames[i]]     # and variables on columns
    df1[uniqnames[i],"klouts"]<-user_avg_klout[uniqnames[i]]
    df1[uniqnames[i],"sentiments"]<-user_avg_sentiment[uniqnames[i]]
    df1[uniqnames[i],"tweets.by"]<-idoccurs[uniqnames[i],"number.tweets"]
    df1[uniqnames[i],"usernames"]<-uniqnames[i]
        
  }
 
num_hashtags<-tapply(df1[,"words"],df1[,"usernames"], function(x) str_count(x,"#")/length(strsplit(x, " ")) ) 
num_links<-tapply(df1[,"words"],df1[,"usernames"], function(x) str_count(x,"http")/length(strsplit(x, " ")) ) 
num_mentions<-tapply(df1[,"words"],df1[,"usernames"], function(x) (str_count(x,"@")/length(strsplit(x, " ")) ) )

for(i in seq_along(uniqnames)){
  df1[uniqnames[i],"hashtags"]<-num_hashtags[uniqnames[i]]
  df1[uniqnames[i],"links"]<-num_links[uniqnames[i]]
  df1[uniqnames[i],"mentions"]<-num_mentions[uniqnames[i]]
  
}


df1
} # closing brace for by_username function
