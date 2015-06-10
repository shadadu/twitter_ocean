makefeatures<-function(freqz){
  
  # mood, engaging, help_source, 
  cuss_words<-c("fuck","f**k","f@#k","f#@k","shit","sh*t","s**t","sh#t","shyte","damn","hell")
  excited_words<-c("omg","awesome","gosh", "omfg")
  #anger_words<-
  eros_words<-c("romance","love","erotic","desire","fire ")
  money_words<-c("cash","dollar","money","owe","own","pay","buy","stock","invest","business","earn",
                 "credit","reward") #count pound or dollar signs
  encourage_words<-c("dream","future","success")
  religion_words<-c("church","god","bible","jesus","mohamed","buddha","mosque","meditate","pray","altar","temple")
  activist_words<-c("fight","human","aid","help","donate","give","freedom","justice","equality","rights","share")
  news_words<-c("news","today","headlines")
  politics_words<-c("congress","politics","democrat","gop","republican","country","nation","senate","president")
  laugh_words<-c("lol","haha","lolol","hahaha","laugh","smile","lmao","lmfao")
  negations<-c("not","never","no","neither")
  #measure me, you; I, you correlation for a user to indicate level of engagement with others
  negative_emotions<-c("sad","grief","sorrow","condolence","dystopia")
  bored_emotions<-c("bleh","meh","duh")
  short_words<-c("u","ur","r","cum")
  point_words<-c("")
  engaging_words<-c("us","we")
  you_words<-c("your","thou","thine","thy")
  quantifier_words<-c("few","many","much","plenty")
  family_words<-c("child","father","mother","son","daughter","grandma","grandpa")
  
  nobs<-function(x,freqz){
    if(x %in% names(freqz)){
      return(freqz[x])
    }else{return(0)} 
  } 
  
  countfeature<-function(wordsfeature,freqz){
    
   s<-sapply(wordsfeature,nobs,freqz)
   return(sum(s,na.rm=TRUE))
      
  }
  
  cuss<-countfeature(cuss_words,freqz)
  excited<-countfeature(excited_words,freqz)
  eros<-countfeature(eros_words,freqz)
  money<-countfeature(money_words,freqz)
  
  wordvars<-c(cuss,excited,eros,money)

  
  
  return(wordvars)
}