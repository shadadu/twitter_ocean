scoop_the_survey<-function(){
  con<-file("Ocean Survey Data.txt","r")
  surveysdata<-readLines(con)
  close(con)
  surveys<-gsub(","," ",surveysdata)
  surveys<-gsub("\t",",",surveys)  # convert surveys to comma separated variables
  write(surveys,"foo.csv") # 
  #rm(list=ls("surveys")) # free the memory
  surveys<-read.csv("foo.csv", header=TRUE)
  usrnames_survey<-surveys[,"s7r2oe"]
  agreeable<-surveys[,"avgAgreeableness"]
  conscientious<-surveys[,"avgConscientiousness"]
  extraversion<-surveys[,"avgExtraversion"]
  neuroticism<-surveys[,"avgNeuroticism"]
  openness<-surveys[,"avgOpennessIntellect"] 
  
  dfs<-data.frame(usrnames_survey,agreeable,conscientious,extraversion,neuroticism,openness)
  names(dfs)<-c("usernames", "avgAgreeableness", "avgConscientiousness", "avgExtraversion", "avgNeuroticism", 
                "avgOpennessIntellect")
  
  
  dfs
}