serve_ocean<-function(userid, trait=FALSE){
  #
  #
  #65663675
  indata<-dget("fzoo")
 
  if(!(userid %in% indata[,"ids"]){
            stop("The id you provided is not in our database ")
    }
    else{
      print(paste0("USER ID is valid: ",userid)
      
      if(trait==FALSE){
        # spit out scores on all the five personality attributes
        print(paste0(trait," ","OCEAN score: "))
        print(indata[userid,trait])
      }
      else{
        # spit out the score for personality attribute 
        print("Agreeableness OCEAN score: ") 
        print(indata[userid,"agreeableness"])
        print("Conscientiousness OCEAN score: ")
        print(indata[userid,"conscientiousness"])
        print("Extraversion OCEAN score: ")
        print(indata[userid,"extraversion"])
        print("Neuroticism OCEAN score: ")
        print(indata[userid,"neuroticism"])
        print("OpennessIntellect OCEAN score: ")
        print(indata[userid,"openness"])
      }
      
    }

 
  
  
  
  
}