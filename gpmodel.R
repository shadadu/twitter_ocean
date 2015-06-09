gmodel<-function(Y,X){
  
  agree<-gpr(Y$avgAgreeableness, X)
  conscient<-gpr(Y$avgConscientiousness, X)
  extraverse<-gpr(Y$avgExtraversion, X)
  neurotic<-gpr(Y$avgNeuroticism, X)
  open<-gpr(Y$avgOpennessIntellect, X)
  
  a<-list(agree, conscient, extraverse, neurotic, open)
  
  
}