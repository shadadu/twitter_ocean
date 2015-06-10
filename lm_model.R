lm_model<-function(Y,X){
  
  # simple linear regression model
agreeable.model<-lm(Y[,"avgAgreeableness"]~ X)
conscient.model<-lm(Y[,"avgConscientiousness"]~ X)
extraversion.model<-lm(Y[,"avgExtraversion"]~ X)
neurotic.model<-lm(Y[,"avgNeuroticism"]~ X)
open.model<-lm(Y[,"avgOpennessIntellect"]~ X)

# predict OCEAN scores for users in the survey data
agreeable.predict <-predict(agreeable.model, data=data.frame(X))
conscient.predict <-predict(conscient.model, data=data.frame(X))
extraversion.predict <-predict(extraversion.model, data=data.frame(X))
neurotic.predict <- predict(neurotic.model, data=data.frame(X))
open.predict <- predict(open.model, dat=data.frame(X))

predictData<-data.frame(Y[,"ids"],Y[,"usernames"],agreeable.predict, conscient.predict,
                        extraversion.predict, neurotic.predict, open.predict)

names(predictData)<-c("ids","usernames","agreeableness","conscientiousness","extraversion"
                      ,"neuroticism","opennessIntellect")
rownames(predictData)<-Y[,"ids"]

dput(predictData, "surf_ocean")

outY<- list(agreeable.model, conscient.model, extraversion.model, neurotic.model, open.model)
  
} 