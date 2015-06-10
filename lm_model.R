lm_model<-function(Y,X){
  
  # simple linear regression model
agreeable.model<-lm(Y[,"avgAgreeableness"]~ X)
conscient.model<-lm(Y[,"avgConscientiousness"]~ X)
extraversion.model<-lm(Y[,"avgExtraversion"]~ X)
neurotic.model<-lm(Y[,"avgNeuroticism"]~ X)
open.model<-lm(Y[,"avgOpennessIntellect"]~ X)


agreeable.predict <-predict(agreeable.model, X)
conscient.predict <-predict(conscient.model, X)
extraversion.predict <-predict(extraversion.model, X)
neurotic.predict <- predict(neurotic.model, X)
open.predict <- predict(open.model, X)

predictData<-data.frame(Y[,"ids"],Y[,"usernames"],agreeable.predict, conscient.predict,
                        extraversion.predict, neurotic.predict, open.predict)
write.csv(predictData, file = "predicted_ocean_scores_for_user_id.csv")

outY<- list(agreeable.model, conscient.model, extraversion.model, neurotic.model, open.model)
  
} 