
########################### Xènia Nicolau Casademont- 1563614 ###############################
################################ SCRIPT ####################################


source("C:/Users/xènia/Desktop/gea/4tcurs/tfg/funcions.R")#cridem a l'script de funcions

library(bnlearn)
require(gRbase)
library(gRain)
require(ggplot2)
library(ggplot2)
require(lattice)
library(caret)

setwd("C:/Users/xènia/Desktop/gea/4tcurs/tfg")
library(readr)
dades <- read_csv("dades.csv")
colnames(dades)<-c('Death','F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','LOS','F11',
                   'F12','F13','F14','F15','F16','F17','F18','F19','F20','F21','F22','F23','F24','F25',
                   'F26','F27','F28','F29','F30','F31','F32','F33')

#View(dades)

######################################################preproces

dades <- as.data.frame(lapply(dades, as.factor))# per treballar amb xarxes bayesianes les variables han de ser factors
apply(dades,2,table)


################################XARXA BAYESIANA I K-FOLD CROSS-VALIDATION###########################

#####BINARY RELEVANCE: augmented naive DEATH
dades<-as.data.frame(dades)
atributes=colnames(dades[-c(1,12)])
bl=data.frame(from=atributes, to = rep("Death",length(atributes))) # black list
wl=data.frame(from=rep("Death",length(atributes)), to = atributes) # white list
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
set.seed(1)
folds <- createFolds(dades$Death, k = 10)
results <- lapply(folds, function(x) {
  train <- dades[-x, ]
  test <- dades[x, ]
  confusion.matrix.Death.BR(train,test)
})
accs<-(results);accs
matrius.augmented.death.BR<-accs
matrius.augmented.death.BR<-arreglar.matriu.death2(matrius.augmented.death.BR)


#####BINARY RELEVANCE: Naive Bayes  DEATH
dades<-as.data.frame(dades)
atributes=colnames(dades[-c(1,12)])
bl=data.frame(from=atributes, to = rep(atributes, each=length(atributes)))
wl=data.frame(from=rep("Death",length(atributes)), to = atributes)  
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
set.seed(1)
folds <- createFolds(dades$Death, k = 10)
results <- lapply(folds, function(x) {
  train <- dades[-x, ]
  test <- dades[x, ]
  confusion.matrix.Death.BR(train,test)
})
accs<-(results);accs
matrius.naive.death.BR<-accs
matrius.naive.death.BR<-arreglar.matriu.death2(matrius.naive.death.BR)

#####BINARY RELEVANCE: augmented naive LOS
dades<-as.data.frame(dades)
atributes=colnames(dades[-c(12,1)])
bl=data.frame(from=atributes, to = rep("LOS",length(atributes))) # black list
wl=data.frame(from=rep("LOS",length(atributes)), to = atributes) # white list
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
set.seed(1)
folds <- createFolds(dades$LOS, k = 10)
results <- lapply(folds, function(x) {
  train <- dades[-x, ]
  test <- dades[x, ]
  confusion.matrix.los.BR(train,test)
})
accs<-(results);accs
matrius.augmented.los.BR<-accs
matrius.augmented.los.BR<-arreglar.matriu.los2(matrius.augmented.los.BR)


#####BINARY RELEVANCE: Naive Bayes LOS
dades<-as.data.frame(dades)
atributes=colnames(dades[-c(12,1)])
bl=data.frame(from=atributes, to = rep(atributes, each=length(atributes)))
wl=data.frame(from=rep("LOS",length(atributes)), to = atributes)  
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
set.seed(1)
folds <- createFolds(dades$LOS, k = 10)
results <- lapply(folds, function(x) {
  train <- dades[-x, ]
  test <- dades[x, ]
  confusion.matrix.los.BR(train,test)
})
accs<-(results);accs
matrius.naive.los.BR<-accs
matrius.naive.los.BR<-arreglar.matriu.los2(matrius.naive.los.BR)


####UNA ÚNICA XARXA: diagnosi
dades<-as.data.frame(dades)
atributes=colnames(dades[-c(1,12)])
bl=data.frame(from=atributes, to = c(rep("Death",length(atributes)),rep("LOS",length(atributes))))# black list
prediccio.Death=NULL
prediccio.LOS=NULL
CL.Death=NULL
CL.LOS=NULL
prueba.Death=NULL
prueba.LOS=NULL
distribucio.Death=NULL
distribucio.LOS=NULL
set.seed(1)
library(caret)
folds <- createFolds(dades$Death, k = 10)
results <- lapply(folds, function(x) {
  train <- dades[-x, ]
  test <- dades[x, ]
  confusion.matrix.una.xarxa(train,test)
})
accs<-(results);accs
matrius.death.una.xarxa<-lapply(accs, obtenir_primer_valor)
matrius.death.una.xarxa<-lapply(matrius.death.una.xarxa,arreglar.matriu.death2)
matrius.los.una.xarxa<-lapply(accs, obtenir_segon_valor)
matrius.los.una.xarxa<-lapply(matrius.los.una.xarxa, arreglar.matriu.los2)

model_explicatiu<-lapply(folds, function(x) {
  train <- dades[-x, ]
  test <- dades[x, ]
  pintar_grafics(train,test)
})


#####CHAIN CLASSIFIER: DEATH-->LOS augmented naive

dades<-as.data.frame(dades)
atributes=colnames(dades[-c(1,12)])
bl=data.frame(from=atributes, to = rep("Death",length(atributes))) # black list
wl=data.frame(from=rep("Death",length(atributes)), to = atributes) # white list
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
library(caret)
library(bnlearn)
set.seed(1)
folds <- createFolds(dades$Death, k = 10)
results <- lapply(folds, function(x) {
  train <- dades[-x, ]
  test <- dades[x, ]
  pred<-confusion.matrix.death_los.CC(train,test)[[2]]
  pred_df<-factor(pred)
  new_data<-cbind(test, "Death_prediction"=pred_df)
  list(new_data,confusion.matrix.death_los.CC(train,test)[[1]])
})

accs<-(results);accs
matriu.augmented.Death_los.CC<-lapply(accs, obtenir_segon_valor) #matrius confusio death
matriu.augmented.Death_los.CC<-arreglar.matriu.death2.CC(matriu.augmented.Death_los.CC)
prediccio.augmented.Death_los.CC<-lapply(accs, obtenir_primer_valor)#base de dades nova amb la death_prediction com a input


#fem servir la base de dades + la variable Death_prediction per predir LOS
for (i in 1:length(prediccio.augmented.Death_los.CC)){
  prediccio.augmented.Death_los.CC[[i]]<-as.data.frame(prediccio.augmented.Death_los.CC[[i]])
}
dades2<- do.call(rbind, prediccio.augmented.Death_los.CC)
dades2[["Death_prediction"]] <- as.character(dades2[["Death_prediction"]])
dades2[["Death_prediction"]][is.na(dades2[["Death_prediction"]])] <- 'unknown'
dades2[["Death_prediction"]] <- as.factor(dades2[["Death_prediction"]])
atributes=colnames(dades2[-c(12,1)])
bl=data.frame(from=atributes, to = rep("LOS",length(atributes))) # black list
wl=data.frame(from=rep("LOS",length(atributes)), to = atributes) # white list
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
library(caret)
library(bnlearn)
set.seed(1)
folds <- createFolds(dades2$LOS, k = 10)
results <- lapply(folds, function(x) {
  train <- dades2[-x, ]
  test <- dades2[x, ]
  pred<-confusion.matrix.death_los.CC2(train,test)
})

accs<-(results);accs
matriu.augmented.death_Los.CC<-accs
matriu.augmented.death_Los.CC<-arreglar.matriu.los2(matriu.augmented.death_Los.CC)


#####CHAIN CLASSIFIER: DEATH-->LOS  naive bayes
dades<-as.data.frame(dades)
atributes=colnames(dades[-c(1,12)])
bl=data.frame(from=atributes, to = rep(atributes, each=length(atributes)))
wl=data.frame(from=rep("Death",length(atributes)), to = atributes)
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
library(caret)
library(bnlearn)
set.seed(1)
folds <- createFolds(dades$Death, k = 10)
results <- lapply(folds, function(x) {
  train <- dades[-x, ]
  test <- dades[x, ]
  pred<-confusion.matrix.death_los.CC(train,test)[[2]]
  pred_df<-factor(pred)
  new_data<-cbind(test, "Death_prediction"=pred_df)
  list(new_data,confusion.matrix.death_los.CC(train,test)[[1]])
})

accs<-results;accs
matriu.naive.Death_los.CC<-lapply(accs, obtenir_segon_valor) #matrius confusio death
matriu.naive.Death_los.CC<-arreglar.matriu.death2.CC(matriu.naive.Death_los.CC)
prediccio.naive.Death_los.CC<-lapply(accs, obtenir_primer_valor)#base de dades nova amb la death_prediction com a input


#### fem servir la base de dades + la variable Death_prediction per predir LOS
for (i in 1:length(prediccio.naive.Death_los.CC)){
  prediccio.naive.Death_los.CC[[i]]<-as.data.frame(prediccio.naive.Death_los.CC[[i]])
}
dades2<- do.call(rbind, prediccio.naive.Death_los.CC)
atributes=colnames(dades2[-c(12,1)])
bl=data.frame(from=atributes, to = rep(atributes, each=length(atributes)))
wl=data.frame(from=rep("LOS",length(atributes)), to = atributes)
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
library(caret)
library(bnlearn)
set.seed(1)
folds <- createFolds(dades2$LOS, k = 10)
results <- lapply(folds, function(x) {
  train <- dades2[-x, ]
  test <- dades2[x, ]
  pred<-confusion.matrix.death_los.CC2(train,test)
})

accs<-(results);accs
matriu.naive.death_Los.CC<-accs
matriu.naive.death_Los.CC<-arreglar.matriu.los2(matriu.naive.death_Los.CC)




#####chain classifier LOS-->DEATH  augmented naive

dades<-as.data.frame(dades)
atributes=colnames(dades[-c(1,12)])
bl=data.frame(from=atributes, to = rep("LOS",length(atributes))) # black list
wl=data.frame(from=rep("LOS",length(atributes)), to = atributes) # white list
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
library(caret)
library(bnlearn)
set.seed(1)
folds <- createFolds(dades$LOS, k = 10)
results <- lapply(folds, function(x) {
  train <-dades[-x, ]
  test <- dades[x, ]
  pred<-confusion.matrix.los_death.CC(train,test)[[2]]
  pred_df<-factor(pred)
  new_data<-cbind(test, "LOS_prediction"=pred_df)
  list(new_data,confusion.matrix.los_death.CC(train,test)[[1]])
})
accs<-results;accs
matriu.augmented.Los_death.CC<-lapply(accs, obtenir_segon_valor) #matrius confusio death
matriu.augmented.Los_death.CC<-arreglar.matriu.los2.CC(matriu.augmented.Los_death.CC)
prediccio.augmented.Los_death.CC<-lapply(accs, obtenir_primer_valor)#base de dades nova amb la death_prediction com a input


#### fem servir la base de dades + la variable LOS_prediction per predir Death
for (i in 1:length(prediccio.augmented.Los_death.CC)){
  prediccio.augmented.Los_death.CC[[i]]<-as.data.frame(prediccio.augmented.Los_death.CC[[i]])
}
dades3<- do.call(rbind, prediccio.augmented.Los_death.CC)
dades3[["LOS_prediction"]] <- as.character(dades3[["LOS_prediction"]])
dades3[["LOS_prediction"]][is.na(dades3[["LOS_prediction"]])] <- 'unknown'
dades3[["LOS_prediction"]] <- as.factor(dades3[["LOS_prediction"]])
atributes=colnames(dades3[-c(1,12)])
bl=data.frame(from=atributes, to = rep("Death",length(atributes))) # black list
wl=data.frame(from=rep("Death",length(atributes)), to = atributes) # white list
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
library(caret)
library(bnlearn)
set.seed(1)
folds <- createFolds(dades3$Death, k = 10)
results <- lapply(folds, function(x) {
  train <- dades3[-x, ]
  test <- dades3[x, ]
  pred<-confusion.matrix.los_death.CC2(train,test)
})

accs<-(results);accs
matriu.augmented.los_Death.CC<-accs
matriu.augmented.los_Death.CC<-arreglar.matriu.death2(matriu.augmented.los_Death.CC)




#####chain classifier LOS-->DEATH   naive bayes

dades<-as.data.frame(dades)
atributes=colnames(dades[-c(12,1)])
bl=data.frame(from=atributes, to = rep(atributes, each=length(atributes)))# black list
wl=data.frame(from=rep("LOS",length(atributes)), to = atributes)  # white list
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
library(caret)
library(bnlearn)
set.seed(1)
folds <- createFolds(dades$LOS, k = 10)
results <- lapply(folds, function(x) {
  train <- dades[-x, ]
  test <- dades[x, ]
  pred<-confusion.matrix.los_death.CC(train,test)[[2]]
  pred_df<-factor(pred)
  new_data<-cbind(test, "LOS_prediction"=pred_df)
  list(new_data,confusion.matrix.los_death.CC(train,test)[[1]])
})

accs<-(results);accs
matriu.naive.Los_death.CC<-lapply(accs, obtenir_segon_valor) #matrius confusio death
matriu.naive.Los_death.CC<-arreglar.matriu.los2.CC(matriu.naive.Los_death.CC)
prediccio.naive.Los_death.CC<-lapply(accs, obtenir_primer_valor)#base de dades nova amb la death_prediction com a input


#### fem servir la base de dades + la variable LOS_prediction per predir Death
for (i in 1:length(prediccio.naive.Los_death.CC)){
  prediccio.naive.Los_death.CC[[i]]<-as.data.frame(prediccio.naive.Los_death.CC[[i]])
}
dades3<- do.call(rbind, prediccio.naive.Los_death.CC)
atributes=colnames(dades3[-c(1,12)])
bl=data.frame(from=atributes, to = rep(atributes, each=length(atributes))) #black list
wl=data.frame(from=rep("Death",length(atributes)), to = atributes) #white list
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
library(caret)
library(bnlearn)
set.seed(1)
folds <- createFolds(dades3$Death, k = 10)
results <- lapply(folds, function(x) {
  train <- dades3[-x, ]
  test <- dades3[x, ]
  pred<-confusion.matrix.los_death.CC2(train,test)
})

accs<-(results);accs
matriu.naive.los_Death.CC<-accs
matriu.naive.los_Death.CC<-arreglar.matriu.death2(matriu.naive.los_Death.CC)



####mètriques comportament

####Death
mc.augmented.death.BR<-metriques_comportament_death(matrius.augmented.death.BR)
mc.augmented.death.BR
mc.naive.death.BR<-metriques_comportament_death(matrius.naive.death.BR)
mc.naive.death.BR
acc <- vector()
balanced_acc <- vector()
f1_sc <- vector()
mcc2 <- vector()
for (i in 1:length(matrius.death.una.xarxa)) {
  m <- metriques_comportament_death(matrius.death.una.xarxa[[i]])
  acc <- c(acc, m$accuracy)
  balanced_acc <- c(balanced_acc, m$balanced_accuracy)
  f1_sc <- c(f1_sc, m$F1_score)
  mcc2 <- c(mcc2, m$mcc)
}
mc.una.xarxa.death<- list('accuracy' = acc, 'balanced_accuracy' = balanced_acc, 'F1_score' = f1_sc, 'mcc' = mcc2)
mc.una.xarxa.death
mc.augmented.Death_los.CC<-metriques_comportament_death(matriu.augmented.Death_los.CC)
mc.augmented.Death_los.CC
mc.naive.Death_los.CC<-metriques_comportament_death(matriu.naive.Death_los.CC)
mc.naive.Death_los.CC
mc.augmented.los_Death.CC<-metriques_comportament_death(matriu.augmented.los_Death.CC)
mc.augmented.los_Death.CC
mc.naive.los_Death.CC<-metriques_comportament_death(matriu.naive.los_Death.CC)
mc.naive.los_Death.CC


####LOS
mc.augmented.los.BR<-metriques_comportament_los(matrius.augmented.los.BR)
mc.augmented.los.BR
mc.naive.los.BR<-metriques_comportament_los(matrius.naive.los.BR)
mc.naive.los.BR
mae2<-vector()
for (i in 1:length(matrius.los.una.xarxa)) {
  m2 <- metriques_comportament_los(matrius.los.una.xarxa[[i]])
  mae2[i] <- unlist(m2)
}
mc.una.xarxa.los<-c(mae2)
mc.una.xarxa.los
mc.augmented.death_Los.CC<-metriques_comportament_los(matriu.augmented.death_Los.CC)
mc.augmented.death_Los.CC
mc.naive.death_Los.CC<-metriques_comportament_los(matriu.naive.death_Los.CC)
mc.naive.death_Los.CC
mc.augmented.Los_death.CC<-metriques_comportament_los(matriu.augmented.Los_death.CC)
mc.augmented.Los_death.CC
mc.naive.Los_death.CC<-metriques_comportament_los(matriu.naive.Los_death.CC)
mc.naive.Los_death.CC


####tests estadístics

lapply(mc.augmented.death.BR,mean)
lapply(mc.naive.death.BR,mean)
lapply(mc.augmented.Death_los.CC,mean)
lapply(mc.naive.Death_los.CC,mean)
lapply(mc.augmented.los_Death.CC,mean)
lapply(mc.naive.los_Death.CC,mean)
lapply(mc.una.xarxa.death,mean)
mean(mc.augmented.los.BR)
mean(mc.naive.los.BR)
mean(mc.augmented.death_Los.CC)
mean(mc.naive.death_Los.CC)
mean(mc.augmented.Los_death.CC)
mean(mc.naive.Los_death.CC)
mean(mc.una.xarxa.los)

####test accuracy

shapiro.test(mc.augmented.death.BR[[1]]-mc.naive.death.BR[[1]]) # hi ha normalitat 
test1<-t.test(mc.augmented.death.BR[[1]],mc.naive.death.BR[[1]], paired=TRUE, alternative="greater")
test1

shapiro.test(mc.augmented.los_Death.CC[[1]]-mc.naive.los_Death.CC[[1]])# hi ha normalitat
test2<-t.test(mc.augmented.los_Death.CC[[1]],mc.naive.los_Death.CC[[1]], paired=TRUE, alternative="greater")
test2

shapiro.test(mc.augmented.death.BR[[1]]-mc.augmented.los_Death.CC[[1]])# hi ha normalitat
test3<-t.test(mc.augmented.death.BR[[1]],mc.augmented.los_Death.CC[[1]], paired=TRUE, alternative="greater")
test3


shapiro.test(mc.augmented.death.BR[[1]]-mc.una.xarxa.death[[1]])# hi ha normalitat
test4<-t.test(mc.augmented.death.BR[[1]],mc.una.xarxa.death[[1]], paired=TRUE, alternative="greater")
test4

####test balanced_accuracy

shapiro.test(mc.naive.death.BR[[2]]-mc.augmented.death.BR[[2]]) # hi ha normalitat 
test1<-t.test(mc.naive.death.BR[[2]],mc.augmented.death.BR[[2]], paired=TRUE, alternative="greater")
test1

shapiro.test(mc.naive.los_Death.CC[[2]]-mc.augmented.los_Death.CC[[2]])#hi ha normalitat
test2<-t.test(mc.naive.los_Death.CC[[2]],mc.augmented.los_Death.CC[[2]], paired=TRUE, alternative="greater")
test2


shapiro.test(mc.naive.los_Death.CC[[2]]-mc.naive.death.BR[[2]])# hi ha normalitat
test3<-t.test(mc.naive.los_Death.CC[[2]],mc.naive.death.BR[[2]], paired=TRUE, alternative="greater")
test3

shapiro.test(mc.naive.death.BR[[2]]-mc.una.xarxa.death[[2]])# hi ha normalitat
test4<-t.test(mc.naive.death.BR[[2]],mc.una.xarxa.death[[2]], paired=TRUE, alternative="greater")
test4
shapiro.test(mc.naive.los_Death.CC[[2]]-mc.una.xarxa.death[[2]])# hi ha normalitat
test4<-t.test(mc.naive.los_Death.CC[[2]],mc.una.xarxa.death[[2]], paired=TRUE, alternative="greater")
test4

#### f1score

shapiro.test(mc.naive.death.BR[[3]]-mc.augmented.death.BR[[3]])# hi ha normalitat
test1<-t.test(mc.naive.death.BR[[3]],mc.augmented.death.BR[[3]], paired=TRUE, alternative="greater")
test1
test<-t.test(mc.naive.death.BR[[3]],mc.augmented.death.BR[[3]], paired=TRUE, alternative="less")
test

shapiro.test(mc.naive.los_Death.CC[[3]]-mc.augmented.los_Death.CC[[3]])# hi ha normalitat
test2<-t.test(mc.naive.los_Death.CC[[3]],mc.augmented.los_Death.CC[[3]], paired=TRUE, alternative="greater")
test2
test2.2<-t.test(mc.naive.los_Death.CC[[3]],mc.augmented.los_Death.CC[[3]], paired=TRUE, alternative="less")
test2.2


shapiro.test(mc.naive.death.BR[[3]]-mc.naive.los_Death.CC[[3]]) #hi ha normalitat
test3<-t.test(mc.naive.death.BR[[3]],mc.naive.los_Death.CC[[3]], paired=TRUE, alternative="greater")
test3

shapiro.test(mc.naive.death.BR[[3]]-mc.una.xarxa.death[[3]])#hi ha normalitat
test4<-t.test(mc.naive.death.BR[[3]],mc.una.xarxa.death[[3]], paired=TRUE, alternative="greater")
test4



#### mcc

shapiro.test(mc.augmented.death.BR[[4]]-mc.naive.death.BR[[4]])# hi ha normalitat
test1<-t.test(mc.augmented.death.BR[[4]],mc.naive.death.BR[[4]], paired=TRUE, alternative="greater")
test1

shapiro.test(mc.augmented.los_Death.CC[[4]]-mc.naive.los_Death.CC[[4]])# hi ha normalitat
test2<-t.test(mc.augmented.los_Death.CC[[4]],mc.naive.los_Death.CC[[4]], paired=TRUE, alternative="greater")
test2

shapiro.test(mc.naive.death.BR[[4]]-mc.naive.los_Death.CC[[4]])# hi ha normalitat
test3<-t.test(mc.naive.death.BR[[4]],mc.naive.los_Death.CC[[4]], paired=TRUE, alternative="greater")
test3


shapiro.test(mc.naive.death.BR[[4]]-mc.una.xarxa.death[[4]])# hi ha normalitat
test3<-t.test(mc.naive.death.BR[[4]],mc.una.xarxa.death[[4]], paired=TRUE, alternative="greater")
test3

#### mae
shapiro.test(mc.naive.los.BR-mc.augmented.los.BR)# hi ha normalitat
test1<-t.test(mc.naive.los.BR,mc.augmented.los.BR, paired=TRUE, alternative="greater")
test1
test<-t.test(mc.naive.los.BR,mc.augmented.los.BR, paired=TRUE, alternative="less")
test

shapiro.test(mc.naive.death_Los.CC-mc.augmented.death_Los.CC)# hi ha normalitat
test2<-t.test(mc.naive.death_Los.CC,mc.augmented.death_Los.CC, paired=TRUE, alternative="greater")
test2
test2<-t.test(mc.augmented.death_Los.CC,mc.naive.death_Los.CC, paired=TRUE, alternative="less")
test2

shapiro.test(mc.augmented.death_Los.CC-mc.naive.los.BR)#hi ha normalitat
test3<-t.test(mc.augmented.death_Los.CC,mc.naive.los.BR, paired=TRUE, alternative="greater")
test3
test3<-t.test(mc.augmented.death_Los.CC,mc.naive.los.BR, paired=TRUE, alternative="less")
test3


shapiro.test(mc.naive.los.BR-mc.una.xarxa.los)# hi ha normalitat
test4<-t.test(mc.naive.los.BR,mc.una.xarxa.los, paired=TRUE, alternative="greater")
test4
test4<-t.test(mc.naive.los.BR,mc.una.xarxa.los, paired=TRUE, alternative="less")
test4
