

########################### Xènia Nicolau Casademont- 1563614 ###############################
################################ FUNCIONS ####################################


####llistes
obtenir_segon_valor <- function(sub_llista) {
  return(sub_llista[2])
}
obtenir_primer_valor <- function(sub_llista) {
  return(sub_llista[1])
}

####arreglar matrius
arreglar.matriu.los<- function(mat_conf_list) {
  colnames_mat_conf_total <- c("1-3 days","4-7 days","1-2 weeks",">2 weeks")
  rownames_mat_conf_total <- c("1-3 days","4-7 days","1-2 weeks",">2 weeks")
  mat_conf_total <- matrix(0, nrow = 4, ncol = 4, dimnames = list(rownames_mat_conf_total, colnames_mat_conf_total))
  for (mat_conf in mat_conf_list) {
    colnames_mat_conf <- colnames(mat_conf)
    rownames_mat_conf <- rownames(mat_conf)
    for (i in 1:length(rownames_mat_conf_total)) {
      for (j in 1:length(colnames_mat_conf_total)) {
        if (rownames_mat_conf_total[i] %in% rownames_mat_conf && colnames_mat_conf_total[j] %in% colnames_mat_conf) {
          mat_conf_total[i, j] <- mat_conf_total[i, j] + mat_conf[rownames_mat_conf == rownames_mat_conf_total[i], colnames_mat_conf == colnames_mat_conf_total[j]]
        }
      }
    }
  }
  return(mat_conf_total)
}

arreglar.matriu.los2<-function(llista){
  resultats <- list() 
  for (i in 1:length(llista)) {
    resultat <- arreglar.matriu.los(llista[i])
    resultats[[i]] <- resultat 
  }
  return(resultats)
}

arreglar.matriu.los2.CC<-function(llista){
  resultats <- list() 
  for (i in 1:length(llista)) {
    resultat <- arreglar.matriu.los(llista[[i]])
    resultats[[i]] <- resultat 
  }
  return(resultats)
}

arreglar.matriu.death<- function(mat_conf_list) {
  colnames_mat_conf_total <- c("0","1")
  rownames_mat_conf_total <- c("0","1")
  mat_conf_total <- matrix(0, nrow = 2, ncol = 2, dimnames = list(rownames_mat_conf_total, colnames_mat_conf_total))
  for (mat_conf in mat_conf_list) {
    colnames_mat_conf <- colnames(mat_conf)
    rownames_mat_conf <- rownames(mat_conf)
    for (i in 1:length(rownames_mat_conf_total)) {
      for (j in 1:length(colnames_mat_conf_total)) {
        if (rownames_mat_conf_total[i] %in% rownames_mat_conf && colnames_mat_conf_total[j] %in% colnames_mat_conf) {
          mat_conf_total[i, j] <- mat_conf_total[i, j] + mat_conf[rownames_mat_conf == rownames_mat_conf_total[i], colnames_mat_conf == colnames_mat_conf_total[j]]
        }
      }
    }
  }
  return(mat_conf_total)
}
arreglar.matriu.death2<-function(llista){
  resultats <- list()  
  for (i in 1:length(llista)) {
    resultat <- arreglar.matriu.death(llista[i])
    resultats[[i]] <- resultat  
  }
  return(resultats)
}

arreglar.matriu.death2.CC<-function(llista){
  resultats <- list()  
  for (i in 1:length(llista)) {
    resultat <- arreglar.matriu.death(llista[[i]])
    resultats[[i]] <- resultat  
  }
  return(resultats)
}


####BINARY RELEVANCE:death 
confusion.matrix.Death.BR<-function(training,validate){
  xarxa<-hc(training[,-12], score="bic", whitelist=wl, blacklist=bl)
  plot<-graphviz.plot(xarxa)
  xarxa.estimada=bn.fit(xarxa,training[,-12],method="mle")
  xarxa.grain<-suppressWarnings(as.grain(xarxa.estimada))
  for (j in 1:nrow(validate)){
    if (is.numeric(predict(xarxa.grain,
                           response="Death",validate[j,],predictors=atributes,
                           type="dist")$pred[[1]][1,1])==FALSE)
    {prediccio[[j]]<-NA
    CL[[j]]<-0
    distribucio[[j]]<-c(rep(0,2))
    }
    else
    {
      prueba[[j]]<-predict(xarxa.grain,response="Death",
                           validate[j,],predictors=atributes,type="dist")
      distribucio[[j]]<-prueba[[j]]$pred[[1]]
      prediccio[[j]]<-dimnames(distribucio[[j]])[[2]][which.max(distribucio[[j]])]
      CL[[j]]<-max(distribucio[[j]])
    }
  }
  confusion.matrix<-
    as.matrix(table(validate$Death,unlist(prediccio)))
  return((as.matrix(confusion.matrix)))
}


####BINARY RELEVANCE:los
confusion.matrix.los.BR<-function(training,validate){
  xarxa<-hc(training[,-1], score="bic", whitelist=wl, blacklist=bl)
  plot<-graphviz.plot(xarxa)
  xarxa.estimada=bn.fit(xarxa,training[,-1],method="mle")
  xarxa.grain<-suppressWarnings(as.grain(xarxa.estimada))
  for (j in 1:nrow(validate)){
    if (is.numeric(predict(xarxa.grain,
                           response="LOS",validate[j,],predictors=atributes,
                           type="dist")$pred[[1]][1,1])==FALSE)
    {prediccio[[j]]<-NA
    CL[[j]]<-0
    distribucio[[j]]<-c(rep(0,4))
    }else
    {
      if (predict(xarxa.grain,response="LOS",
                  validate[j,],predictors=atributes,type="class")$pred[[1]]=="unknown")
      {
        prueba[[j]]<-predict(xarxa.grain,response="LOS",
                             validate[j,],predictors=atributes,type="dist")
        distribucio[[j]]<-prueba[[j]]$pred[[1]][,-5]
        prediccio[[j]]<-names(distribucio[[j]])[which.max(distribucio[[j]])]
        CL[[j]]<-max(distribucio[[j]])/sum(distribucio[[j]])
      } else {
        prueba[[j]]<-predict(xarxa.grain,response="LOS",
                             validate[j,],predictors=atributes,type="dist")
        distribucio[[j]]<-prueba[[j]]$pred[[1]]
        prediccio[[j]]<-dimnames(distribucio[[j]])[[2]][which.max(distribucio[[j]])]
        CL[[j]]<-max(distribucio[[j]])/sum(distribucio[[j]][,-5])
      }
    }
  }
  confusion.matrix<-
    as.matrix(table(validate$LOS,unlist(prediccio)))
  return(as.matrix(confusion.matrix))
}

####UNA UNICA XARXA
confusion.matrix.una.xarxa<-function(training,validate){
  xarxa<-hc(training, score="bic",blacklist=bl)
  plot<-graphviz.plot(xarxa)
  xarxa.estimada=bn.fit(xarxa,training,method="mle")
  xarxa.grain<-suppressWarnings(as.grain(xarxa.estimada))
  for (j in 1:nrow(validate)){
    if ( is.numeric(predict(xarxa.grain,response="Death",validate[j,],predictors=atributes,type="dist")$pred[[1]][1,1])==FALSE) 
    {prediccio.Death[[j]]<-NA
    CL.Death[[j]]<-0
    distribucio.Death[[j]]<-c(rep(0,2))
    }else{
      prueba.Death[[j]]<-predict(xarxa.grain,response="Death",
                                 validate[j,],predictors=atributes,type="dist")
      distribucio.Death[[j]]<-prueba.Death[[j]]$pred[[1]]
      prediccio.Death[[j]]<-dimnames(distribucio.Death[[j]])[[2]][which.max(distribucio.Death[[j]])]
      CL.Death[[j]]<-max(distribucio.Death[[j]])
    }
    if (is.numeric(predict(xarxa.grain,response="LOS",validate[j,],predictors=atributes,type="dist")$pred[[1]][1,1])==FALSE)
    {prediccio.LOS[[j]]<-NA
    CL.LOS[[j]]<-0
    distribucio.LOS[[j]]<-c(rep(0,4))
    } else
    {if (predict(xarxa.grain,response="LOS",
                 validate[j,],predictors=atributes,type="class")$pred[[1]]=="unknown"){
      prueba.LOS[[j]]<-predict(xarxa.grain,response="LOS",
                               validate[j,],predictors=atributes,type="dist")
      distribucio.LOS[[j]]<-prueba.LOS[[j]]$pred[[1]][,-5]
      prediccio.LOS[[j]]<-names(distribucio.LOS[[j]])[which.max(distribucio.LOS[[j]])]
      CL.LOS[[j]]<-max(distribucio.LOS[[j]])/sum(distribucio.LOS[[j]])
    }else
    {prueba.LOS[[j]]<-predict(xarxa.grain,response="LOS",
                              validate[j,],predictors=atributes,type="dist")
    distribucio.LOS[[j]]<-prueba.LOS[[j]]$pred[[1]]
    prediccio.LOS[[j]]<-dimnames(distribucio.LOS[[j]])[[2]][which.max(distribucio.LOS[[j]])]
    CL.LOS[[j]]<-max(distribucio.LOS[[j]])/sum(distribucio.LOS[[j]][,-5])
    }
    }
  }
  confusion.matrix.Death<-as.matrix(table(validate$Death,unlist(prediccio.Death)))
  confusion.matrix.LOS<-as.matrix(table(validate$LOS,unlist(prediccio.LOS)))
  llistamatrius<-list((as.matrix(confusion.matrix.Death)),(as.matrix(confusion.matrix.LOS)))
  return(llistamatrius)
}

#####CHAIN CLASSIFIER: DEATH-->LOS
confusion.matrix.death_los.CC<-function(training,validate){
xarxa<-hc(training[,-12], score="bic", whitelist=wl, blacklist=bl)
plot<-graphviz.plot(xarxa)
xarxa.estimada=bn.fit(xarxa,training[,-12],method="mle")
xarxa.grain<-suppressWarnings(as.grain(xarxa.estimada))
for (j in 1:nrow(validate)){
  if (is.numeric(predict(xarxa.grain,
                         response="Death",validate[j,],predictors=atributes,
                         type="dist")$pred[[1]][1,1])==FALSE)
  {prediccio[[j]]<-NA
  CL[[j]]<-0
  distribucio[[j]]<-c(rep(0,2))
  }
  else
  {
    prueba[[j]]<-predict(xarxa.grain,response="Death",
                         validate[j,],predictors=atributes,type="dist")
    distribucio[[j]]<-prueba[[j]]$pred[[1]]
    prediccio[[j]]<-dimnames(distribucio[[j]])[[2]][which.max(distribucio[[j]])]
    CL[[j]]<-max(distribucio[[j]])
  }
}
confusion.matrix<-
  as.matrix(table(validate$Death,unlist(prediccio)))
prediccio<-unlist(prediccio)
return(list(as.matrix(confusion.matrix),prediccio))
}

confusion.matrix.death_los.CC2<-function(training,validate){
  xarxa<-hc(training[,-1], score="bic", whitelist=wl, blacklist=bl)
  plot<-graphviz.plot(xarxa)
  xarxa.estimada=bn.fit(xarxa,training[,-1],method="mle")
  xarxa.grain<-suppressWarnings(as.grain(xarxa.estimada))
  for (j in 1:nrow(validate)){
    if (is.numeric(predict(xarxa.grain,
                           response="LOS",validate[j,],predictors=atributes,
                           type="dist")$pred[[1]][1,1])==FALSE)
    {prediccio[[j]]<-NA
    CL[[j]]<-0
    distribucio[[j]]<-c(rep(0,4))
    }else
    {
      if (predict(xarxa.grain,response="LOS",
                  validate[j,],predictors=atributes,type="class")$pred[[1]]=="unknown")
      {
        prueba[[j]]<-predict(xarxa.grain,response="LOS",
                             validate[j,],predictors=atributes,type="dist")
        distribucio[[j]]<-prueba[[j]]$pred[[1]][,-5]
        prediccio[[j]]<-names(distribucio[[j]])[which.max(distribucio[[j]])]
        CL[[j]]<-max(distribucio[[j]])/sum(distribucio[[j]])
      } else {
        prueba[[j]]<-predict(xarxa.grain,response="LOS",
                             validate[j,],predictors=atributes,type="dist")
        distribucio[[j]]<-prueba[[j]]$pred[[1]]
        prediccio[[j]]<-dimnames(distribucio[[j]])[[2]][which.max(distribucio[[j]])]
        CL[[j]]<-max(distribucio[[j]])/sum(distribucio[[j]][,-5])
      }
    }
  }
  confusion.matrix<-
    as.matrix(table(validate$LOS,unlist(prediccio)))
  return(as.matrix(confusion.matrix))
}


#####CHAIN CLASSIFIER: LOS-->DEATH
confusion.matrix.los_death.CC<-function(training,validate){
  xarxa<-hc(training[,-1], score="bic", whitelist=wl, blacklist=bl)
  plot<-graphviz.plot(xarxa)
  xarxa.estimada=bn.fit(xarxa,training[,-1],method="mle")
  xarxa.grain<-suppressWarnings(as.grain(xarxa.estimada))
  for (j in 1:nrow(validate)){
    if (is.numeric(predict(xarxa.grain,
                           response="LOS",validate[j,],predictors=atributes,
                           type="dist")$pred[[1]][1,1])==FALSE)
    {prediccio[[j]]<-NA
    CL[[j]]<-0
    distribucio[[j]]<-c(rep(0,4))
    }else
    {
      if (predict(xarxa.grain,response="LOS",
                  validate[j,],predictors=atributes,type="class")$pred[[1]]=="unknown")
      {
        prueba[[j]]<-predict(xarxa.grain,response="LOS",
                             validate[j,],predictors=atributes,type="dist")
        distribucio[[j]]<-prueba[[j]]$pred[[1]][,-5]
        prediccio[[j]]<-names(distribucio[[j]])[which.max(distribucio[[j]])]
        CL[[j]]<-max(distribucio[[j]])/sum(distribucio[[j]])
      } else {
        prueba[[j]]<-predict(xarxa.grain,response="LOS",
                             validate[j,],predictors=atributes,type="dist")
        distribucio[[j]]<-prueba[[j]]$pred[[1]]
        prediccio[[j]]<-dimnames(distribucio[[j]])[[2]][which.max(distribucio[[j]])]
        CL[[j]]<-max(distribucio[[j]])/sum(distribucio[[j]][,-5])
      }
    }
  }
  confusion.matrix<-
    as.matrix(table(validate$LOS,unlist(prediccio)))
  # confusion.matrix<-arreglar.matriu(confusion.matrix)
  prediccio<-unlist(prediccio)
  return(list(as.matrix(confusion.matrix),prediccio))
}

confusion.matrix.los_death.CC2<-function(training,validate){
  
  xarxa<-hc(training[,-12], score="bic", whitelist=wl, blacklist=bl)
  plot<-graphviz.plot(xarxa)
  xarxa.estimada=bn.fit(xarxa,training[,-12],method="mle")
  xarxa.grain<-suppressWarnings(as.grain(xarxa.estimada))
  for (j in 1:nrow(validate)){
    if (is.numeric(predict(xarxa.grain,
                           response="Death",validate[j,],predictors=atributes,
                           type="dist")$pred[[1]][1,1])==FALSE)
    {prediccio[[j]]<-NA
    CL[[j]]<-0
    distribucio[[j]]<-c(rep(0,2))
    }
    else
    {
      prueba[[j]]<-predict(xarxa.grain,response="Death",
                           validate[j,],predictors=atributes,type="dist")
      distribucio[[j]]<-prueba[[j]]$pred[[1]]
      prediccio[[j]]<-dimnames(distribucio[[j]])[[2]][which.max(distribucio[[j]])]
      CL[[j]]<-max(distribucio[[j]])
    }
  }
  confusion.matrix<-
    as.matrix(table(validate$Death,unlist(prediccio)))
  return(as.matrix(confusion.matrix))
}


####mètriques comportament
accuracy <- function(matrix_confusion) {
  acc<-sum(diag(as.matrix(matrix_confusion))) / sum(matrix_confusion)
  accuracy<-round(acc*100,3)
  return(accuracy)
}

balanced_accuracy <- function(matrix_confusion) {
  tp <- matrix_confusion[1, 1]
  tn <- matrix_confusion[2, 2]
  fp <- matrix_confusion[2, 1]
  fn <- matrix_confusion[1, 2]
  tpr<- tp / (tp + fn)
  tnr<- tn / (tn + fp)
  balanced_acc<- (1/2)*(tpr+tnr) 
  balanced_accuracy<-round(balanced_acc*100,3)
  return(balanced_accuracy)
}

f1_score <- function(confusion_matrix) {
  tp<- confusion_matrix[2, 2]  
  fp<- confusion_matrix[1, 2]  
  fn<- confusion_matrix[2, 1]  
  precision <-tp/(tp+fp)
  f1_score <- (2 *tp )/ (2*tp+fp+fn)
  return(f1_score)
}

mcc<- function(confusion_matrix) {
  tp <- confusion_matrix[1, 1]  
  fp<- confusion_matrix[2, 1]  
  fn<- confusion_matrix[1, 2] 
  tn<- confusion_matrix[2, 2] 
  mcc<- ((tp * tn) - (fp * fn))/sqrt((tp+ fp) * (tp + fn) * (tn + fp) * (tn + fn))
  return(mcc)
}

mae<- function(confusion_matrix) {
  n <- sum(confusion_matrix)  
  n_classes <- nrow(confusion_matrix)  
  mae <- 0
  for (i in 1:n_classes) {
    for (j in 1:n_classes) {
      weight <- abs(i - j)  
      mae <- mae + confusion_matrix[i, j] * weight  
    }
  }
  mae <- (1/n) * mae  
  return(mae)
}

metriques_comportament_death<-function(llistamatrius){
  acc<-vector()
  balanced_acc<-vector()
  f1_sc<-vector()
  mcc2<-vector()
  for ( j in 1:length(llistamatrius)){
    acc[j]<-accuracy(llistamatrius[[j]])
    balanced_acc[j]<-balanced_accuracy(llistamatrius[[j]])
    f1_sc[j]<-f1_score(llistamatrius[[j]])
    mcc2[j]<-mcc(llistamatrius[[j]])
  }
  return(list('accuracy'=acc,'balanced_accuracy'=balanced_acc,'F1_score'=f1_sc,'mcc'=mcc2))
}

metriques_comportament_los<-function(llistamatrius){
  mae<-vector()
  for ( j in 1:length(llistamatrius)){
    mae[j]<-mae(llistamatrius[[j]])
  }
  return(mae)
}

calcular_mitjana <- function(llista) {
  mean(llista)
}


####model explicatiu

pintar_grafics <- function(training, validate) {
  xarxa <- hc(training, score = "bic", blacklist = bl)
  plot <- graphviz.plot(xarxa, highlight = list(nodes = c('F13', 'F7', 'F3', 'F4', 'F2', 'F5', 'F1', 'F6'),col = "black", fill = "lightblue"))
  return(plot)
}

