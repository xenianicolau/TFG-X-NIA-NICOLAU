

########################### Xènia Nicolau Casademont- 1563614 ###############################
################################ PREDICCIONS ####################################


####Prediccons amb tota la base de dades variable DEATH

dades<-as.data.frame(dades)
atributes=colnames(dades[-c(1,12)])
bl=data.frame(from=atributes, to = rep(atributes, each=length(atributes)))
wl=data.frame(from=rep("Death",length(atributes)), to = atributes)  
prediccio=NULL
CL=NULL
prueba=NULL
distribucio=NULL
set.seed(1)
results <- confusion.matrix.Death.BR(dades,dades) #millor model death: binary relevance naive bayes
accs<-(results);accs
millor.model.death<-list(accs)
millor.model.death<-arreglar.matriu.death2(millor.model.death)


####PREDICCIO D'UN NOU CAS PER A LES DIFERENTS VARIABLES

####VARIABLE DEATH
dades<-as.data.frame(dades)
atributes=colnames(dades[-c(1,12)])
bl=data.frame(from=atributes, to = rep(atributes, each=length(atributes)))
wl=data.frame(from=rep("Death",length(atributes)), to = atributes)  
set.seed(1)
xarxa.final<-hc(dades[,-12], score="bic", whitelist=wl, blacklist=bl)
xarxa.final.estimada=bn.fit(xarxa.final,dades[-12],method="mle")
plot.xarxa.final<-graphviz.plot(xarxa.final)
xarxa.final.grain<-suppressWarnings(as.grain(xarxa.final.estimada))


#Si tenim un pacient amb uns valors concrets de les variables atributs introduïm l'evidència i trobem la marginal per a la variable classe V1

####edat [0,45)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14"),
                        states=c("[0,45)"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                     "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient NO mori, amb prob. 93.58%. Es a dir, el risc de reincidencia o recaiguda es d'un 6.42%.

##### edat [45-55)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14"),
                        states=c("[45-55)"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                     "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient NO mori, amb prob. 89.56%.Es a dir, el risc de reincidencia o recaiguda es d'un 10.44%.

#### edat [55-65)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14"),
                        states=c("[55-65)"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat3<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat3=as.data.frame(Resultat3)
colnames(Resultat3)=c("Predicció","Confidence Level (CL) en %",
                     "Risc de reincidència")
evid
Resultat3 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient NO mori, amb prob. 81.39 %. Es a dir, el risc de reincidencia o recaiguda es d'un 18.61%.

###edat [65-75)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14"),
                        states=c("[65-75)"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat4<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat4=as.data.frame(Resultat4)
colnames(Resultat4)=c("Predicció","Confidence Level (CL) en %",
                     "Risc de reincidència")
evid
Resultat4 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient NO mori, amb prob. 71.79%. Es a dir, el risc de reincidencia o recaiguda es d'un 28.21%.

###edat [75,103]
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14"),
                        states=c("[75,103]"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat5<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat5=as.data.frame(Resultat5)
colnames(Resultat5)=c("Predicció","Confidence Level (CL) en %",
                     "Risc de reincidència")
evid
Resultat5 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient NO mori, amb prob. 58.84 %. Es a dir, el risc de reincidencia o recaiguda es d'un 41.16%.


###grafic evolucio risc de mort en funció de l'edat
dades_mort <- data.frame(edat = as.factor(c( "[0,45)",  "[45-55)",  "[55-65)",  "[65-75)", "[75,103]" )),
                         prob_mort = c(6.42, 10.44, 18.61, 28.21, 41.16))
ggplot(dades_mort, aes(x = edat, y = prob_mort)) +
  geom_point(size = 2) +
  labs(x = "Edat", y = "Risc de mort") +
  theme_minimal()





#### creatinina (f22)

####f22 <1.5
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F22"),
                        states=c("<1.5"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.82.33%.  Es a dir, el risc de reincidencia o recaiguda es d'un 17.67%.   

####f22 1.5-2.5
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F22"),
                        states=c("1.5-2.5"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.  62.66%. Es a dir, el risc de reincidencia o recaiguda es d'un 37.34%.    


####f22 >2.5
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F22"),
                        states=c(">2.5"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat3<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat3=as.data.frame(Resultat3)
colnames(Resultat3)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat3 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.  58.86%. Es a dir, el risc de reincidencia o recaiguda es d'un 41.14%.  


###grafic evolucio risc de mort en funcio dels diferents valors de creatinina
library(ggplot2)
dades_mort <- data.frame(creatinina = as.factor(c( "<1.5",  "1.5-2.5",">2.5")),
                         prob_mort = c(17.67,37.34,41.14))
ggplot(dades_mort, aes(x = creatinina, y = prob_mort)) +
  geom_point(size = 2) +
  labs(x = "Creatinina (F22)", y = "Risc de mort") +
  theme_minimal()




####saturacio d'oxigen en sang (F15)

####F15 ("adequat" >=94)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F15"),
                        states=c("0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 80.69%.Es a dir, el risc de reincidencia o recaiguda es d'un 19.31%.   

####F15 ("no adequat" <94)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F15"),
                        states=c("1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 65.65%. Es a dir, el risc de reincidencia o recaiguda es d'un 34.35%.    


####grafic evolucio risc de mort en funcio dels valors de la saturació d'oxigen en sang
library(ggplot2)
dades_mort <- data.frame(o2sats = as.factor(c( "0",  "1")),
                         prob_mort = c(19.31,34.35))
ggplot(dades_mort, aes(x = o2sats, y = prob_mort)) +
  geom_point(size = 2) +
  labs(x = "Saturació oxigen en sang (F15)", y = "Risc de mort") +
  theme_minimal()




#####troponin(F33)

####troponin 0
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F33"),
                        states=c("0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 76.77%.Es a dir, el risc de reincidencia o recaiguda es d'un 23.23%.   

####troponin 1
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F33"),
                        states=c("1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 55.11%.Es a dir, el risc de reincidencia o recaiguda es d'un 44.89%.    

####grafic evolució del risc de mort en funció dels valors de troponin
library(ggplot2)
dades_mort <- data.frame(o2sats = as.factor(c( "0",  "1")),
                         prob_mort = c(23.23,44.89))
ggplot(dades_mort, aes(x = o2sats, y = prob_mort)) +
  geom_point(size = 2) +
  labs(x = "Troponina (F33)", y = "Risc de mort") +
  theme_minimal()




###limfocits (F28)

####limfocits 0 ("adequats" >=1)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F28"),
                        states=c("0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.79.42%. Es a dir, el risc de reincidencia o recaiguda es d'un 20.58%.   


####limfocits 1 ("no adequats"<1)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F28"),
                        states=c("1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 70.15%. Es a dir, el risc de reincidencia o recaiguda es d'un 29.85%.   


####grafic evolució risc de mort en funció dels valors de limfocits
library(ggplot2)
dades_mort <- data.frame(limfocits = as.factor(c( "0",  "1")),
                         prob_mort = c(20.58,29.85))
ggplot(dades_mort, aes(x = limfocits, y = prob_mort)) +
  geom_point(size = 2) +
  labs(x = "Limfòcits (F28)", y = "Risc de mort") +
  theme_minimal()



####severity (F11)

#### severity 0-2
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F11"),
                        states=c("0-2"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.   93.36   %Es a dir, el risc de reincidencia o recaiguda es d'un 6.64%.  


#### severity "3-4" 

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F11"),
                        states=c("3-4"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.   79.53%.Es a dir, el risc de reincidencia o recaiguda es d'un 20.47%.  



#### severity "5-11 " 

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F11"),
                        states=c("5-11" ))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat3<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat3=as.data.frame(Resultat3)
colnames(Resultat3)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat3 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.  52.4%.Es a dir, el risc de reincidencia o recaiguda es d'un 47.6%. 


####grafic evolucio risc de mort en funcio del grau de severitat
library(ggplot2)
dades_mort <- data.frame(severity = as.factor(c( "0-2",  "3-4",  "5-11")),
                         prob_mort = c(6.64, 20.47, 47.6))
ggplot(dades_mort, aes(x = severity, y = prob_mort)) +
  geom_point(size = 2) +
  labs(x = "Severity", y = "Risc de mort") +
  theme_minimal()




#### Es fa el maeix anàlisis de variables però amb l'edat [45-55) i l'edat [75,103]


###edat [75,103] i creatinina <1.5
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F22"),
                        states=c("[75,103]","<1.5"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient NO mori, amb prob. 68.22%. Es a dir, el risc de reincidencia o recaiguda es d'un 31.78%.


####edat[75,103] i creatinina "1.5-2.5"  
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F22"),
                        states=c("[75,103]","1.5-2.5"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient  morirà, amb prob. 56.41%.Es a dir, el risc de reincidencia o recaiguda es d'un 56.41%.  


####edat[75,103] i creatinina ">2.5"

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F22"),
                        states=c("[75,103]",">2.5"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat3<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat3=as.data.frame(Resultat3)
colnames(Resultat3)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat3 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient  morirà, amb prob.  60.28%.Es a dir, el risc de reincidencia o recaiguda es d'un 60.28%.  



###edat [45-55) i creatinina <1.5
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F22"),
                        states=c("[45-55)","<1.5"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient NO mori, amb prob.92.79%. Es a dir, el risc de reincidencia o recaiguda es d'un 7.21%.


####edat[45-55) i creatinina ">2.5"  
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F22"),
                        states=c("[45-55)","1.5-2.5"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 70.28%. Es a dir, el risc de reincidencia o recaiguda es d'un 29.72%.  



####edat[45-55) i creatinina "1.5-2.5"

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F22"),
                        states=c("[45-55)",">2.5"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat3<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat3=as.data.frame(Resultat3)
colnames(Resultat3)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat3 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.  66.85%.  Es a dir, el risc de reincidencia o recaiguda es d'un 33.15%. 



#### edat [75-103] i saturacio oxigen en sang (F15)

####F15 0 i edat [75,103]
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F15"),
                        states=c("[75,103]","0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 65.8  %.Es a dir, el risc de reincidencia o recaiguda es d'un 34.2%.   


####F15 1 i edat [75,103]
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F15"),
                        states=c("[75,103]","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient morirà, amb prob. 53.18 %. Es a dir, el risc de reincidencia o recaiguda es d'un 53.18%.    



####F15 0 i edat [45-55)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F15"),
                        states=c("[45-55)","0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 92.03  %.Es a dir, el risc de reincidencia o recaiguda es d'un 7.97%.   


####F15 1 i edat [45-55)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F15"),
                        states=c("[45-55)","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.84.08 %. Es a dir, el risc de reincidencia o recaiguda es d'un 15.92%.    




###limfocits (F28)

####limfocits 0 i edat [75,103]
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F28"),
                        states=c("[75,103]","0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.64%. Es a dir, el risc de reincidencia o recaiguda es d'un 36%.   


####limfocits 1
xarxa.evid<-setEvidence(xarxa.final.grain,nodes=c("F14","F28"),
                        states=c("[75,103]","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 51.97 %. Es a dir, el risc de reincidencia o recaiguda es d'un 48.03%.   



####limfocits 0 i edat [45-55)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F28"),
                        states=c("[45-55)","0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 91.43%. Es a dir, el risc de reincidencia o recaiguda es d'un 8.57%.   


####limfocits 1
xarxa.evid<-setEvidence(xarxa.final.grain,nodes=c("F14","F28"),
                        states=c("[45-55)","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.86.65  %. Es a dir, el risc de reincidencia o recaiguda es d'un 13.35%.   



#####troponin(F33) i edat [75,103]

####troponin 0 i edat [75,103]
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F33"),
                        states=c("[75,103]","0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 60.35 %.Es a dir, el risc de reincidencia o recaiguda es d'un 39.65%.   

####troponin 1 i edat [75,103]
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F33"),
                        states=c("[75,103]","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient morirà, amb prob.  63.88 %.Es a dir, el risc de reincidencia o recaiguda es d'un 63.88%.    



####troponin 0 i edat [45-55)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F33"),
                        states=c("[45-55)","0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 90.13  %.Es a dir, el risc de reincidencia o recaiguda es d'un  9.87%.   

####troponin 1 i edat [45-55)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F33"),
                        states=c("[45-55)","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient morirà, amb prob.  77.23 %.Es a dir, el risc de reincidencia o recaiguda es d'un 22.77%.    


####severity i edat 

####edat[75,103] i severity 0-2"

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F11"),
                        states=c("[75,103]","0-2"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.  86.62%. Es a dir, el risc de reincidencia o recaiguda es d'un 13.38%.   

####edat[75,103] i severity 3-4"
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F11"),
                        states=c("[75,103]","3-4"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 64.15.% Es a dir, el risc de reincidencia o recaiguda es d'un  35.85%.  


####edat[75-103] i severity 5-11"
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F11"),
                        states=c("[75,103]","5-11"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat3<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat3=as.data.frame(Resultat3)
colnames(Resultat3)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat3 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient morirà, amb prob. 66.36%. Es a dir, el risc de reincidencia o recaiguda es d'un 66.36%.   


####edat[45-55) i severity 0-2"

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F11"),
                        states=c("[45-55)","0-2"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat1<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob. 97.49%. Es a dir, el risc de reincidencia o recaiguda es d'un 2.51%.  


####edat[45-55) i severity 3-4"

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F11"),
                        states=c("[45-55)","3-4"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.  91.48 %. Es a dir, el risc de reincidencia o recaiguda es d'un 8.52%.  


####edat[45-55) i severity 5-11"

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F11"),
                        states=c("[45-55)","5-11"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat3<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat3=as.data.frame(Resultat3)
colnames(Resultat3)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat3 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient no morirà, amb prob.75.26%. Es a dir, el risc de reincidencia o recaiguda es d'un 24.74%.   (en cap cas el pacient ha mort per a una edat de 55-65)







####Prediccons amb tota la base de dades variable LOS

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
results <- confusion.matrix.una.xarxa(dades,dades) #millor model death: binary relevance naive bayes
accs<-(results);accs



####PREDICCIO D'UN NOU CAS PER A LES DIFERENTS VARIABLES

####VARIABLE LOS
dades<-as.data.frame(dades)
atributes=colnames(dades[-c(1,12)])
bl=data.frame(from=atributes, to = c(rep("Death",length(atributes)),rep("LOS",length(atributes))))# black list
set.seed(1)
xarxa.final<-hc(dades, score="bic", blacklist=bl)
xarxa.final.estimada=bn.fit(xarxa.final,dades,method="mle")
plot.xarxa.final<-graphviz.plot(xarxa.final)
xarxa.final.grain<-suppressWarnings(as.grain(xarxa.final.estimada))


#Si tenim un pacient amb uns valors concrets de les variables atributs introduïm l'evidència i trobem la marginal per a la variable classe V1

####edat [0,45)

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14"),
                        states=c("[0,45)"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat1<-matrix(c(prediccio, CL),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 4-7 days a l'hospital amb prob. 32.79%


##### edat [45-55)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14"),
                        states=c("[45-55)"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat2<-matrix(c(prediccio, CL),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat2 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient es quedi 4-7 days  amb prob.  32.73%


####[55-65)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14"),
                        states=c("[55-65)"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat3<-matrix(c(prediccio, CL),nrow=1)
Resultat3=as.data.frame(Resultat3)
colnames(Resultat3)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat3## Veiem que amb aquesta evidencia, el mes probable es que el pacient es quedi 4-7 days, amb prob. 32.53 %


###edat [65-75)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14"),
                        states=c("[65-75)"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat4<-matrix(c(prediccio, CL),nrow=1)
Resultat4=as.data.frame(Resultat4)
colnames(Resultat4)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat4 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient es quedi 4-7 days, amb prob. 32.18 %


###edat [75,103]
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14"),
                        states=c("[75,103]"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat5<-matrix(c(prediccio, CL),nrow=1)
Resultat5=as.data.frame(Resultat5)
colnames(Resultat5)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat5 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient es quedi 4-7 dies, amb prob. 31.8  %

####grafic de l'evolució de los respecte els valors de l'edat
library(ggplot2)
dades_prob <- data.frame(edat = as.factor(c( "[0,45)", "[45-55)","[55-65)","[65-75)", "[75,103]" )),
                         prob = c( 32.79,32.73,32.53,32.18, 31.8))
ggplot(dades_prob, aes(x = edat, y = prob)) +
  geom_point(size = 2) +
  labs(x = "Edat (F14)", y = "prob") +
  theme_minimal()





#### bun (f21) 0
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F21"),
                        states=c("0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat1<-matrix(c(prediccio, CL),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 4-7 dies , amb prob.34.77%. 


####bun 1
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F21"),
                        states=c("1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat2<-matrix(c(prediccio, CL),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat2## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 4-7 dies, amb prob. 32.22 %. 


####grafic de l'evolució de los respecte els valors de bun
library(ggplot2)
dades_prob <- data.frame(bun = as.factor(c( "0",  "1")),
                         prob = c(34.77, 32.22))
ggplot(dades_prob, aes(x = bun, y = prob)) +
  geom_point(size = 2) +
  labs(x = "Nitrogen ureic sanguini (F21)", y = "prob") +
  theme_minimal()



####pvd "0"

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F13"),
                        states=c("0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat1<-matrix(c(prediccio, CL),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 4-7 dies, amb prob. 33.82%

####pvd "1"

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F13"),
                        states=c("1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat2<-matrix(c(prediccio, CL),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat2
## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 1-3 days , amb prob.  34.49 %


####grafic de l'evolució de los respecte els valors de pvd
library(ggplot2)
dades_prob <- data.frame(pvd = as.factor(c( "0",  "1")),
                         prob = c(33.82, 34.49))
ggplot(dades_prob, aes(x = pvd, y = prob)) +
  geom_point(size = 2) +
  labs(x = "Nitrogen ureic sanguini (F21)", y = "prob") +
  theme_minimal()




####fem el mateix però en funciò de l'edat [45-55) i [75-103]


#### bun (f21) 0 i edat [45-55]
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F21"),
                        states=c("[45-55)","0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat1<-matrix(c(prediccio, CL),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 4-7 dies , amb prob. 34.89%. 


####bun 1 i edat [45-55)
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F21"),
                        states=c("[45-55)","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat2<-matrix(c(prediccio, CL),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat2## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 4-7 dies, amb prob. 32.13 %. 


#### bun (f21) 0 i edat [75,103]
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F21"),
                        states=c("[75,103]","0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat1<-matrix(c(prediccio, CL),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 4-7 dies , amb prob. 34.55%. 


####bun 1 i edat[75,103]
xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F21"),
                        states=c("[75,103]","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat2<-matrix(c(prediccio, CL),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat2## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 4-7 dies, amb prob. 32.27 %. 


####pvd "0" i edat [45-55)

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F13"),
                        states=c("[45-55)","0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat1<-matrix(c(prediccio, CL),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 4-7 dies, amb prob. 34.37%


####pvd "1"i edat [45-55)

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F13"),
                        states=c("[45-55)","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat2<-matrix(c(prediccio, CL),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat2
## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 1-3 days , amb prob.  34.91 %



####pvd "0" i edat [75,103]

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F13"),
                        states=c("[75,103]","0"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat1<-matrix(c(prediccio, CL),nrow=1)
Resultat1=as.data.frame(Resultat1)
colnames(Resultat1)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat1 ## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 4-7 dies, amb prob. 33.15%


####pvd "1"i edat [75-103]

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F13"),
                        states=c("[75,103]","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat2<-matrix(c(prediccio, CL),nrow=1)
Resultat2=as.data.frame(Resultat2)
colnames(Resultat2)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat2  ## Veiem que amb aquesta evidencia, el mes probable es que el pacient estigui 1-3 days , amb prob.  33.99 %





####prediccio d'un individu amb totes les variables escollides
###edat [75,103]
###severity 5-11
###creatinina >2.5 
###limfocits (1)
###troponin (1)
###saturacio oxigen (1)

#####VARIABLE DEATH
dades<-as.data.frame(dades)
atributes=colnames(dades[-c(1,12)])
bl=data.frame(from=atributes, to = rep(atributes, each=length(atributes)))
wl=data.frame(from=rep("Death",length(atributes)), to = atributes)  
set.seed(1)
xarxa.final<-hc(dades[,-12], score="bic", whitelist=wl, blacklist=bl)
xarxa.final.estimada=bn.fit(xarxa.final,dades[-12],method="mle")
plot.xarxa.final<-graphviz.plot(xarxa.final)
xarxa.final.grain<-suppressWarnings(as.grain(xarxa.final.estimada))


xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F11","F15","F22","F28","F33"),
                        states=c("[75,103)","5-11",">2.5","1","1","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("Death"),type="marginal")
distribucio<-qq$Death
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
reincidence.risc<-round(100*distribucio[2],2)
Resultat<-matrix(c(prediccio, CL,reincidence.risc),nrow=1)
Resultat=as.data.frame(Resultat)
colnames(Resultat)=c("Predicció","Confidence Level (CL) en %",
                      "Risc de reincidència")
evid
Resultat ## Veiem que amb aquesta evidencia, el mes probable es que el pacient  morirà, amb prob.75.21 %. Es a dir, el risc de reincidencia o recaiguda es d'un 75.21%.  


####VARIABLE LOS
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
results <- confusion.matrix.una.xarxa(dades,dades) #millor model death: binary relevance naive bayes
accs<-(results);accs

xarxa.evid<-setEvidence(xarxa.final.grain, nodes=c("F14","F11","F15","F22","F28","F33","F13"),
                        states=c("[75,103)","5-11",">2.5","1","1","1","1"))
evid<-xarxa.evid$evidence
qq<-querygrain(xarxa.evid,nodes=c("LOS"),type="marginal")
distribucio<-qq$LOS
prediccio<-dimnames(distribucio)[[1]][which.max(distribucio)]
CL<-round(100*max(distribucio),2)
Resultat<-matrix(c(prediccio, CL),nrow=1)
Resultat=as.data.frame(Resultat)
colnames(Resultat)=c("Predicció","Confidence Level (CL) en %")
evid
Resultat ## Veiem que amb aquesta evidencia, el mes probable es que el pacient  morirà, amb prob.75.21 %. Es a dir, el risc de reincidencia o recaiguda es d'un 75.21%.  




####GRAFICS VARIABLE DEATH


####variable oxigen en sang
#<94 #45 anys 15.92  #75 anys 53.18
#≥ 94 #45 anys  7.97 #75 anys 34.2
library(ggplot2)
ds <- data.frame(Categoria = rep(c("<94", ">=94"), each = 2),
                 Edat = rep(c("[45-55)", "[75,103]"), times = 2),
                 Valor = c(15.92, 53.18, 7.97, 34.2))  

ggplot(ds, aes(x = Categoria, y = Valor, fill = factor(Edat))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Saturació oxigen en sang (F15)", y = "Risc de mort (%)") +
  scale_fill_manual(values = c("#006699", "chocolate2")) +
  theme_minimal()+
  geom_hline(yintercept = 50, linetype = "dashed",size = 0.7, color = "black")

####variable creatinina
ds <- data.frame(Categoria = rep(c("<1.5", "1.5-2.5", ">2.5"), each = 2),
                 Edat = rep(c("[45-55)", "[75,103]"), times = 3),
                 Valor = c(7.21, 31.78, 29.72, 56.41, 33.15, 60.28))

ds$Categoria <- factor(ds$Categoria, levels = c("<1.5", "1.5-2.5", ">2.5"))# perque apareixin en l'ordre que volem
ggplot(ds, aes(x = Categoria, y = Valor, fill = factor(Edat))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Creatinina (F22)", y = "Risc de mort (%)") +
  scale_fill_manual(values = c("#006699", "chocolate2")) +
  theme_minimal()+
  geom_hline(yintercept = 50, linetype = "dashed",size = 0.7, color = "black")

####variable limfòcits
ds <- data.frame(Categoria = rep(c("<1", ">=1"), each = 2),
                 Edat = rep(c("[45-55)", "[75,103]"), times = 2),
                 Valor = c(13.35, 48.03,  8.57, 36.00))
ggplot(ds, aes(x = Categoria, y = Valor, fill = factor(Edat))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Limfòcits (F28)", y = "Risc de mort (%)") +
  scale_fill_manual(values = c("#006699", "chocolate2")) +
  theme_minimal()+
  geom_hline(yintercept = 50, linetype = "dashed",size = 0.7, color = "black")

####variable troponin
ds <- data.frame(Categoria = rep(c("<0.1", ">=0.1"), each = 2),
                 Edat = rep(c("[45-55)", "[75,103]"), times = 2),
                 Valor = c(9.87, 39.65,  22.77, 63.88))
ggplot(ds, aes(x = Categoria, y = Valor, fill = factor(Edat))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Troponin (F33)", y = "Risc de mort (%)") +
  scale_fill_manual(values = c("#006699", "chocolate2")) +
  theme_minimal()+
  geom_hline(yintercept = 50, linetype = "dashed",size = 0.7, color = "black")

####variable severity
ds <- data.frame(Categoria = rep(c("0-2", "3-4","5-11"), each = 2),
                 Edat = rep(c("[45-55)", "[75,103]"), times = 3),
                 Valor = c(2.51, 13.38,  8.52, 35.85,24.74,66.36))
ggplot(ds, aes(x = Categoria, y = Valor, fill = factor(Edat))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Severity (F11)", y = "Risc de mort (%)") +
  scale_fill_manual(values = c("#006699", "chocolate2")) +
  theme_minimal()+
  geom_hline(yintercept = 50, linetype = "dashed",size = 0.7, color = "black")


####grafics variable LOS

####variable bun
library(ggplot2)
ds <- data.frame(Categoria = rep(c("<=30", ">30"), each = 2),
                 Edat = rep(c("[45-55)", "[75,103]"), times = 2),
                 Valor = c(34.89,32.13,34.55,32.27))  

ggplot(ds, aes(x = Categoria, y = Valor, fill = factor(Edat))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Nitrogen Ureic Sanguini (F21)", y = "Probabilitat (%)") +
  scale_fill_manual(values = c("#006699", "chocolate2")) +
  theme_minimal()


####variable pvd
library(ggplot2)
ds <- data.frame(Categoria = rep(c("0", "1"), each = 2),
                 Edat = rep(c("[45-55)", "[75,103]"), times = 2),
                 Valor = c(34.37,34.91,33.15,33.99))  

ggplot(ds, aes(x = Categoria, y = Valor, fill = factor(Edat))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Malaltia vascular perifèrica (F13)", y = "Probabilitat (%)") +
  scale_fill_manual(values = c("#006699", "chocolate2")) +
  theme_minimal()


