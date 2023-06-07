
########################### Xènia Nicolau Casademont- 1563614 ###############################
################################ TRACTAMENT DE LES DADES ####################################

library(readxl)
library(dplyr)
setwd("C:/Users/xènia/Desktop/gea/4tcurs/tfg")
dades<- read_excel("Mortality_incidence_sociodemographic_and_clinical_data_in_COVID19_patients.xlsx")
summary(dades)


los<-function(data){
  v<-c();cnt<-0
  for(i in 1:nrow(dades)){
    if (dades[i,2]==0 & dades[i,3]!= 0){
      v[cnt]<-i
    } else if(dades[i,2]==1& dades[i,3]<=0){ #el temps no pot ser negatiu
      v[cnt]<-i
    } 
  }
  print(any(v!='NULL'))
} #variables los,los_y
los(dades) #tots els 0 de la variable los_y son desconeguts a la variable los
summary(dades$LOS)

dades<-dades%>%
  mutate (Los = case_when (LOS==0 ~'unknown',
                           LOS <=3  ~ '1-3 days',
                           LOS>=4 & LOS<=7 ~ '4-7 days',
                           LOS>7 & LOS<=14 ~ '1-2 weeks ',
                           LOS>14 ~ '>2 weeks '))
taula<-table(dades$Los)
prop.table(taula)*100
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1-3 days"] / t) * 100;p_1 
p_2 <- (taula["4-7 days"] / t) * 100;p_2
p_3 <- (taula["1-2 weeks "] / t) * 100;p_3
p_4 <- (taula[">2 weeks "] / t) * 100;p_4

taula<-table(dades$Death)
prop.table(taula)*100
dades<-dades%>%mutate(severity= case_when(dades$Severity==0 ~'0-2',dades$Severity==1 ~'0-2', dades$Severity==2 ~'0-2',
                                          dades$Severity==3 ~'3-4',dades$Severity==4 ~'3-4',
                                          dades$Severity==5 ~'5-11',dades$Severity==6 ~'5-11',dades$Severity==7 ~'5-11',
                                          dades$Severity==8 ~'5-11',dades$Severity==9 ~'5-11',dades$Severity==10 ~'5-11',
                                          dades$Severity==11 ~'5-11'))
dades$severity<-as.factor(dades$severity)

dades<-dades%>%
  mutate (race = case_when (Black ==1 & Latino==0 ~ 'black',
                            White==1& Latino==0 ~ ' white ',
                            Asian==1 & Latino==0 ~ ' asian ',
                            Latino==1~'latino',
                            Black==0& Latino==0 & White==0 & Asian==0 ~'unknown'))

dades$PVD<-as.factor(dades$PVD)
dades<-dades%>%mutate(pvd= case_when(dades$PVD==2 ~'1',dades$PVD==1 ~'1', dades$PVD==0~'0'))


age<-cut(dades$Age...27, breaks = c(0, 44, 54, 64, 74,120))
levels(age) <- c("[0,45)", "[45-55)", "[55-65)", "[65-75)", "[75,103]")
dades<-cbind(dades, age)
plot(dades$age,dades$Age...27,main='Discretització variable Age',xlab='variable Age discretitzada',ylab='variable Age')
dades<-dades%>% select (-LOS_Y,-'Derivation cohort', -PVD,-OldSyncope,-OldOtherNeuro,-OtherBrnLsn,-Black,-White,-Asian,-Latino,-Severity,- 'DM Complicated', - 'Pure CNS',-Age...5,-Age...27,-AgeScore)


####grups de tres variables



O2<-dades[,13:15]
o2<-function(data){
  matriu= matrix(nrow = nrow(data),ncol=1)
  matriu2= matrix(nrow = nrow(data),ncol=1)
  for (i in 1:nrow(data)){
    if(data[i,1]==0){
      cas<-which(data[i,2]!=0| data[i,2]==0)
      data[i,2][cas]<-3
      cas2.1<-which(data[i,2]==3& (data[i,3]==0 |data[i,3]==1))           
      data[i,3][cas2.1]<-3
    }else if (data[i,1]==1){
      cas3<-which(data[i,2]==0 )
      data[i,2][cas3]<-3
      cas4<-which(data[i,2]==3& (data[i,3]==0 |data[i,3]==1))
      data[i,3][cas4]<-3
    }else if (data[i,2]<94){
      cas5<-which(dades[i,3]!=0)
      data[i,3][cas5]<-3
    }else if(dades[i,2]>=94){
      cas6<-which(dades[i,3]!=1)
      data[i,3][cas6]<-3
    }
    matriu[i]<-data[i,3]
    matriu2[i]<-data[i,2]
  }
  llista<-list('columna2'=matriu2,'columna3'=matriu)
  return (llista)
}

valors<-o2(O2)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 3] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 3] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'O2Sat<94'=matriu_3)
taula<-table(dades$`O2Sat<94`)
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 



t<-dades[,16:18]
temp<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-3
      cas2.1<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-3 
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-3
      cas6<-which(dades[i,2]< 27 )
      dades[i,2][cas6]<-3
      cas4.1<-which(dades[i,2]> 46)
      dades[i,2][cas4.1]<-3
      cas5<-which(dades[i,2]>38 & dades[i,3]==0)
      dades[i,3][cas5]<-3
      cas7<-which(dades[i,2]<38& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-3
      cas4<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-3
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}

valors<-temp(t)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 3] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 3] <- 'unknown'
table(matriu_3)


dades<-cbind(dades,'temperatura'=matriu_2)

dades<-dades%>%
  mutate (Temp. = case_when (temperatura =='unknown'  ~ 'unknown',
                             temperatura <38  ~ '<38',
                             temperatura >=38 &temperatura <= 39  ~ '38-39',
                             temperatura >39  ~ '>39'))
taula<-table(dades$Temp.)
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["<38"] / t) * 100;p_1 
p_2 <- (taula["38-39"] / t) * 100;p_2
p_3 <- (taula[">39"] / t) * 100;p_3
matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 



m<-dades[,19:21]
map<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0 | dades[i,2]==0)
      dades[i,2][cas]<-3
      cas2.1<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-3 
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-3
      cas6<-which(dades[i,2]< 30 )
      dades[i,2][cas6]<-3
      cas4<-which(dades[i,2]==3 & (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-3
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}

valors<-map(m)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 3] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 3] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'map<70'=matriu_3)
taula<-table(dades$'map<70')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 



d_dimer<-dades[,22:24]
ddimer<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-99
      cas2.1<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-99
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-99
      cas5<-which(dades[i,2]>3 & dades[i,3]==0)
      dades[i,3][cas5]<-99
      cas7<-which(dades[i,2]<3& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-99
      cas4<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-99
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}

valors<-ddimer(d_dimer)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 99] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 99] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'ddimer>3'=matriu_3)
taula<-table(dades$'ddimer>3')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 



plts2<-dades[,25:26]
plts<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-3
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-3
    }
    matriu[i]<-dades[i,2]
  }
  return(matriu)
}

dades<-cbind(dades,'plts'=plts(plts2))
dades$plts[dades$plts==3]<-'unknown'
dades<-dades%>%
  mutate (Plts. = case_when ( plts=='unknown'  ~ 'unknown',
                              plts<150  ~ '<150',
                              plts >=150 &plts <= 450  ~ '150-450',
                              plts >450  ~ '>450'))
var(dades$plts,na.rm = T) 
taula<-table(dades$Plts.)
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["<150"] / t) * 100;p_1 
p_2 <- (taula["150-450"] / t) * 100;p_2
p_3 <- (taula[">450"] / t) * 100;p_3

dades$plts<-as.numeric(dades$plts)
summary(dades$plts)




INR<-dades[,28:30]
inr<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-99
      cas2.1<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-99
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-99
      cas5<-which(dades[i,2]>1.2 & dades[i,3]==0)
      dades[i,3][cas5]<-99
      cas7<-which(dades[i,2]<1.2& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-99
      cas4<-which(dades[i,2]==22& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-99
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columan1'=matriu,'columna2'=matriu2)
  return(llista)
}

valors<-inr(INR)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 99] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 99] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'inr'=matriu_3)
taula<-table(dades$'inr')
table(matriu_3)
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 





BUN<-dades[,31:33]
bun<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-3
      cas2.1<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-3 
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-3
      cas5<-which(dades[i,2]>30 & dades[i,3]==0)
      dades[i,3][cas5]<-3
      cas7<-which(dades[i,2]<30& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-3
      cas4<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-3
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}

valors<-bun(BUN)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 3] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 3] <- 'unknown'

dades<-cbind(dades,'bun'=matriu_3)
taula<-table(dades$'bun')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 
matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 


Creatinine<-dades[,34:35]
creatinine<-function(dades){
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0 | dades[i,2]==0)
      dades[i,2][cas]<-99
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-99
    }
    matriu2[i]<-dades[i,2]
  }
  return(matriu2)
}

dades<-cbind(dades,'creatinine'=creatinine(Creatinine))
dades$creatinine[dades$creatinine==99]<-'unknown'
dades<-dades%>%
  mutate (creatinine. = case_when ( creatinine=='unknown'  ~ 'unknown',
                                    creatinine<1.5  ~ '<1.5',
                                    creatinine >=1.5 &creatinine <= 2.5  ~ '1.5-2.5',
                                    creatinine >2.5  ~ '>2.5'))
dades$creatinine<-as.numeric(dades$creatinine)
summary(dades$creatinine)
taula<-table(dades$creatinine.)
var(dades$creatinine,na.rm = T) 
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["<1.5"] / t) * 100;p_1 
p_2 <- (taula["1.5-2.5"] / t) * 100;p_2 
p_3 <- (taula[">2.5"] / t) * 100;p_3 




Sodium<-dades[,37:39]
dades$Sodium[dades$Sodium=='170.001']<-170
sodium<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-99
      cas2.1<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-99
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-99
      cas4<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-99
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'= matriu2)
  return(llista)
}
valors<-sodium(Sodium)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 99] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 99] <- 'unknown'

dades<-cbind(dades,'sodium<139 or >154'=matriu_3)
taula<-table(dades$'sodium<139 or >154')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 
matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 



Glucose<-dades[,40:42]
glucose<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-3
      cas2.1<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-3
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-3
      cas6<-which(dades[i,2]< 30 )
      dades[i,2][cas6]<-3
      cas4.1<-which(dades[i,2]> 1000)
      dades[i,2][cas4.1]<-3
      cas4<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-3
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'= matriu2)
  return(llista)
}

valors<-glucose(Glucose)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 3] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 3] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'glucose<60 o >500'=matriu_3)
taula<-table(dades$'glucose<60 o >500')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 





AST<-dades[,43:45]
ast<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-3
      cas2.1<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-3 
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-3
      cas5<-which(dades[i,2]>40 & dades[i,3]==0)
      dades[i,3][cas5]<-3
      cas7<-which(dades[i,2]<40& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-3
      cas4<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-3
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}

valors<-ast(AST)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 3] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 3] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'ast>40'=matriu_3)
taula<-table(dades$'ast>40')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 




ALT<-dades[,46:48]
alt<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-3
      cas2.1<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-3 
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-3
      cas5<-which(dades[i,2]>38 & dades[i,3]==0)
      dades[i,3][cas5]<-3
      cas7<-which(dades[i,2]<38& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-3
      cas4<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-3
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}
valors<-alt(ALT)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 3] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 3] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'alt>40'=matriu_3)
taula<-table(dades$'alt>40')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 


WBC<-dades[,49:51]
wbc<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-99
      cas2.1<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-99
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-99
      cas4<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-99
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}
valors<-wbc(WBC)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 99] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 99] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'wbc<1.8 or >4.8'=matriu_3)
taula<-table(dades$'wbc<1.8 or >4.8')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 



Lympho<-dades[,52:54]
lympho<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-99
      cas2.1<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-99
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-99
      cas5<-which(dades[i,2]<1 & dades[i,2]!= 0 & dades[i,3]==0)
      dades[i,3][cas5]<-99
      cas7<-which(dades[i,2]>1& dades[i,3]==1)
      dades[i,3][cas7]<-99
      cas4<-which(dades[i,2]==32& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-99
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}
valors<-lympho(Lympho)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 99] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 99] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'lympho<1'=matriu_3)
taula<-table(dades$'lympho<1')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 



IL6<-dades[,55:57]
il6<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-3
      cas2.1<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-3 
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-3
      cas5<-which(dades[i,2]>150 & dades[i,3]==0)
      dades[i,3][cas5]<-3
      cas7<-which(dades[i,2]<150& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-3
      cas4<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-3
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}
valors<-il6(IL6)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 3] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 3] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'il6>150'=matriu_3)
taula<-table(dades$'il6>150')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 



Ferritin<-dades[,58:60]
ferritin<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-3
      cas2.1<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-3
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-3
      cas5<-which(dades[i,2]>300 & dades[i,3]==0)
      dades[i,3][cas5]<-3
      cas7<-which(dades[i,2]<300& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-3
      cas4<-which(dades[i,2]==3& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-3
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}
valors<-ferritin(Ferritin)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 3] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 3] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'ferritin>300'=matriu_3)
taula<-table(dades$'ferritin>300')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 




crctprotein<-dades[,61:63]
protein<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-99
      cas2.1<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-99
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-99
      cas6<-which(dades[i,2]>80 )
      dades[i,2][cas6]<-99
      cas5<-which(dades[i,2]>10 & dades[i,3]==0)
      dades[i,3][cas5]<-99
      cas7<-which(dades[i,2]<10& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-99
      cas4<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-99
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}
valors<-protein(crctprotein)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 99] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 99] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'crct_protein>10'=matriu_3)
taula<-table(dades$'crct_protein>10')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 




procalcitonin<-dades[,64:66]
procal<-function(dades){
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  matriu= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-99
      cas2.1<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-99 
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-99
      cas5<-which(dades[i,2]>0.1 & dades[i,3]==0)
      dades[i,3][cas5]<-99
      cas7<-which(dades[i,2]<0.1& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-99
      cas4<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-99
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
}
valors<-procal(procalcitonin)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 99] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 99] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'procalcitonin>0.1'=matriu_3)
taula<-table(dades$'procalcitonin>0.1')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 


Troponin<-dades[,67:69]
troponin<-function(dades){
  matriu= matrix(nrow = nrow(dades),ncol=1)
  matriu2= matrix(nrow = nrow(dades),ncol=1)
  for (i in 1:nrow(dades)){
    if(dades[i,1]==0){
      cas<-which(dades[i,2]!=0| dades[i,2]==0)
      dades[i,2][cas]<-99
      cas2.1<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))           
      dades[i,3][cas2.1]<-99 
    }else if (dades[i,1]==1){
      cas3<-which(dades[i,2]==0 )
      dades[i,2][cas3]<-99
      cas5<-which(dades[i,2]>0.1 & dades[i,3]==0)
      dades[i,3][cas5]<-99
      cas7<-which(dades[i,2]<0.1& dades[i,2]!= 0  & dades[i,3]==1)
      dades[i,3][cas7]<-99
      cas4<-which(dades[i,2]==99& (dades[i,3]==0 |dades[i,3]==1))
      dades[i,3][cas4]<-99
    }
    matriu[i]<-dades[i,2]
    matriu2[i]<-dades[i,3]
  }
  llista<-list('columna2'=matriu,'columna3'=matriu2)
  return(llista)
  
}

valors<-troponin(Troponin)
matriu_2 <- valors[[1]]
matriu_2[matriu_2 == 99] <- 'unknown'
matriu_3 <- valors[[2]]
matriu_3[matriu_3 == 99] <- 'unknown'

matriu_2<-as.numeric(matriu_2)
summary(matriu_2)
var(matriu_2,na.rm = T) 
dades<-cbind(dades,'troponin>0.1'=matriu_3)
taula<-table(dades$'troponin>0.1')
prop.table(taula)*100 #percentatge NA
t<- sum(taula) - taula["unknown"]
p_1 <- (taula["1"] / t) * 100;p_1 
p_0 <- (taula["0"] / t) * 100;p_0 



dades<-dades%>%select(-LOS,-c(13:69),-temperatura,-creatinine,-plts)


proptable <- apply(apply(dades[2:11], 2, table), 2, prop.table)
proptable<-proptable*100
taula<-table(dades$severity)
prop.table(taula)*100
taula<-table(dades$race)
prop.table(taula)*100
t<- sum(taula) - taula["unknown"]
p_1 <- (taula[" asian "] / t) * 100;p_1 
p_2 <- (taula[" white "] / t) * 100;p_2 
p_3 <- (taula["black"] / t) * 100;p_3
p_4 <- (taula["latino"] / t) * 100;p_4

taula<-table(dades$pvd)
prop.table(taula)*100

library(openxlsx)
library(writexl)
write.csv(dades, file="dades.csv", row.names = FALSE)
