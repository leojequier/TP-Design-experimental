
rm(list=ls())
#install.packages("pracma")
library("pracma")

#charger les données, après avoir fait session, set working directory, to source file location
path = "Data_W1_csv2/T"
List = vector(mode = "list", length = 8) #sorte de liste qui peut contenir des variables
csv = c()
for(i in 1:8){
  List[[i]]= paste("T" , i-1, sep = "")
  csv[i] = paste(path,i-1,".csv", sep = "")
  assign(paste("T" , i-1, sep = ""), read.csv(csv[i], header = T)) 
}

#echanger A5,6,7 avec E5,6,7 pour T1
x1 = T1[40:48,]
y1 = T1[64:72,]
T1[40:48,] = y1
T1[64:72,] = x1
T1$Well.ID[40:48] = rep(c("A5", "A6", "A7"),times = 3)
T1$Well.ID[64:72] = rep(c("E5", "E6", "E7"),times = 3)

#t0 % 10 et T1
T0$All.Abs..Count = T0$All.Abs..Count
T0$mChe.SYTO.9.Abs..Count = T0$mChe.SYTO.9.Abs..Count
T0$SYTO.9.Abs..Count = T0$SYTO.9.Abs..Count
T1$All.Abs..Count = T1$All.Abs..Count*10
T1$mChe.SYTO.9.Abs..Count = T1$mChe.SYTO.9.Abs..Count*10
T1$SYTO.9.Abs..Count = T1$SYTO.9.Abs..Count*10

#T2, 3, 4, 5, 6, 7, 8 dilution adjustment

T2$All.Abs..Count = T2$All.Abs..Count*100
T2$mChe.SYTO.9.Abs..Count = T2$mChe.SYTO.9.Abs..Count*100
T2$SYTO.9.Abs..Count = T3$SYTO.9.Abs..Count*100
T3$All.Abs..Count = T3$All.Abs..Count*100
T3$mChe.SYTO.9.Abs..Count = T3$mChe.SYTO.9.Abs..Count*100
T3$SYTO.9.Abs..Count = T3$SYTO.9.Abs..Count*100
T4$All.Abs..Count = T4$All.Abs..Count*100
T4$mChe.SYTO.9.Abs..Count = T4$mChe.SYTO.9.Abs..Count*100
T4$SYTO.9.Abs..Count = T4$SYTO.9.Abs..Count*100
T5$All.Abs..Count = T5$All.Abs..Count*100
T5$mChe.SYTO.9.Abs..Count = T5$mChe.SYTO.9.Abs..Count*100
T5$SYTO.9.Abs..Count = T5$SYTO.9.Abs..Count*100
T6$All.Abs..Count = T6$All.Abs..Count*100
T6$mChe.SYTO.9.Abs..Count = T6$mChe.SYTO.9.Abs..Count*100
T6$SYTO.9.Abs..Count = T6$SYTO.9.Abs..Count*100
T7$All.Abs..Count = T7$All.Abs..Count*100
T7$mChe.SYTO.9.Abs..Count = T7$mChe.SYTO.9.Abs..Count*100
T7$SYTO.9.Abs..Count = T7$SYTO.9.Abs..Count*100




#noms de traitements: Toluène vs mix C, SC vs PP vs PPSC, replicat
mil = rep(c("MixC","Tolu" ), each = 9)
tre = rep(rep(c("SC", "PP", "PPSC"), each = 3),times = 2)
n = rep(c(1,2,3), times= 6)
names = c()
for(i in 1:18){
  names[i] = paste(mil[i], tre[i], n[i], sep = "")
}

#crer un data frame pour acceuilir les données dans le mauvais sens
for(i in 1:8){
  assign(paste("t_", i-1, sep=""), rep(NA, times = 18))
}
syto = data.frame(names,t_0, t_1, t_2, t_3, t_4, t_5, t_6, t_7)

###------------------------------------------------------------------ SYTO-9
##créer un data frame pour acceuilir les données dans le bon sens
#créer une liste contenant les noms de traitement et leur assigner un vecteur 8xNA 
List_treat = vector(mode = "list", length = 18)
for(i in 1:18){
  List_treat[[i]] = names[i]
  assign(List_treat[[i]], rep(NA, times = 8))
}

#vecteur contenant le temps en heure
time = c(0, 11, 15 + 1/6, 19.5,22.5,38.5 , 45+ 1/3 ,48 )

#creer le dataframe a proprement parler
syto2 = data.frame(time, MixCPP1)
for(i in 2:18){
  data = get(List_treat[[i]])
  syto2[,i+1] = data
}
names(syto2) = c("time", List_treat)

#créer un vecteur contenant les noms des puits d'intérêt
let = rep(c(rep("A", times = 3),rep("C", times = 3),rep("E", times = 3)),times = 2)
chi = c(rep(c(1,2,3), times = 3), rep(c(5,6,7),times= 3))
lech = c()
for(i in 1:18){
  lech[i] = paste(let[i], chi[i], sep = "")
} 

#remplir avec SYTO 9 pour tous les traitements
for(i in 1:8){ 
  for(j in 1:18){
    Data = get(List[[i]]) #stocke T[I]
    print(lech[j]) 
    syto2[i,j+1] = mean(Data$SYTO.9.Abs..Count[Data$Well.ID== lech[j]][1:2])
    #parcours syto 2 en ligne, prend les comptes absolu syto9 de T[i] qui ont le nom lech[j] (deux replicat tech) et fait une moyenne 
  }}

#graphiques SYTO-9
par(mfrow = c(3,3))
for(i in 2:10){
  plot(syto2[,1],syto2[,i], log = "y", ylim=c(min(syto2[,2:19]), max(syto2[,2:19])),main = List_treat[[i-1]], xlab = "time[h]", ylab = "SYTO-9 count")
}

for(i in 11:19){
  plot(syto2[,1],syto2[,i], log = "y", ylim=c(min(syto2[,2:19]), max(syto2[,2:19])), main = List_treat[[i-1]],xlab = "time[h]", ylab = "SYTO-9 count")
}

###------------------------------------------------------------------m-che 
#creer data frame pour acceuilir les données

cher = data.frame(time, List_treat)
for(i in 1:18){
  data = get(List_treat[[i]])
  cher[,i+1] = data
}
names(cher) = c("time", List_treat)

#remplir le Data frame
for(i in 1:8){
  for(j in 1:18){
    Data = get(List[[i]])
    print(lech[j])
    cher[i,j+1] = mean(Data$mChe.SYTO.9.Abs..Count[Data$Well.ID == lech[j]][1:2])
    
  }}

#graphiques
par(mfrow = c(3,3))
for(i in 2:10){
  plot(cher[,1],cher[,i],ylim=c(min(cher[,2:19]), max(cher[,2:19])), main = List_treat[[i-1]], xlab = "time[h]", ylab = "mChe count")
}

for(i in 11:19){
  plot(cher[,1],cher[,i], ylim=c(min(cher[,2:19]), max(cher[,2:19])),main = List_treat[[i-1]],xlab = "time[h]", ylab = "mChe count")
}

### ------------------------------------------------ALL- ABSOLUT COUNT
#créer le data frame pour acceuillir les données
absol = data.frame(time, List_treat)
for(i in 1:18){
  data = get(List_treat[[i]])
  absol[,i+1] = data
}
names(absol) = c("time", List_treat)

#remplir
for(i in 1:8){
  for(j in 1:18){
    Data = get(List[[i]])
    print(lech[j])
    absol[i,j+1] = mean(Data$mChe.SYTO.9.Abs..Count[Data$Well.ID == lech[j]][1:2] + Data$SYTO.9.Abs..Count[Data$Well.ID== lech[j]][1:2])
    
  }}

#graphiques
par(mfrow = c(3,3))
for(i in 2:10){
  plot(absol[,1],absol[,i], ylim = c(min(absol[,2:19]), max(absol[,2:19])), log = "y", main = List_treat[[i-1]], xlab = "time[h]", ylab = "Absolut count")
}

for(i in 11:19){
  plot(absol[,1],absol[,i],  ylim = c(min(absol[,2:19]), max(absol[,2:19])),log = "y", main = List_treat[[i-1]],xlab = "time[h]", ylab = "Absolut count")
}
#means sc
MixC_SCmoy<- c()

for(i in 1:8){
  MixC_SCmoy[i] <- sum(syto2$MixCSC1[time==time[i]],  syto2$MixCSC2[time==time[i]], syto2$MixCSC3[time==time[i]])/3
}

Tol_SCmoy<- c()

for(i in 1:8){
  Tol_SCmoy[i] <- sum(syto2$ToluSC1[time==time[i]],  syto2$ToluSC2[time==time[i]], syto2$ToluSC3[time==time[i]])/3
}

Tol_PPSC_SCmoy <-c()

for(i in 1:8){
  Tol_PPSC_SCmoy[i] <- sum(syto2$ToluPPSC1[time==time[i]],  syto2$ToluPPSC2[time==time[i]], syto2$ToluPPSC3[time==time[i]])/3
}

MixC_PPSC_SCmoy <-c()

for(i in 1:8){
  MixC_PPSC_SCmoy[i] <- sum(syto2$MixCPPSC1[time==time[i]],  syto2$MixCPPSC2[time==time[i]], syto2$MixCPPSC3[time==time[i]])/3
}


# means PP

MixC_PPmoy<- c()

for(i in 1:8){
  MixC_PPmoy[i] <- sum(cher$MixCPP1[time==time[i]],  cher$MixCPP2[time==time[i]],cher$MixCPP3[time==time[i]])/3
}

Tol_PPmoy <- c()
for(i in 1:8){
  Tol_PPmoy[i] <- sum(cher$ToluPP1[time==time[i]],  cher$ToluPP2[time==time[i]],cher$ToluPP3[time==time[i]])/3
}

Tol_PPSC_PPmoy <- c()
for(i in 1:8){
  Tol_PPSC_PPmoy[i] <- sum(cher$ToluPPSC1[time==time[i]],  cher$ToluPPSC2[time==time[i]],cher$ToluPP3[time==time[i]])/3
}
MixC_PPSC_PPmoy <- c()
for(i in 1:8){
  MixC_PPSC_PPmoy[i] <- sum(cher$MixCPPSC1[time==time[i]],  cher$MixCPPSC2[time==time[i]],cher$MixCPP3[time==time[i]])/3
}

#cell count
list_cell_count = vector(mode = "list", length = 33)
list_cell_count[1] = "time"
CvTol = rep(c("MixC", "Tol"), each = 16)
bac = rep(rep(c("SC", "PP", "PPSC_SC","PPSC_PP"), each = 4), time = 2)
n = rep(c(1,2,3,"moy"), times = 8)
for(i in 1:32){
  list_cell_count[i+1] = paste(CvTol[i], paste(bac[i], n[i], sep = ""), sep = "_")
}
cell_count = data.frame(time, syto2$MixCSC1, syto2$MixCSC2, syto2$MixCSC3,MixC_SCmoy,
                        cher$MixCPP1,cher$MixCPP2,cher$MixCPP3,MixC_PPmoy,
                        syto2$MixCPPSC1,syto2$MixCPPSC2,syto2$MixCPPSC3,MixC_PPSC_SCmoy,
                        cher$MixCPPSC1, cher$MixCPPSC2, cher$MixCPPSC3,Tol_PPSC_SCmoy, 
                        syto2$ToluSC1,syto2$ToluSC2,syto2$ToluSC3, Tol_SCmoy,
                        cher$ToluPP1,cher$ToluPP2,cher$ToluPP3, Tol_PPmoy, 
                        syto2$ToluPPSC1, syto2$ToluPPSC2,syto2$ToluPPSC3,Tol_PPSC_SCmoy,
                        cher$ToluPPSC1,cher$ToluPPSC2,cher$ToluPPSC3, Tol_PPSC_PPmoy)
names(cell_count) = list_cell_count
names(cell_count)



MixC_PPSC1 <- cell_count$MixC_PPSC_SC1 + cell_count$MixC_PPSC_PP1
MixC_PPSC2 <- cell_count$MixC_PPSC_SC2 + cell_count$MixC_PPSC_PP2
MixC_PPSC3 <- cell_count$MixC_PPSC_SC3 + cell_count$MixC_PPSC_PP3
MixC_PPSCmoy <- (MixC_PPSC1+ MixC_PPSC2+ MixC_PPSC3)/3

Tol_PPSC1 <- cell_count$Tol_PPSC_SC1 + cell_count$Tol_PPSC_PP1
Tol_PPSC2 <- cell_count$Tol_PPSC_SC2 + cell_count$Tol_PPSC_PP2
Tol_PPSC3 <- cell_count$Tol_PPSC_SC3 + cell_count$Tol_PPSC_PP3
Tol_PPSCmoy <- (Tol_PPSC1+ Tol_PPSC2+ Tol_PPSC3)/3

cell_count <- cbind(cell_count, MixC_PPSC1, MixC_PPSC2,MixC_PPSC3, MixC_PPSCmoy, Tol_PPSC1, Tol_PPSC2, Tol_PPSC3, Tol_PPSCmoy)


#puissance stat 
SC_t6_mixC_syto9 = c(syto2$MixCSC1[syto2$time == 43],syto2$MixCSC2[syto2$time == 43],syto2$MixCSC3[syto2$time == 43] )

PP_t6_mixC_mChe = c(cher$ToluPP1[cher$time == 43],cher$ToluPP2[cher$time == 43],cher$ToluPP3[cher$time == 43] )

SC_t6_Tol_syto9 = c(syto2$ToluSC1[syto2$time == 43],syto2$ToluSC2[syto2$time == 43],syto2$ToluSC3[syto2$time == 43] )

SC_t6_Tol_mChe = c(c(cher$ToluSC1[cher$time == 43],cher$ToluSC2[cher$time == 43],cher$ToluSC3[cher$time == 43]))

PP_t6_Tol_mChe = c(cher$ToluPP1[cher$time == 43],cher$ToluPP2[cher$time == 43],cher$ToluPP3[cher$time == 43])

PP_T6_Tol_syto9 = c(syto2$ToluPP1[syto2$time == 43],syto2$ToluPP2[syto2$time == 43],syto2$ToluPP3[syto2$time == 43])

PPSC_T6_Tol_syto9 = c(syto2$ToluPPSC1[syto2$time == 43],syto2$ToluPPSC2[syto2$time == 43],syto2$ToluPPSC3[syto2$time == 43])

PPSC_T6_Tol_mChe = c(cher$ToluPPSC1[cher$time == 43],cher$ToluPPSC2[cher$time == 43],cher$ToluPPSC3[cher$time == 43])

t.test(PPSC_T6_Tol_syto9,SC_t6_Tol_syto9, var.equal = T)
power.t.test(n = 3, delta = mean(PPSC_T6_Tol_syto9) - mean(SC_t6_Tol_syto9), sd = sd(c(SC_t6_Tol_syto9, PPSC_T6_Tol_syto9)))
mean(PPSC_T6_Tol_syto9)

mean(PP_T6_Tol_syto9)

mean(PPSC_T6_Tol_mChe)

mean(PP_T6_Tol_syto9)

var(SC_t6_mixC_syto9)

var(PP_t6_Tol_mChe)

var(PP_t6_mixC_mChe)

var(SC_t6_Tol_syto9)

#graphiques



par(mfrow = c(1,1))

totsc <- data.frame(time, MixC_SCmoy, Tol_SCmoy, MixC_PPSC_SCmoy, Tol_PPSC_SCmoy)
Tol_deadPPmoy<- c()
#ppmortes dans ppseul

for(i in 1:8){
  Tol_deadPPmoy[i] <- sum(syto2$ToluPP1[time==time[i]],  syto2$ToluPP2[time==time[i]],syto2$ToluPP3[time==time[i]])/3
}

#total dans ppseul
Tol_all_in_pp = c()
for(i in 1:8){
  Tol_all_in_pp[i]  <- Tol_deadPPmoy[i] + Tol_PPmoy[i]
}

#Plot des comptes de SC
par(mfrow = c(1,1))
plot(totsc$time, totsc$MixC_SCmoy,log="y",xlim=c(0,48),ylim=c(min(Tol_deadPPmoy), max(cell_count$Tol_PPSCmoy)), type="o",main="SC growth",xlab= "Time [Hours]", ylab="log(SC count)")

points(totsc$time,totsc$Tol_SCmoy, type="o", col="red")
points(totsc$time,totsc$MixC_PPSC_SCmoy, type="o", col="blue")
points(totsc$time,totsc$Tol_PPSC_SCmoy, type="o", col="orange")
points(totsc$time, Tol_deadPPmoy, type="o", col="green")
points(totpp$time, cell_count$MixC_PPSCmoy, type="o", col="brown")
points(totpp$time, cell_count$Tol_PPSCmoy, type="o", col="gray")
points(totpp$time, Tol_all_in_pp, type = "o", col = "pink")
legend("bottomright",legend = c( "Syt9 cnt in Tol PPalone", "PPSC total in mixC", "PPSC total in Tol", "total count in pp alone"),fill=c("green", "brown", "gray", "pink"))
legend("bottom", legend=c("SC in mixC","SC in Tol","SC in PPSC in mixC","SC in PPSC in Tol"),fill=c("black","red","blue","orange"))
      
  #SAME WITH PP

#plot par réplicats ATTENTION jsuis pas du tout sûr si c'est les bonnes données :3
par(mfrow = c(1,3))
plot(totsc$time, absol$MixCSC1,log="y",xlim=c(0,48),ylim=c(min(Tol_deadPPmoy), max(cell_count$Tol_PPSCmoy)), type="o",main="SC growth (1)",xlab= "Time [Hours]", ylab="log(SC count)")

points(totsc$time,absol$ToluSC1, type="o", col="red")
points(totsc$time,absol$MixCPPSC1, type="o", col="blue")
points(totsc$time,absol$ToluPPSC1, type="o", col="orange")
legend("bottom", legend=c("SC in mixC","SC in Tol","SC in PPSC in mixC","SC in PPSC in Tol"),fill=c("black","red","blue","orange"))
#réplicat 2
plot(totsc$time, absol$MixCSC2,log="y",xlim=c(0,48),ylim=c(min(Tol_deadPPmoy), max(cell_count$Tol_PPSCmoy)), type="o",main="SC growth (2)",xlab= "Time [Hours]", ylab="log(SC count)")

points(totsc$time,absol$ToluSC2, type="o", col="red")
points(totsc$time,absol$MixCPPSC2, type="o", col="blue")
points(totsc$time,absol$ToluPPSC2, type="o", col="orange")
legend("bottom", legend=c("SC in mixC","SC in Tol","SC in PPSC in mixC","SC in PPSC in Tol"),fill=c("black","red","blue","orange"))

#réplicat 3
plot(totsc$time, absol$MixCSC1,log="y",xlim=c(0,48),ylim=c(min(Tol_deadPPmoy), max(cell_count$Tol_PPSCmoy)), type="o",main="SC growth (3)",xlab= "Time [Hours]", ylab="log(SC count)")

points(totsc$time,absol$ToluSC3, type="o", col="red")
points(totsc$time,absol$MixCPPSC3, type="o", col="blue")
points(totsc$time,absol$ToluPPSC3, type="o", col="orange")
legend("bottom", legend=c("SC in mixC","SC in Tol","SC in PPSC in mixC","SC in PPSC in Tol"),fill=c("black","red","blue","orange"))



totpp <- data.frame(time, MixC_PPmoy, Tol_PPmoy, MixC_PPSC_PPmoy, Tol_PPSC_PPmoy)
plot(totpp$time, totpp$MixC_PPmoy,log="y",xlim=c(0,48),ylim=c(min(Tol_deadPPmoy), max(cell_count$Tol_PPSCmoy)), type="o",main="PP growth",xlab= "Time [Hours]", ylab="log(PP count)")

#Plot des comptes de PP

points(totpp$time,totpp$Tol_PPmoy, type="o", col="red")
points(totpp$time,totpp$MixC_PPSC_PPmoy, type="o", col="blue")
points(totpp$time,totpp$Tol_PPSC_PPmoy, type="o", col="orange")
points(totpp$time, cell_count$MixC_PPSCmoy, type="o", col="brown")
points(totpp$time, cell_count$Tol_PPSCmoy, type="o", col="gray")
legend("bottomright",legend=c("PP in mixC","PP in Tol","PP in PP+SC in mixC","PP in PP+SC in Tol", "PPSC total in mixC", "PPSC total in Tol"),fill=c("black","red","blue","orange", "brown", "gray"))

#estimer l'erreur des réplicats technique 
#mentionner les problèmes avec le 3èmes réplicats techniques
#erreur type du flow cytometer ? 

#paramètres importants, pls colonnes : SCw1 or w2, Replicat, PV, PP, SC, AIre sous la courbe
#il aurait fallu faire mixC+tol
#cmb de ug de carbon dans le tol et dans le mixC? 




#---------------AREA under a CURVE --> WESH CA MARCHE
#---- INSTALL.PACKAGES("PRACMA") 




##GENERAL LINAR MODEL TABLE

Replicat <- rep(c("1", "2", "3"),20) 
week_on_SC <- c(rep(c(rep(c("1"),time=3), rep(c("0"), time=12)),time=2),rep(c(rep(c("2"),time=3), rep("0", time=12)),time=2))
SCweek1 = c(rep(c(rep(c("1"),time=3), rep(c("0"), time=12)),time=2), rep("0", times = 30))
SCweek2 = c(rep("0", times = 30),rep(c(rep(c("1"),time=3), rep(c("0"), time=12)),time=2))
Substrate<-rep(c("MixC", "Tol"), each=15, time=2)
PV <- c(rep(c("0"), 30),rep(c("0"), 3), rep(c("1"), 12),rep(c("0"), 3), rep(c("1"), 12))
PP <- c(rep(c("0"), 3), rep(c("1"), 12),rep(c("0"), 3), rep(c("1"), 12), rep(c("0"), 30))
SC <- rep(c(rep(c("1","0"), each=3),rep(c("1"), time=9)),time=4)
PPSC <- c(rep(c(rep(c("0"),6), rep(c("1"),9)), time=2 ),rep(c("0"), time=30))
PVSC <- c(rep(c("0"), time=30),rep(c(rep(c("0"),6), rep(c("1"),9)), time=2 ))
AUC <- vector(length = 60)


names1 <- c()
names2 <- c()
CvTol = rep(c("MixC", "Tol"), each = 15, time=2)
bac = c(rep(rep(c("SC", "PP", "PPSC_SC","PPSC_PP", "PPSC_tot"), each = 3), time = 2),rep(rep(c("SC", "PV", "PVSC_SC","PVSC_PV", "PVSC_tot"), each = 3), time = 2))
n = c(rep(c(1,2,3), times = 10), rep(c("1_1","2_1","3_1"), times=10))
for(i in 1:60){
  names1[i] = paste(CvTol[i], paste(bac[i], n[i], sep = ""), sep = "_")
  names2[i] = bac[i]
}


week <- data.frame(names1, names2,Substrate,week_on_SC, SCweek1,Replicat,PV, PP,SC,PPSC, PVSC,AUC)

## Table avec les mêmes noms dans le même ordre que week
time_w1 = c(0, 11, 15 + 1/6, 19.5,22.5,38.5 , 45+ 1/3 ,48 )
time_w2 = c(0, 14.5, 18, 21, 23.5, 36.5, 42 ,44.5 , 47.5 )

ess <- read.table("ess.txt", sep="\t", header=T)
ess2 <- read.table("ess2.txt", header = T)

## Aire sous la courbe
for(i in 1:30){
  week$AUC[i] = trapz(time_w1,ess[,i])
}

for(i in 31:60){
  week$AUC[i] = trapz(time_w2, ess2[,i-30])
}

summary(glm(log(week$AUC) ~ as.factor(week$week_on_SC)+ as.factor(week$names2)+as.factor(week$Substrate)+as.factor(week$Replicat)))
## que les réplicat ne sont pas important, n'ont pas d'influence, que il n'y a pas de différence entre le SC d'une semaine à l'autre.

##test week 1
model_week_1 = aov(log(week$AUC[week$names1 %in% week$names1[c(1:3, 7:9, 16:18, 22:24)]]))~ )
                   
                  

##Anova
model = aov(log(week$AUC)~week$Substrate*as.factor(week$names2))
anova(model)
#le substrat a un effet significatif, les échantillons ayant poussé dans des substrats différents sont en moyenne différents.
#au moins un traitement à un effet significatif,
#graphes
par(mfrow = c(1,1))

bac2 = c(rep(rep(c("SC1", "PP", "PPSC_SC","PPSC_PP", "PPSC_tot"), each = 3), time = 2),rep(rep(c("SC2", "PV", "PVSC_SC","PVSC_PV", "PVSC_tot"), each = 3), time = 2))
CvTol2 = rep(c("MixC", "Tol"), each = 15, time=2)
eti = c()
for(i in 1:60){
  eti[i] = paste(CvTol2[i], bac2[i],sep = "_")
}

plot(rep(1:10, each = 3), log(week$AUC[1:30]), xlab = "", xaxt = "n", ylab = "Log AUC", main = "Valeurs de AUC\n dans chaque traitement\nsemaine1", pch = c(1,2,3))
axis(1, at = 1:10, labels = unique(eti[1:30]), las = 2, hadj = T)

plot(rep(1:10, each = 3), log(week$AUC[31:60]), xlab = "",  xaxt = "n", ylab = "Log AUC", main = "Valeurs de AUC\n dans chaque traitement\nsemaine2", pch = c(1,2,3))
axis(1, at = 1:10, labels = unique(eti[31:60]), las = 2, hadj = T)

