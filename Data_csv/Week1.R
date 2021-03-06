
rm(list=ls())
#install.packages("pracma")
#install.packages("car")
library("car")
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


#means sc fluo vert(G) et rouge(R)
MixC_SCmoy_G<- c()

for(i in 1:8){
  MixC_SCmoy_G[i] <- sum(syto2$MixCSC1[i],  syto2$MixCSC2[i], syto2$MixCSC3[i])/3
}

MixC_SCmoy_R<- c()

for(i in 1:8){
  MixC_SCmoy_R[i] <- sum(cher$MixCSC1[i],  cher$MixCSC2[i], cher$MixCSC3[i])/3
}


Tol_SCmoy_G<- c()

for(i in 1:8){
  Tol_SCmoy_G[i] <- sum(syto2$ToluSC1[i],  syto2$ToluSC2[i], syto2$ToluSC3[i])/3
}

Tol_SCmoy_R<- c()

for(i in 1:8){
  Tol_SCmoy_R[i] <- sum(cher$ToluSC1[i],  cher$ToluSC2[i], cher$ToluSC3[i])/3
}

Tol_PPSCmoy_G <-c()

for(i in 1:8){
  Tol_PPSCmoy_G[i] <- sum(syto2$ToluPPSC1[i],  syto2$ToluPPSC2[i], syto2$ToluPPSC3[i])/3
}

Tol_PPSCmoy_R <-c()

for(i in 1:8){
  Tol_PPSCmoy_R[i] <- sum(cher$ToluPPSC1[i],  cher$ToluPPSC2[i], cher$ToluPPSC3[i])/3
}

MixC_PPSCmoy_G <-c()

for(i in 1:8){
  MixC_PPSCmoy_G[i] <- sum(syto2$MixCPPSC1[i],  syto2$MixCPPSC2[i], syto2$MixCPPSC3[i])/3
}

MixC_PPSCmoy_R <-c()

for(i in 1:8){
  MixC_PPSCmoy_R[i] <- sum(cher$MixCPPSC1[i],  cher$MixCPPSC2[i], cher$MixCPPSC3[i])/3
}

# means PP

MixC_PPmoy_R<- c()

for(i in 1:8){
  MixC_PPmoy_R[i] <- sum(cher$MixCPP1[i], cher$MixCPP2[i],cher$MixCPP3[i])/3
}

MixC_PPmoy_G<- c()

for(i in 1:8){
  MixC_PPmoy_G[i] <- sum(syto2$MixCPP1[i],  syto2$MixCPP2[i],syto2$MixCPP3[i])/3
}

Tol_PPmoy_R <- c()
for(i in 1:8){
  Tol_PPmoy_R[i] <- sum(cher$ToluPP1[i],  cher$ToluPP2[i],cher$ToluPP3[i])/3
}

Tol_PPmoy_G <- c()
for(i in 1:8){
  Tol_PPmoy_G[i] <- sum(syto2$ToluPP1[i],  syto2$ToluPP2[i],syto2$ToluPP3[i])/3
}

#cell count
list_cell_count = vector(mode = "list", length = 49)
list_cell_count[1] = "time"
CvTol = rep(c("MixC", "Tol"), each = 24)
bac = rep(rep(c("SC", "PP","PPSC"), each = 8), time = 2)
Fluo = rep(rep(c("G", "R"), each = 4), time = 6)
n = rep(c(1,2,3,"moy"), times = 12)
for(i in 1:48){
  list_cell_count[i+1] = paste(CvTol[i], paste(bac[i], n[i], sep = ""),Fluo[i], sep = "_")
}
cell_count = data.frame(time, syto2$MixCSC1, syto2$MixCSC2, syto2$MixCSC3,MixC_SCmoy_G,
                        cher$MixCSC1, cher$MixCSC2, cher$MixCSC3, MixC_SCmoy_R,
                        syto2$MixCPP1, syto2$MixCPP2, syto2$MixCPP3,MixC_PPmoy_G,
                        cher$MixCPP1, cher$MixCPP2, cher$MixCPP3, MixC_PPmoy_R,
                        syto2$MixCPPSC1, syto2$MixCPPSC2, syto2$MixCPPSC3, MixC_PPSCmoy_G,
                        cher$MixCPPSC1, cher$MixCPPSC2, cher$MixCPPSC3, MixC_PPSCmoy_R, 
                        syto2$ToluSC1, syto2$ToluSC2, syto2$ToluSC3, Tol_SCmoy_G,
                        cher$ToluSC1, cher$ToluSC2, cher$ToluSC3, Tol_SCmoy_R,
                        syto2$ToluPP1, syto2$ToluPP2, syto2$ToluPP3, Tol_PPmoy_G, 
                        cher$ToluPP1, cher$ToluPP2, cher$ToluPP3, Tol_PPmoy_R,
                        syto2$ToluPPSC1, syto2$ToluPPSC2,syto2$ToluPPSC3,Tol_PPSCmoy_G,
                        cher$ToluPPSC1,cher$ToluPPSC2,cher$ToluPPSC3, Tol_PPSCmoy_R)
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
celllog <- cbind(cell_count$time, log(cell_count[,2:49]))
colnames(celllog)[1] <-"time"

#graphiques



par(mfrow = c(1,1))

totsc <- data.frame(time, MixC_SCmoy_G, Tol_SCmoy_G, MixC_PPSCmoy_G, Tol_PPSCmoy_G)
Tol_deadPPmoy<- c()
#ppmortes dans ppseul

for(i in 1:8){
  Tol_deadPPmoy[i] <- sum(syto2$ToluPP1[i],  syto2$ToluPP2[i],syto2$ToluPP3[i])/3
}

#total dans ppseul
Tol_all_in_pp = c()
for(i in 1:8){
  Tol_all_in_pp[i]  <- Tol_deadPPmoy[i] + Tol_PPmoy_R[i]
}

#Plot des comptes de SC (fluoresence verte)

#Normal
par(mfrow = c(1,1))
par(mai = rep(1,4), pin = c(10, 6))
plot( cell_count$time, cell_count$MixC_SCmoy_G,log="y",xlim=c(0,48),ylim=c(2950,4028683330), cex.main=2.5,cex.axis=1.5,
     cex.lab=2.2,type="o",main="SC growth\n Week 1",xlab= "Time [Hours]", ylab="SC count")
#replicats
points( cell_count$time, cell_count$MixC_SC1_G,type = "p")
points( cell_count$time, cell_count$MixC_SC2_G,type = "p")
points( cell_count$time, cell_count$MixC_SC3_G,type = "p")
#lines sc tol
points(cell_count$time,cell_count$Tol_SCmoy_G, type="o", col="red")
#replicats
points(cell_count$time,cell_count$Tol_SC1_G, type="p", col="red")
points(cell_count$time,cell_count$Tol_SC2_G, type="p", col="red")
points(cell_count$time,cell_count$Tol_SC3_G, type="p", col="red")
#lines ppsc mixc
points(cell_count$time,cell_count$MixC_PPSCmoy_G, type="l", col="blue")
#rep 
points(cell_count$time,cell_count$MixC_PPSC1_G, type="p", col="blue")
points(cell_count$time,cell_count$MixC_PPSC2_G, type="p", col="blue")
points(cell_count$time,cell_count$MixC_PPSC3_G, type="p", col="blue")
#line ppsc tol
points(cell_count$time,cell_count$Tol_PPSCmoy_G, type="l", col="orange")
#rep
points(cell_count$time,cell_count$Tol_PPSC1_G, type="p", col="orange")
points(cell_count$time,cell_count$Tol_PPSC2_G, type="p", col="orange")
points(cell_count$time,cell_count$Tol_PPSC3_G, type="p", col="orange")

legend("topleft", cex=2,legend=c("SC alone in mixC","SC alone in Tol","SC in PPSC in mixC","SC in PPSC in Tol"),fill=c("black","red","blue","orange"))

#AIRE COLOREES
x<-c(0,cell_count$time)
y <- c(0,cell_count$MixC_SCmoy_G)

xleo = c(0, cell_count$time,48)

plot(x,y,log="y",xlim=c(0,48),cex.main=2.5,cex.axis=1.5,
     cex.lab=2.2,ylim=c(2950,4028683330), type="o",main="SC growth\nWeek1",xlab= "Time [Hours]", ylab="log(SC count)")
yleo = c(100,cell_count$MixC_SCmoy_G,100)
polygon(xleo, yleo, col=rgb(0.1,0.1,0.1,0.5))

points(cell_count$time,cell_count$Tol_SCmoy_G, type="o", col="red")
yol<- c(100,cell_count$Tol_SCmoy_G,100)
polygon(xleo, yol, col=rgb(0.9,0,0,0.4))

points(cell_count$time,cell_count$MixC_PPSCmoy_G, type="o", col="blue")
yol2 <- c(100,cell_count$MixC_PPSCmoy_G,100)
polygon(xleo, yol2, col=rgb(0,0,1,0.4))

points(cell_count$time,cell_count$Tol_PPSCmoy_G, type="o", col="orange")
yol3 <- c(100,cell_count$Tol_PPSCmoy_G,100)
polygon(xleo, yol3, col=rgb(0.8,0.3,0.1,0.4))
legend("topleft", cex=2,legend=c("SC alone in mixC","SC alone in Tol","SC in PPSC in mixC","SC in PPSC in Tol"),fill=c("black","red","blue","orange"))

#plot(cell_count$time, cell_count$MixC_PPmoy_G, log="y",xlim=c(0,48),ylim=c(min(cell_count$Tol_PPmoy_G), max(cell_count$Tol_PPSCmoy_G + cell_count$Tol_PPSCmoy_R)),type="o", col="green")
#points(cell_count$time, (cell_count$MixC_PPSCmoy_G + cell_count$MixC_PPSCmoy_R), type="o", col="brown")
#points(cell_count$time, (cell_count$Tol_PPSCmoy_G + cell_count$Tol_PPSCmoy_R), type="o", col="gray")
#points(cell_count$time, (cell_count$Tol_PPmoy_G + cell_count$Tol_PPmoy_R), type = "o", col = "pink")
#legend("bottomright",legend = c( "Syt9 cnt in Tol PPalone", "PPSC total in mixC", "PPSC total in Tol", "total count in pp alone"),fill=c("green", "brown", "gray", "pink"))

#SAME WITH PP (fluoresence rouge)
par(mai= c(1,1,1,1))
##Plot des comptes de PP
par(mai = rep(1,4), pin = c(10, 6))
plot(cell_count$time, cell_count$Tol_PPmoy_R,cex.main=2.5,cex.axis=1.5,log="y",
     cex.lab=2.2,xlim=c(0,48),ylim=c(2950,4028683330), type="l", col = "red"
     ,main="PP growth\n Week 1",xlab= "Time [Hours]", ylab="PP count")
## replicats
points(cell_count$time,cell_count$Tol_PP1_R, type="p", col="red")
points(cell_count$time,cell_count$Tol_PP2_R, type="p", col="red")
points(cell_count$time,cell_count$Tol_PP3_R, type="p", col="red")
#lignes ppsc
points(cell_count$time,cell_count$Tol_PPSCmoy_R, type="l", col="orange")
# replicats
points(cell_count$time,cell_count$Tol_PPSC1_R, type="p", col="orange")
points(cell_count$time,cell_count$Tol_PPSC2_R, type="p", col="orange")
points(cell_count$time,cell_count$Tol_PPSC3_R, type="p", col="orange")
#lignes pppsc
points(cell_count$time,cell_count$MixC_PPmoy_R, type="l", col="black")
#replicats
points(cell_count$time,cell_count$MixC_PP1_R, type="p", col="black")
points(cell_count$time,cell_count$MixC_PP2_R, type="p", col="black")
points(cell_count$time,cell_count$MixC_PP3_R, type="p", col="black")

#lignes pppsc
points(cell_count$time,cell_count$MixC_PPSCmoy_R, type="l", col="blue")
#replicats
points(cell_count$time,cell_count$MixC_PPSC1_R, type="p", col="blue")
points(cell_count$time,cell_count$MixC_PPSC2_R, type="p", col="blue")
points(cell_count$time,cell_count$MixC_PPSC3_R, type="p", col="blue")

#points(cell_count$time, cell_count$MixC_PPSCmoy_R + cell_count$MixC_PPSCmoy_G, type="o", col="brown")
#points(cell_count$time, cell_count$Tol_PPSCmoy_R, type="o", col="gray")
legend("bottomright",cex=2,legend=c("PP alone in mixC","PP alone in Tol","PP in PPSC in mixC","PP in PPSC in Tol"),fill=c("black","red","blue","orange"))


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
Species <- c()
names3 <- c()
CvTol = rep(c("MixC", "Tol"), each = 15, time=2)
bac = c(rep(rep(c("SC", "PP", "PPSC_SC","PPSC_PP", "PPSC_tot"), each = 3), time = 2),rep(rep(c("SC", "PV", "PVSC_SC","PVSC_PV", "PVSC_tot"), each = 3), time = 2))
n = c(rep(c(1,2,3), times = 10), rep(c("1_1","2_1","3_1"), times=10))
for(i in 1:60){
  names1[i] = paste(CvTol[i], paste(bac[i], n[i], sep = ""), sep = "_")
  Species[i] = bac[i]
  names3[i] = paste(CvTol[i], paste(bac[i], sep = ""), sep = "_")
}

## Table avec les mêmes noms dans le même ordre que week
time_w1 = c(0, 11, 15 + 1/6, 19.5,22.5,38.5 , 45+ 1/3 ,48 )
time_w2 = c(0, 14.5, 18, 21, 23.5, 36.5, 42 ,44.5 , 47.5 )

ess <- read.table("ess.txt", sep="\t", header=T)
ess2 <- read.table("ess2bis.csv", header = T, sep = ",")


week <- data.frame(names1, Species,names3,Substrate,week_on_SC,Replicat,PV, PP,SC,PPSC, PVSC,AUC)

## Aire sous la courbe
for(i in 1:30){
  week$AUC[i] = trapz(time_w1,ess[,i])
}

for(i in 31:60){
  week$AUC[i] = trapz(time_w2, ess2[,i-30])
}




summary(glm(log(week$AUC) ~ as.factor(week$week_on_SC)+ as.factor(week$Species)+as.factor(week$Substrate)+as.factor(week$Replicat)))
## que les réplicat ne sont pas important, n'ont pas d'influence, que il n'y a pas de différence entre le SC d'une semaine à l'autre.


                  

##Anova
model = aov(log(AUC)~Substrate*as.factor(Species), data=week)
anova(model)
#le substrat a un effet significatif, les échantillons ayant poussé dans des substrats différents sont en moyenne différents.
#au moins un traitement à un effet significatif,





#----------------------------------graphes et anova
library(car)
## etiquettes de l'axe X
bac2 = c(rep(rep(c("SCw1", "PP", "PPSC_SC","PPSC_PP", "PPSC_tot"), each = 3), time = 2),rep(rep(c("SCw2", "PV", "PVSC_SC","PVSC_PV", "PVSC_tot"), each = 3), time = 2))
CvTol2 = rep(c("MixC", "Tol"), each = 15, time=2)
eti = c()
for(i in 1:60){
  eti[i] = paste(CvTol2[i], bac2[i],sep = "_")
}
eti = as.factor(eti)

#t.test on SC week1 vs week2
t.test(week$AUC[week$week_on_SC=="1"& week$names3=="Tol_SC"], week$AUC[week$week_on_SC=="2"& week$names3=="Tol_SC"])
t.test(week$AUC[week$week_on_SC=="1"& week$names3=="MixC_SC"], week$AUC[week$week_on_SC=="2"& week$names3=="MixC_SC"])

#-----------------week1
## graphes de l'anova SC semaine 1
par(mfrow = c(1,1))
par(mai = c(2,1,1,1))
AUC_col = rep(c("red", "black", "blue","black", "black", "green", "black", "orange","black","black", "black", "black"), each = 3)
plot(rep(1:10, each = 3), log(week$AUC[1:30]),
     cex.main=2.5,cex.axis=1.5,
     cex.lab=2.2,main = "Area under the curve according to the treatment\n Week 1",
     xlab = "", xaxt = "n", ylab = "log (AUC)", pch=c(1,3,4))
axis(1, cex=2,at = 1:10, labels = unique(eti[1:30]), las = 2, hadj = T, font = 2, outer = F)

## graphes de l'anova SC semaine 1 seulement 4 valeurs
par(mfrow = c(1,1))
par(mai = c(2,1,1,1))
AUC_col = rep(c("black", "blue", "red","orange"), each = 3)
plot(rep(1:4, each = 3), log(week$AUC[c(1:3,7:9,16:18, 22:24)]),
     cex.main=2.5,cex.axis=1.5,
     cex.lab=2.2,main = "Area under the curve according to the treatment\nSC Experiment 1",
     xlab = "", xaxt = "n", ylab = "log (AUC)", pch=c(1,3,4), col=AUC_col, ylim = range(log(week$AUC)))
axis(1, cex=2,at = 1:4, labels = c("SC in MixC", "SC in MixC in \npresence of PP ","SC in Tol", "SC in tol\nin presence of PP" ), las = 2, hadj = T, font = 2, outer = F)
#effect de pp in tol
abline(h = mean(log(week$AUC[c(22:24)])))
abline(h = mean(log(week$AUC[c(16:18)])))
exp(20) - exp(16) 

#effet du media
abline(h = mean(log(week$AUC[c(16:18, 22:24)])), col = "red")
abline(h = mean(log(week$AUC[c(1:3,7:9)])), col = "brown")
## on avait prévu 8 test, 4 anova, et test si différences dans tol.
## Alpha ajd = 1 - (0.95 ^ 1/8) = 0.0063
## tests Week 1 SC
week1 <- week[1:30,]
AUC_w1_sc = log(week1$AUC[week1$Species%in%c("SC","PPSC_SC")])
Sub_w1_sc = week1$Substrate[week1$Species%in%c("SC","PPSC_SC")]
Species_w1_sc = week1$Species[week1$Species %in%c("SC","PPSC_SC")]
modelw1_SC <- aov(AUC_w1_sc~Sub_w1_sc* Species_w1_sc)
plot(as.factor(rep((1:4), each = 3)),residuals(modelw1_SC))
leveneTest(modelw1_SC)
anova(modelw1_SC) 
## F corigés car modèle mixte, substrat effet fixe et species effet aléatoire
## F species = 9.87/ 0.5 = 17.94
1 - pf(17.94, 1, 8)
## F substrate = 4.11 / 14.62 = 0.28
1 - pf(0.28, 1, 1)
#sctol vs PPSC_G tol 
week_1_cpl_1 = week1$Species%in%c("SC","PPSC_SC") & week1$Substrate == "Tol"
week_1_cpl_1_mdl = aov(log(week1$AUC[week_1_cpl_1])~ week1$Species[week_1_cpl_1])
anova(week_1_cpl_1_mdl)
## on extrait le carré moyen sp = 24.25
## F = MSsp/MSe première anova = 44.09
1- pf(44.09, 1, 8) # < 0.00639 
## on rejette l'hypothèse nulle, les moyennes de tol_SC_G et tol_PPSC_G 
## sont identiques. à l'aide du graphe, sc<ppsc

## ou bien
TukeyHSD(modelw1_SC)


## graphes week 1 PP
AUC_col_pseudo = rep(c( "black","red", "black", "blue", "black", "black", "green", "black", "orange","black"), each = 3)
plot(rep(1:10, each = 3), log(week$AUC[1:30]),
     cex.main=2.5,cex.axis=1.5,
     cex.lab=2.2, main = "Area under the curve according to the treatment\nWeek1",
     xlab = "", xaxt = "n", ylab = "log (AUC)", col = AUC_col_pseudo, pch=c(1,3,4))
axis(1, cex=2,at = 1:10, labels = unique(eti[1:30]), las = 2, hadj = T, font = 2, outer = F,pch = c(1,3,4))


## graphes seulement 4 valeurs week 2
par(mai = c(2,1,1,1))
AUC_col = rep(c("black", "blue", "red","orange"), each = 3)
plot(rep(1:4, each = 3), log(week$AUC[c(4:6,10:12,19:21, 25:27)]),
     cex.main=2.5,cex.axis=1.5,
     cex.lab=2.2,main = "Area under the curve according to the treatment\n PP Experiment 1",
     xlab = "", xaxt = "n", ylab = "log (AUC)", pch=c(1,3,4), col=AUC_col, ylim = range(log(week$AUC)))
axis(1, cex=2,at = 1:4, labels = c("PP in MixC", "PP in MixC in \npresence of SC ","PP in Tol", "PP in tol\nin presence of SC" ), las = 2, hadj = T, font = 2, outer = F)


## test week 1 PP 
#t.test(week1$AUC[week1$names3=="Tol_PP"], week$AUC[week1$names3=="MixC_PP"])
AUC_w1_pp = log(week1$AUC[week1$Species%in%c("PP","PPSC_PP")])
Sub_w1_pp = week1$Substrate[week1$Species%in%c("PP","PPSC_PP")]
Species_w1_pp = week1$Species[week1$Species %in%c("PP","PPSC_PP")]
modelw1_PP <- aov(AUC_w1_pp~Sub_w1_pp* Species_w1_pp)
plot(as.factor(rep((1:4), each = 3)),residuals(modelw1_PP))
leveneTest(modelw1_PP)
anova(modelw1_PP)
#pas d'interaction
modelw1_PP2 <- aov(AUC_w1_pp~Sub_w1_pp+ Species_w1_pp)
anova(modelw1_PP2)
## Fcorigés car modèle mixte
## F species = MSsp / MSe = 0.5
1-pf(0.5, 1, 8) # > 0.006
## F sub = MSsub/MSx = 92289.37
1-pf(92289.37, 1, 1) # < 0.006

TukeyHSD(modelw1_PP)

#   #croissance SC sur MixC week1
# t.test(week1$AUC[week1$names3=="MixC_SC"], week1$AUC[week1$names3=="MixC_PPSC_SC"])
#   #croissance de PP sur MixC week1
# t.test(week1$AUC[week1$names3=="MixC_PP"], week1$AUC[week1$names3=="MixC_PPSC_PP"])
#   #croissance SC sur tol week1
# t.test(week1$AUC[week1$names3=="Tol_SC"], week1$AUC[week1$names3=="Tol_PPSC_SC"])
#   #croissance PP sur tol week1
# t.test(week1$AUC[week1$names3=="Tol_PP"], week1$AUC[week1$names3=="Tol_PPSC_PP"])
#   

#GRAPH AUC WEEK 1
par(mai = c(2,1,1,1))
plot(rep(1:10, each = 3), log(week$AUC[1:30]),
     cex.main=2.5,cex.axis=1.5, ylim = c(15.5, 25),
     cex.lab=2.2,main = "Area under the curve according to the treatment\n Week 1",
     xlab = "", xaxt = "n", ylab = "log (AUC)", pch=c(1,3,4))
axis(1, cex.axis=1.4,at = 1:10, labels = unique(eti[1:30]), las = 2, hadj = T, font = 2, outer = F)

#GRAPH AUC WEEK 2
#----------------week2
#graphes week 2 SC
par(mai = c(2,1,1,1))
plot(rep(1:10, each = 3), log(week$AUC[31:60]),
     main = "Area under the curve according to the treatment\n Week 2",
     cex.main=2.5,cex.axis=1.5, ylim = c(15.5, 25),
     cex.lab=2.2, xlab = "", xaxt = "n", ylab = "log (AUC)", pch = c(1,3,4))
axis(1, cex.axis=1.4,at = 1:10, labels = unique(eti[31:60]), las = 2, hadj = T, font = 2, outer = F)

## graphes de l'anova SC semaine 2 seulement 4 valeurs
par(mfrow = c(1,1))
par(mai = c(2,1,1,1))
AUC_col = rep(c("black", "blue", "red","yellow"), each = 3)
plot(rep(1:4, each = 3), log(week$AUC[c(1:3,7:9,16:18, 22:24)+30]),
     cex.main=2.5,cex.axis=1.5,
     cex.lab=2.2,main = "Area under the curve according to the treatment\nSC Experiment 2",
     xlab = "", xaxt = "n", ylab = "log (AUC)", pch=c(1,3,4), col=AUC_col, ylim = range(log(week$AUC)))
axis(1, cex=2,at = 1:4, labels = c("SC in MixC", "SC in MixC in \npresence of PV ","SC in Tol", "SC in tol\nin presence of PV" ), las = 2, hadj = T, font = 2, outer = F)
abline(h = mean(log(week$AUC[c(16:18, 22:24)])))


##test week 2 SC
week2 <- week[31:60,]
AOV_w2_SC = log(week2$AUC[week2$Species%in%c("SC","PVSC_SC")])
Sub_w2_sc = week2$Substrate[week2$Species%in%c("SC","PVSC_SC")]
Species_w2_SC = week2$Species[week2$Species %in%c("SC","PVSC_SC")]
modelw2_SC <- aov(AOV_w2_SC~Sub_w2_sc* Species_w2_SC)
plot(as.factor(rep((1:4), each = 3)),residuals(modelw2_SC))
leveneTest(modelw2_SC)
summary(modelw2_SC)
# f corrigé car modèle mixte
# Fsp = MSsp/MSe = 0.6152/0.7526 = 0.8137
pf(0.8137,1,8, lower.tail = F) # > alpha = 0.006
#Fsub = MSsub/MSx = 0.5693/0.2383 = 2.389
pf(2.389,1,1, lower.tail = F) #> alpha = 0.006

TukeyHSD(modelw2_SC)
par(mfrow=c(1,1))


## plot week2 PV
AUC_col_pseudo = rep(c( "black","red", "black", "blue", "black", "black", "green", "black", "orange","black"), each = 3)
plot(rep(1:10, each = 3), log(week$AUC[31:60]),
     main = "Area under the curve according to the treatment\nWeek2",
     cex.main=2.5,cex.axis=1.5,
     cex.lab=2.2,xlab = "", xaxt = "n", ylab = "log (AUC)", col = AUC_col_pseudo, pch = c(1,3,4))
axis(1,cex=2, at = 1:10, labels = unique(eti[31:60]), las = 2, hadj = T, font = 2, outer = F)


## graphes seulement 4 valeurs
par(mai = c(2,1,1,1))
AUC_col = rep(c("black", "blue", "red","yellow"), each = 3)
plot(rep(1:4, each = 3), log(week$AUC[c(4:6,10:12,19:21, 25:27)+30]),
     cex.main=2.5,cex.axis=1.5,
     cex.lab=2.2,main = "Area under the curve according to the treatment\n PV Experiment 2",
     xlab = "", xaxt = "n", ylab = "log (AUC)", pch=c(1,3,4), col=AUC_col, ylim = range(log(week$AUC)))
axis(1, cex=2,at = 1:4, labels = c("PV in MixC", "PV in MixC in \npresence of SC ","PV in Tol", "PV in tol\nin presence of SC" ), las = 2, hadj = T, font = 2, outer = F)


## test week 2 PV
AUC_w2_pv = log(week2$AUC[week2$Species%in%c("PV","PVSC_PV")])
Sub_w2_pv = week2$Substrate[week2$Species%in%c("PV","PVSC_PV")]
Species_w2_pv = week2$Species[week2$Species %in%c("PV","PVSC_PV")]
modelw2_PV <- aov(AUC_w2_pv~Sub_w2_pv*Species_w2_pv )
plot(as.factor(rep((1:4), each = 3)),residuals(modelw2_PV))
leveneTest(modelw2_PV)
anova(modelw2_PV)
# corriger F 
# Fsp = MSsp /MSe = 0.4779/0.02907 = 16.44
pf(16.44, 1, 8, lower.tail = F) #<0.006
#Fsub = MSsub/MSx = 25.95 /0.1309 = 198.2
pf(198.2,1,1, lower.tail = F)# > 0.006
# test complémentaire pour savoir quels niveaux diffèrent
week_2_cpl_1 = week2$Species%in%c("PV","PVSC_PV") & week1$Substrate == "Tol"
week_2_cpl_1_mdl = aov(log(week2$AUC[week_2_cpl_1])~ week2$Species[week_2_cpl_1])
anova(week_2_cpl_1_mdl)
## on extrait le carré moyen sp = 0.5546
## F = MSsp/MSe première anova = 0.5546/0.02907 = 19.08 
1- pf(19.08, 1, 8) # < 0.00639 
## on rejette l'hypothèse nulle, les moyennes de tol_PV_R et tol_PVSC_R 
## sont identiques. à l'aide du graphe, PVSC < PV

TukeyHSD

# 
# #croissance SC sur MixC week1
# t.test(week2$AUC[week2$names3=="MixC_SC"], week2$AUC[week2$names3=="MixC_PVSC_SC"])
# #croissance de PV sur MixC week2
# t.test(week2$AUC[week2$names3=="MixC_PV"], week2$AUC[week2$names3=="MixC_PVSC_PV"])
# #croissance SC sur tol week2
# t.test(week2$AUC[week2$names3=="Tol_SC"], week2$AUC[week2$names3=="Tol_PVSC_SC"])
# #croissance PV sur tol week2
# t.test(week2$AUC[week2$names3=="Tol_PV"], week2$AUC[week2$names3=="Tol_PVSC_PV"])
# 

# 
# t.test(week1$AUC[week1$names3=="Tol_SC"], week1$AUC[week1$names3=="Tol_PPSC_SC"])
# t.test(week1$AUC[week1$names3=="MixC_PP"], week1$AUC[week1$names3=="MixC_PPSC_PP"])
# 
# 
# t.test(week1$AUC[week1$names3=="Tol_PP"], week1$AUC[week1$names3=="Tol_PPSC_PP"])
# t.test(week1$AUC[week1$names3=="MixC_PP"], week1$AUC[week1$names3=="MixC_PPSC_PP"])
# 
# t.test(week2$AUC[week2$names3=="Tol_PV"], week2$AUC[week2$names3=="Tol_PVSC_PV"])
# t.test(week2$AUC[week2$names3=="MixC_PV"], week2$AUC[week2$names3=="MixC_PVSC_PV"])
# 
# week1$AUC[week1$names3=="MixC_SC"]- week1$AUC[week1$names3=="MixC_PPSC_SC"]
# week2$AUC[week2$names3=="MixC_PV"]- week2$AUC[week2$names3=="MixC_PVSC_PV"]
# 

## graphique problèmes problem
par(mai = rep(1,4), pin = c(10, 6))
plot(cell_count$time,  cell_count$Tol_PPmoy_G,ylim= c( 2033.5, 1136783330), type = "l",cex.main=2.5,
     cex.lab=2.2, cex.axis=1.5,
     log="y",main = "Cell count in green fluorescence gate - Toluene \n Week 1",
     ylab = "Total count", xlab = "Time[hours]", col = "red" )
points(cell_count$time,  cell_count$Tol_PPSCmoy_G, type = "l", col = "blue")
points(cell_count$time,  cell_count$Tol_PPSC1_G,type = "p", col = "blue")
points(cell_count$time,  cell_count$Tol_PPSC2_G,type = "p", col = "blue")
points(cell_count$time,  cell_count$Tol_PPSC3_G,type = "p", col = "blue")

test1 <- (cell_count$Tol_PPSC1_G+cell_count$Tol_PPSC2_G+cell_count$Tol_PPSC3_G)/3

points(cell_count$time,  cell_count$Tol_PP1_G,type = "p", col = "red")
points(cell_count$time,  cell_count$Tol_PP2_G,type = "p", col = "red")
points(cell_count$time,  cell_count$Tol_PP3_G,type = "p", col = "red")

legend(x = "bottomright",legend = c("Green (only) fluorescence in PP", "Green (only) fluorescence in PPSC"), fill = c("red", "blue"), cex=2)

t.test(cell_count$Tol_PPmoy_G, cell_count$Tol_PPSCmoy_G) #p = 0.6916

###------------------------------------------------Comptes totaux
## MIx carbon PPsc moyenne
par(mai = rep(1,4), pin = c(10, 6))
plot(cell_count$time,  cell_count$MixC_PPSCmoy_G+cell_count$MixC_PPSCmoy_R,type = "l",
     cex.main=2.5,cex.axis=1.5,log="y",
     cex.lab=2.2, main = "Total count in Mixed Carbon \n Week 1",
     ylab = "Cell count", xlab = "Time[hours]", col = "orange", ylim=c( 2950, 4028683330))

##points pour ppsc
points(cell_count$time,  cell_count$MixC_PPSC1_G+cell_count$MixC_PPSC1_R,type = "p", col = "orange")
points(cell_count$time,  cell_count$MixC_PPSC2_G+cell_count$MixC_PPSC2_R,type = "p", col = "orange")
points(cell_count$time,  cell_count$MixC_PPSC3_G+cell_count$MixC_PPSC3_R,type = "p", col = "orange")
# PP seul moyenne
points(cell_count$time,  cell_count$MixC_PPmoy_G+cell_count$MixC_PPmoy_R, type = "l", col = "red")
# pp seul points
points(cell_count$time,  cell_count$MixC_PP1_G+cell_count$MixC_PP1_R, type = "p", col = "red")
points(cell_count$time,  cell_count$MixC_PP2_G+cell_count$MixC_PP2_R, type = "p", col = "red")
points(cell_count$time,  cell_count$MixC_PP3_G+cell_count$MixC_PP3_R, type = "p", col = "red")
## sc seul moy
points(cell_count$time,  cell_count$MixC_SCmoy_G+cell_count$MixC_SCmoy_R, type = "l", col = "green")
## sc seul points
points(cell_count$time,  cell_count$MixC_SC1_G+cell_count$MixC_SC1_R, type = "p", col = "green")
points(cell_count$time,  cell_count$MixC_SC2_G+cell_count$MixC_SC2_R, type = "p", col = "green")
points(cell_count$time,  cell_count$MixC_SC3_G+cell_count$MixC_SC3_R, type = "p", col = "green")

legend(x="topleft", legend = c("MixC SC alone", "MixC PP alone", "MixC PPSC"), fill=c( "green", "red", "orange"),cex=2)



## dans le toluène
par(mai = rep(1,4), pin = c(10, 6))
plot(cell_count$time,  cell_count$Tol_PPmoy_G+cell_count$Tol_PPmoy_R,type = "l",
     cex.main=2.5,cex.axis=1.5,log="y",
     cex.lab=2.2, main = "Total count in Toluene \n Week 1",
     ylab = "Cell count", xlab = "Time[hours]", col = "red", ylim=c( 2950, 4028683330))
## points pour les relicats pp
points(cell_count$time,  cell_count$Tol_PP1_G+cell_count$Tol_PP1_R,type = "p", col = "red")
points(cell_count$time,  cell_count$Tol_PP2_G+cell_count$Tol_PP2_R,type = "p", col = "red")
points(cell_count$time,  cell_count$Tol_PP3_G+cell_count$Tol_PP3_R,type = "p", col = "red")
## les ligne pour les moyennes ppsc
points(cell_count$time,  cell_count$Tol_PPSCmoy_G+cell_count$Tol_PPSCmoy_R, type = "l", col = "orange")
## les points pour les réplicats ppsc
points(cell_count$time,  cell_count$Tol_PPSC1_G+cell_count$Tol_PPSC1_R, type = "p", col = "orange")
points(cell_count$time,  cell_count$Tol_PPSC2_G+cell_count$Tol_PPSC2_R, type = "p", col = "orange")
points(cell_count$time,  cell_count$Tol_PPSC2_G+cell_count$Tol_PPSC2_R, type = "p", col = "orange")
## lignes pour la moyenne sc
points(cell_count$time,  cell_count$Tol_SCmoy_G+cell_count$Tol_SCmoy_R, type = "l", col = "green")
# points pour les réplicats sc
points(cell_count$time,  cell_count$Tol_SC1_G+cell_count$Tol_SC1_R, type = "p", col = "green")
points(cell_count$time,  cell_count$Tol_SC2_G+cell_count$Tol_SC2_R, type = "p", col = "green")
points(cell_count$time,  cell_count$Tol_SC3_G+cell_count$Tol_SC3_R, type = "p", col = "green")
# légende
legend(x="topleft", legend = c("Tol SC alone", "Tol PP alone", "Tol PPSC"), fill=c("green", "red", "orange"),cex=2)

##stat
#AUC pour comptes totaux
tot_sc_w1_MixC = data.frame(cell_count$MixC_SC1_G+cell_count$MixC_SC1_R,
                            cell_count$MixC_SC2_G+cell_count$MixC_SC2_R,
                            cell_count$MixC_SC3_G+cell_count$MixC_SC3_R)
names(tot_sc_w1_MixC) = c("SC1", "SC2", "SC3")
tot_sc_w1_MixC_auc = rep(NA, times = 3)
for(i in 1:3){
  tot_sc_w1_MixC_auc[i] = trapz(time_w1, tot_sc_w1_MixC[,i])
}
tot_sc_w1_MixC_auc

model_w1_tot = aov()

#graphes AUC paper
## graphes de l'anova SC semaine 1 seulement 4 valeurs
par(mfrow = c(1,1))
par(mai = c(2.8,1,1,1))
AUC_col = rep(c("black", "blue", "red","orange"), each = 3)
plot(rep(1:4, each = 3), log(week$AUC[c(1:3,7:9,16:18, 22:24)]),
     cex.main=2.5,cex.axis=1.5, lwd=2, 
     cex.lab=2.2,main = "AUC for each treatment of SC\nWeek 1",
     xlab = "", xaxt = "n", ylab = "log (AUC)", pch=c(1,3,4), col=AUC_col, ylim = range(log(week$AUC)))
axis(1, cex.axis=2,at = 1:4, labels = c("SC in MixC", "SC in MixC in \npresence of PP ","SC in toluene", "SC in toluene in\n presence of PP" ), las = 2, hadj = T, font = 1, outer = F)

plot(rep(1:4, each = 3), log(week$AUC[c(4:6,10:12,19:21, 25:27)]),
     cex.main=2.5,cex.axis=1.5,lwd=2,
     cex.lab=2.2,main = "AUC for each treatment of PP\nWeek 1",
     xlab = "", xaxt = "n", ylab = "log (AUC)", pch=c(1,3,4), col=AUC_col, ylim = range(log(week$AUC)))
axis(1, cex.axis=2,at = 1:4, labels = c("PP in MixC", "PP in MixC in \npresence of SC ","PP in toluene", "PP in toluene in\n presence of SC" ), las = 2, hadj = T, font = 1, outer = F)

plot(rep(1:4, each = 3), log(week$AUC[c(1:3,7:9,16:18, 22:24)+30]),
     cex.main=2.5,cex.axis=1.5,lwd=2,
     cex.lab=2.2,main = "AUC for each treatment of SC\nWeek 2",
     xlab = "", xaxt = "n", ylab = "log (AUC)", pch=c(1,3,4), col=AUC_col, ylim = range(log(week$AUC)))
axis(1, cex.axis=2,at = 1:4, labels = c("SC in MixC", "SC in MixC in \npresence of PV ","SC in toluene", "SC in toluene in\n presence of PV" ), las = 2, hadj = T, font = 1, outer = F)

plot(rep(1:4, each = 3), log(week$AUC[c(4:6,10:12,19:21, 25:27)+30]),
     cex.main=2.5,cex.axis=1.5,lwd=2,
     cex.lab=2.2,main = "AUC for each treatment of PV\nWeek 2",
     xlab = "", xaxt = "n", ylab = "log (AUC)", pch=c(1,3,4), col=AUC_col, ylim = range(log(week$AUC)))
axis(1, cex.axis=2,at = 1:4, labels = c("PV in MixC", "PV in MixC in \npresence of SC ","PV in toluene", "PV in toluene in\n presence of SC" ), las = 2, hadj = T, font = 1, outer = F)




#concentration de tol
mvol = 0.8670 #g/cm cube
vol = 100 #centimetre cube
masse = mvol * vol
masse
MM = 92.1384
nmol = MM/masse
nmol