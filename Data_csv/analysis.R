
#charger les données, après avoir fait session, set working directory, to source file location
path = "T"
List = vector(mode = "list", length = 8)
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
T0$All.Abs..Count = T0$All.Abs..Count*10
T0$mChe.SYTO.9.Abs..Count = T0$mChe.SYTO.9.Abs..Count*10
T0$SYTO.9.Abs..Count = T0$SYTO.9.Abs..Count*10
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
T8$All.Abs..Count = T8$All.Abs..Count*100
T8$mChe.SYTO.9.Abs..Count = T8$mChe.SYTO.9.Abs..Count*100
T8$SYTO.9.Abs..Count = T8$SYTO.9.Abs..Count*100



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

#vecteur contenant le temps
time = c(0, 12, 15, 18,24, 39,43 ,48 )

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
  plot(syto2[,1],syto2[,i], log = "y", main = List_treat[[i-1]], xlab = "time[h]", ylab = "SYTO-9 count")
}

for(i in 11:19){
  plot(syto2[,1],syto2[,i], log = "y", main = List_treat[[i-1]],xlab = "time[h]", ylab = "SYTO-9 count")
}

###------------------------------------------------------------------m-che 
#creer data frame pour acceuilir les données

time = c(0, 12, 15, 18,24, 39,43 ,48 )

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
  plot(cher[,1],cher[,i], log = "y", main = List_treat[[i-1]], xlab = "time[h]", ylab = "mChe count")
}

for(i in 11:19){
  plot(cher[,1],cher[,i], log = "y", main = List_treat[[i-1]],xlab = "time[h]", ylab = "mChe count")
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
    absol[i,j+1] = mean(Data$All.Abs..Count[Data$Well.ID == lech[j]][1:2])
    
  }}

#graphiques
par(mfrow = c(3,3))
for(i in 2:10){
  plot(absol[,1],absol[,i], ylim = c(min(absol[,2:10]), max(absol[,2:10])), log = "y", main = List_treat[[i-1]], xlab = "time[h]", ylab = "Absolut count")
}

for(i in 11:19){
  plot(absol[,1],absol[,i],  ylim = c(min(absol[,2:19]), max(absol[,2:19])),log = "y", main = List_treat[[i-1]],xlab = "time[h]", ylab = "Absolut count")
}

#puissance stat 
SC_t6_mixC_syto9 = c(syto2$MixCSC1[syto2$time == 43],syto2$MixCSC2[syto2$time == 43],syto2$MixCSC3[syto2$time == 43] )

PP_t6_mixC_mChe = c(cher$ToluPP1[cher$time == 43],cher$ToluPP2[cher$time == 43],cher$ToluPP3[cher$time == 43] )

SC_t6_Tol_syto9 = c(syto2$ToluSC1[syto2$time == 43],syto2$ToluSC2[syto2$time == 43],syto2$ToluSC3[syto2$time == 43] )

PP_t6_Tol_mChe = c(cher$ToluPP1[cher$time == 43],cher$ToluPP2[cher$time == 43],cher$ToluSC3[cher$time == 43])

var(SC_t6_mixC_syto9)

var(PP_t6_Tol_mChe)

var(PP_t6_mixC_mChe)

var(SC_t6_Tol_syto9)

