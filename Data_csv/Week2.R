#charger les données, après avoir fait session, set working directory, to source file location
path = "Data_W2/T"
List = vector(mode = "list", length = 8) #sorte de liste qui peut contenir des variables
csv = c()
for(i in 1:9){
  List[[i]]= paste("T" , i-1, sep = "")
  csv[i] = paste(path,i-1,".csv", sep = "")
  assign(paste("T" , i-1, sep = ""), read.csv(csv[i], header = T))
  
}
##I created 2 sets of data, for the stained and the unstained. Maybe we can merge them later in the same table, 
##but I think it will be easier to treat the data that way. 

#T0, 1 , 2, 3, 4 10% dilution adjustment 
T0$All.Abs..Count = T0$All.Abs..Count*10
T0$mChe.SYTO.9.Abs..Count = T0$mChe.SYTO.9.Abs..Count*10
T0$SYTO.9.Abs..Count = T0$SYTO.9.Abs..Count*10
T1$All.Abs..Count = T1$All.Abs..Count*10
T1$mChe.SYTO.9.Abs..Count = T1$mChe.SYTO.9.Abs..Count*10
T1$SYTO.9.Abs..Count = T1$SYTO.9.Abs..Count*10
T2$All.Abs..Count = T2$All.Abs..Count*10
T2$mChe.SYTO.9.Abs..Count = T2$mChe.SYTO.9.Abs..Count*10
T2$SYTO.9.Abs..Count = T3$SYTO.9.Abs..Count*10
T3$All.Abs..Count = T3$All.Abs..Count*10
T3$mChe.SYTO.9.Abs..Count = T3$mChe.SYTO.9.Abs..Count*10
T3$SYTO.9.Abs..Count = T3$SYTO.9.Abs..Count*10
T4$All.Abs..Count = T4$All.Abs..Count*100
T4$mChe.SYTO.9.Abs..Count = T4$mChe.SYTO.9.Abs..Count*10
T4$SYTO.9.Abs..Count = T4$SYTO.9.Abs..Count*10

#T2, 3, 4, 5, 6, 7, 8 dilution adjustment 100%

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

#stainedED noms de traitements: Toluène vs mix C, SC vs PP vs PPSC, replicat
mil_stained = rep(c("MixC","Tolu" ), each = 9)
tre_stained = rep(rep(c("SC_stained", "PV_stained", "PVSC_stained"), each = 3),times = 2)
n_stained = rep(c(1,2,3), times= 6)
names_stained = c()
for(i in 1:18){
  names_stained[i] = paste(mil_stained[i], tre_stained[i], n_stained[i], sep = "")
}

#UNstainedED noms de traitements: Toluène vs mix C, SC vs PP vs PPSC, replicat
mil_unstained = rep(c("MixC","Tolu" ), each = 9)
tre_unstained = rep(rep(c("SC_unstained", "PV_unstained", "PVSC_unstained"), each = 3),times = 2)
n_unstained = rep(c(1,2,3), times= 6)
names_unstained = c()
for(i in 1:18){
  names_unstained[i] = paste(mil_unstained[i], tre_unstained[i], n_unstained[i], sep = "")
}


###------------------------------------------------------------------ SYTO-9
##créer un data frame pour acceuilir les données dans le bon sens
#créer une liste contenant les noms de traitement et leur assigner un vecteur 8xNA 


List_treat_stained = vector(mode = "list", length = 18)
for(i in 1:18){
  List_treat_stained[[i]] = names_stained[i]
  assign(List_treat_stained[[i]], rep(NA, times = 8))
}

List_treat_unstained = vector(mode = "list", length = 18)
for(i in 1:18){
  List_treat_unstained[[i]] = names_unstained[i]
  assign(List_treat_unstained[[i]], rep(NA, times = 8))
}

#vecteur contenant le temps en heure
time = c(0, 14, 17, 20, 34, 39, 42 ,45 )

#création du data.frame stained
syto_stained = data.frame(time, MixCPV_stained1)
for(i in 2:18){
  data_stained = get(List_treat_stained[[i]])
  syto_stained[,i+1] = data_stained
}
names(syto_stained) = c("time", List_treat_stained)

#création du data.frame UNstained
syto_unstained = data.frame(time, MixCPV_unstained1)
for(i in 2:18){
  data_unstained = get(List_treat_unstained[[i]])
  syto_unstained[,i+1] = data_unstained
}
names(syto_unstained) = c("time", List_treat_unstained)


#créer un vecteur contenant les noms des puits d'intérêt stained
let = rep(c(rep("A", times = 3),rep("C", times = 3),rep("E", times = 3)),times = 2)
chi = c(rep(c(1,2,3), times = 3), rep(c(5,6,7),times= 3))
lech = c()
for(i in 1:18){
  lech[i] = paste(let[i], chi[i], sep = "")
} 
#créer un vecteur contenant les noms des puits d'intérêt UNstained
chi_un = c(rep(c(7,8,9), times = 3), rep(c(10,11,12),times= 3))
lech_un = c()
for(i in 1:18){
  lech_un[i] = paste(let[i], chi_un[i], sep = "")
} 

#remplir avec SYTO 9 pour tous les traitements stained
for(i in 1:8){ 
  for(j in 1:18){
    Data = get(List[[i]]) #stocke T[I]
    print(lech[j]) 
    syto_stained[i,j+1] = mean(Data$SYTO.9.Abs..Count[Data$Well.ID== lech[j]][1:2])
    #parcours syto 2 en ligne, prend les comptes absolu syto9 de T[i] qui ont le nom lech[j] (deux replicat tech) et fait une moyenne 
  }}

#remplir avec SYTO 9 pour tous les traitements stained
for(i in 1:8){ 
  for(j in 1:18){
    Data = get(List[[i]]) #stocke T[I]
    print(lech_un[j]) 
    syto_unstained[i,j+1] = mean(Data$SYTO.9.Abs..Count[Data$Well.ID== lech_un[j]][1:2])
    #parcours syto 2 en ligne, prend les comptes absolu syto9 de T[i] qui ont le nom lech[j] (deux replicat tech) et fait une moyenne 
  }}


#graphiques SYTO-9
par(mfrow = c(3,3))
for(i in 2:10){
  plot(syto_stained[,1],syto_stained[,i], log = "y", ylim = c(min(syto_unstained[,2:10]), max(syto_unstained[,2:10])),main = List_treat_stained[[i-1]], xlab = "time[h]", ylab = "SYTO-9 count")
}

for(i in 11:19){
  plot(syto_stained[,1],syto_stained[,i], log = "y", ylim = c(min(syto_unstained[,2:10]), max(syto_unstained[,2:10])),main = List_treat_stained[[i-1]],xlab = "time[h]", ylab = "SYTO-9 count")
}

#-------------------------------mCHE_stained

cher_stained = data.frame(time, List_treat_stained)
for(i in 1:18){
  data = get(List_treat_stained[[i]])
  cher_stained[,i+1] = data
}
names(cher_stained) = c("time", List_treat_stained)

#remplir le Data frame
for(i in 1:8){
  for(j in 1:18){
    Data = get(List[[i]])
    print(lech[j])
    cher_stained[i,j+1] = mean(Data$mChe.SYTO.9.Abs..Count[Data$Well.ID == lech[j]][1:2])
    
  }}
#graphiques

par(mfrow = c(3,3))
for(i in 2:10){
  plot(cher_stained[,1],cher_stained[,i], log = "y", ylim = c(min(cher_stained[,2:10]), max(cher_stained[,2:10])),main = List_treat_stained[[i-1]], xlab = "time[h]", ylab = "mChe count")
}

for(i in 11:19){
  plot(cher_stained[,1],cher_stained[,i], log = "y",ylim = c(min(cher_stained[,2:10]), max(cher_stained[,2:10])), main = List_treat_stained[[i-1]],xlab = "time[h]", ylab = "mChe count")
}



#-------------------------------mCHE_UNstained

cher_unstained = data.frame(time, List_treat_unstained)
for(i in 1:18){
  data = get(List_treat_unstained[[i]])
  cher_unstained[,i+1] = data
}
names(cher_unstained) = c("time", List_treat_unstained)

#remplir le Data frame
for(i in 1:8){
  for(j in 1:18){
    Data = get(List[[i]])
    print(lech_un[j])
    cher_unstained[i,j+1] = mean(Data$mChe.SYTO.9.Abs..Count[Data$Well.ID == lech_un[j]][1:2])
    
  }}

#graphiques

par(mfrow = c(3,3))
for(i in 2:10){
  plot(cher_unstained[,1],cher_unstained[,i], log = "y",ylim = c(min(cher_unstained[,2:10]), max(cher_unstained[,2:10])), main = List_treat_unstained[[i-1]], xlab = "time[h]", ylab = "mChe count")
}

for(i in 11:19){
  plot(cher_unstained[,1],cher_unstained[,i], log = "y",ylim = c(min(cher_unstained[,2:10]), max(cher_unstained[,2:10])), main = List_treat_unstained[[i-1]],xlab = "time[h]", ylab = "mChe count")
}


### ------------------------------------------------ALL- ABSOLUT COUNT STAINED
#créer le data frame pour acceuillir les données
absol_stained = data.frame(time, List_treat_stained)
for(i in 1:18){
  data = get(List_treat_stained[[i]])
  absol_stained[,i+1] = data
}
names(absol_stained) = c("time", List_treat_stained)

#remplir
for(i in 1:8){
  for(j in 1:18){
    Data = get(List[[i]])
    print(lech[j])
    absol_stained[i,j+1] = mean(Data$All.Abs..Count[Data$Well.ID == lech[j]][1:2])
    
  }}

#graphiques
par(mfrow = c(3,3))
for(i in 2:10){
  plot(absol_stained[,1],absol_stained[,i], ylim = c(min(absol_stained[,2:10]), max(absol_stained[,2:10])), log = "y", main = List_treat_stained[[i-1]], xlab = "time[h]", ylab = "Absolut count")
}

for(i in 11:19){
  plot(absol_stained[,1],absol_stained[,i],  ylim = c(min(absol_stained[,2:19]), max(absol_stained[,2:19])),log = "y", main = List_treat_stained[[i-1]],xlab = "time[h]", ylab = "Absolut count")
}


### ------------------------------------------------ALL- ABSOLUT COUNT UNSTAINED
#créer le data frame pour acceuillir les données
absol_unstained = data.frame(time, List_treat_unstained)
for(i in 1:18){
  data = get(List_treat_unstained[[i]])
  absol_unstained[,i+1] = data
}
names(absol_unstained) = c("time", List_treat_unstained)

#remplir
for(i in 1:8){
  for(j in 1:18){
    Data = get(List[[i]])
    print(lech[j])
    absol_unstained[i,j+1] = mean(Data$All.Abs..Count[Data$Well.ID == lech[j]][1:2])
    
  }}

#graphiques
par(mfrow = c(3,3))
for(i in 2:10){
  plot(absol_unstained[,1],absol_unstained[,i], ylim = c(min(absol_unstained[,2:10]), max(absol_unstained[,2:10])), log = "y", main = List_treat_unstained[[i-1]], xlab = "time[h]", ylab = "Absolut count")
}

for(i in 11:19){
  plot(absol_unstained[,1],absol_unstained[,i],  ylim = c(min(absol_unstained[,2:19]), max(absol_unstained[,2:19])),log = "y", main = List_treat_unstained[[i-1]],xlab = "time[h]", ylab = "Absolut count")
}





