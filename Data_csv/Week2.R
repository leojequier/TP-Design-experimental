rm(list=ls())


#charger les données, après avoir fait session, set working directory, to source file location
path = "Data_W2_csv2/T"
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
T0$All.Abs..Count = T0$All.Abs..Count
T0$mChe.SYTO.9.Abs..Count = T0$mChe.SYTO.9.Abs..Count
T0$SYTO.9.Abs..Count = T0$SYTO.9.Abs..Count
T1$All.Abs..Count = T1$All.Abs..Count*10
T1$mChe.SYTO.9.Abs..Count = T1$mChe.SYTO.9.Abs..Count*10
T1$SYTO.9.Abs..Count = T1$SYTO.9.Abs..Count*10
T2$All.Abs..Count = T2$All.Abs..Count*10
T2$mChe.SYTO.9.Abs..Count = T2$mChe.SYTO.9.Abs..Count*10
T2$SYTO.9.Abs..Count = T3$SYTO.9.Abs..Count*10
T3$All.Abs..Count = T3$All.Abs..Count*10
T3$mChe.SYTO.9.Abs..Count = T3$mChe.SYTO.9.Abs..Count*10
T3$SYTO.9.Abs..Count = T3$SYTO.9.Abs..Count*10
T4$All.Abs..Count = T4$All.Abs..Count*10
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
  assign(List_treat_stained[[i]], rep(NA, times = 9))
}

List_treat_unstained = vector(mode = "list", length = 18)
for(i in 1:18){
  List_treat_unstained[[i]] = names_unstained[i]
  assign(List_treat_unstained[[i]], rep(NA, times = 9))
}

#vecteur contenant le temps en heure
time = c(0, 14.5, 18, 21, 23.5, 36.5, 42 ,44.5 , 47.5 )

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
for(i in 1:9){ 
  for(j in 1:18){
    Data = get(List[[i]]) #stocke T[I]
    print(lech[j]) 
    syto_stained[i,j+1] = mean(Data$SYTO.9.Abs..Count[Data$Well.ID== lech[j]][1:2])
    #parcours syto 2 en ligne, prend les comptes absolu syto9 de T[i] qui ont le nom lech[j] (deux replicat tech) et fait une moyenne 
  }}

#remplir avec SYTO 9 pour tous les traitements stained
for(i in 1:9){ 
  for(j in 1:18){
    Data = get(List[[i]]) #stocke T[I]
    print(lech_un[j]) 
    syto_unstained[i,j+1] = mean(Data$SYTO.9.Abs..Count[Data$Well.ID== lech_un[j]][1:2])
    #parcours syto 2 en ligne, prend les comptes absolu syto9 de T[i] qui ont le nom lech[j] (deux replicat tech) et fait une moyenne 
  }}


#graphiques SYTO-9
par(mfrow = c(3,3))

for(i in 2:10){
  plot(syto_stained[,1],syto_stained[,i], log = "y", ylim = range(syto_stained[,c(2:15,17:19)]) ,main = List_treat_stained[[i-1]], xlab = "time[h]", ylab = "SYTO-9 count")
}

for(i in 11:19){
  plot(syto_stained[,1],syto_stained[,i], log = "y",ylim = range(syto_stained[,c(2:15,17:19)]),main = List_treat_stained[[i-1]],xlab = "time[h]", ylab = "SYTO-9 count")
}

#-------------------------------mCHE_stained

cher_stained = data.frame(time, List_treat_stained)
for(i in 1:18){
  data = get(List_treat_stained[[i]])
  cher_stained[,i+1] = data
}
names(cher_stained) = c("time", List_treat_stained)

#remplir le Data frame
for(i in 1:9){
  for(j in 1:18){
    Data = get(List[[i]])
    print(lech[j])
    cher_stained[i,j+1] = mean(Data$mChe.SYTO.9.Abs..Count[Data$Well.ID == lech[j]][1:2])
    
  }}
#graphiques

par(mfrow = c(3,3))
for(i in 2:10){
  plot(cher_stained[,1],cher_stained[,i], log = "y", ylim = range(cher_stained[2:19]),main = List_treat_stained[[i-1]], xlab = "time[h]", ylab = "mChe count")
}

for(i in 11:19){
  plot(cher_stained[,1],cher_stained[,i], log = "y",ylim = range(cher_stained[2:19]), main = List_treat_stained[[i-1]],xlab = "time[h]", ylab = "mChe count")
}



#-------------------------------mCHE_UNstained

cher_unstained = data.frame(time, List_treat_unstained)
for(i in 1:18){
  data = get(List_treat_unstained[[i]])
  cher_unstained[,i+1] = data
}
names(cher_unstained) = c("time", List_treat_unstained)

#remplir le Data frame
for(i in 1:9){
  for(j in 1:18){
    Data = get(List[[i]])
    print(lech_un[j])
    cher_unstained[i,j+1] = mean(Data$mChe.SYTO.9.Abs..Count[Data$Well.ID == lech_un[j]][1:2])
    
  }}

#graphiques

par(mfrow = c(3,3))
for(i in 2:10){
  plot(cher_unstained[,1],cher_unstained[,i], log = "y",ylim = range(cher_unstained[,2:19]), main = List_treat_unstained[[i-1]], xlab = "time[h]", ylab = "mChe count")
}

for(i in 11:19){
  plot(cher_unstained[,1],cher_unstained[,i], log = "y",ylim = range(cher_unstained[,2:19]), main = List_treat_unstained[[i-1]],xlab = "time[h]", ylab = "mChe count")
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
for(i in 1:9){
  for(j in 1:18){
    Data = get(List[[i]])
    print(lech[j])

    absol_stained[i,j+1] = mean(Data$SYTO.9.Abs..Count[Data$Well.ID == lech[j]][1:2]) + mean(Data$mChe.SYTO.9.Abs..Count[Data$Well.ID == lech[j]][1:2])
    
  }}

#graphiques
par(mfrow = c(3,3))
for(i in 2:10){
  plot(absol_stained[,1],absol_stained[,i], ylim = range(absol_stained[,2:19]), log = "y", main = List_treat_stained[[i-1]], xlab = "time[h]", ylab = "Absolut count")
}

for(i in 11:19){
  plot(absol_stained[,1],absol_stained[,i],  ylim = range(absol_stained[,2:19]),log = "y", main = List_treat_stained[[i-1]],xlab = "time[h]", ylab = "Absolut count")
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
for(i in 1:9){
  for(j in 1:18){
    Data = get(List[[i]])
    print(lech[j])
    absol_unstained[i,j+1] = mean(Data$All.Abs..Count[Data$Well.ID == lech[j]][1:2])
    
  }}

#graphiques
par(mfrow = c(3,3))
for(i in 2:10){
  plot(absol_unstained[,1],absol_unstained[,i], ylim = range(absol_unstained[,2:19]), log = "y", main = List_treat_unstained[[i-1]], xlab = "time[h]", ylab = "Absolut count")
}

for(i in 11:19){
  plot(absol_unstained[,1],absol_unstained[,i],  ylim = range(absol_unstained[,2:19]),log = "y", main = List_treat_unstained[[i-1]],xlab = "time[h]", ylab = "Absolut count")
}

#cell count
list_cell_count = vector(mode = "list", length = 32)
list_cell_count[1] = "time"
CvTol = rep(c("MixC", "Tol"), each = 16)
bac = rep(rep(c("SC", "PV", "PVSC_SC","PVSC_PV"), each = 4), time = 2)
n = rep(c(1,2,3,"mean"), times = 8)
for(i in 1:32){
  list_cell_count[i+1] = paste(CvTol[i], paste(bac[i], n[i], sep = ""), sep = "_")
}

## moyennes

for(i in seq(from = 5, to = length(list_cell_count), by = 4)){
  print(i)
  print(list_cell_count[i])
  assign(list_cell_count[[i]], rep(NA, times = 9))
}

cell_count = data.frame(time, syto_stained$MixCSC_stained1, syto_stained$MixCSC_stained2, syto_stained$MixCSC_stained3, get(list_cell_count[[5]])
                        
                        ,cher_stained$MixCPV_stained1,cher_stained$MixCPV_stained2,cher_stained$MixCPV_stained3, get(list_cell_count[[9]])
                        
                        ,syto_stained$MixCPVSC_stained1,syto_stained$MixCPVSC_stained2,syto_stained$MixCPVSC_stained3,get(list_cell_count[[13]])
                        
                        ,cher_stained$MixCPVSC_stained1, cher_stained$MixCPVSC_stained2, cher_stained$MixCPVSC_stained3, get(list_cell_count[[17]])
                        
                        ,syto_stained$ToluSC_stained1,syto_stained$ToluSC_stained2,syto_stained$ToluSC_stained3,get(list_cell_count[[21]])
                        
                        ,cher_stained$ToluPV_stained1,cher_stained$ToluPV_stained2,cher_stained$ToluPV_stained3, get(list_cell_count[[25]])
                        
                        ,syto_stained$ToluPVSC_stained1, syto_stained$ToluPVSC_stained2,syto_stained$ToluPVSC_stained3, get(list_cell_count[[29]])
                        
                        ,cher_stained$ToluPVSC_stained1,cher_stained$ToluPVSC_stained2,cher_stained$ToluPVSC_stained3, get(list_cell_count[[33]]))


names(cell_count) = list_cell_count
names(cell_count)

###CELL_COUNT2
MixC_SCmoy_G<- c()

for(i in 1:9){
  MixC_SCmoy_G[i] <- sum(syto_stained$MixCSC_stained1[i],  syto_stained$MixCSC_stained2[i], syto_stained$MixCSC_stained3[i])/3
}

MixC_SCmoy_R<- c()

for(i in 1:9){
  MixC_SCmoy_R[i] <- sum(cher_stained$MixCSC_stained1[i],  cher_stained$MixCSC_stained2[i], cher_stained$MixCSC_stained3[i])/3
}


Tol_SCmoy_G<- c()

for(i in 1:9){
  Tol_SCmoy_G[i] <- sum(syto_stained$ToluSC_stained1[i],  syto_stained$ToluSC_stained2[i], syto_stained$ToluSC_stained3[i])/3
}

Tol_SCmoy_R<- c()

for(i in 1:9){
  Tol_SCmoy_R[i] <- sum(cher_stained$ToluSC_stained1[i],  cher_stained$ToluSC_stained2[i], cher_stained$ToluSC_stained3[i])/3
}

Tol_PVSCmoy_G <-c()

for(i in 1:9){
  Tol_PVSCmoy_G[i] <- sum(syto_stained$ToluPVSC_stained1[i],  syto_stained$ToluPVSC_stained2[i], syto_stained$ToluPVSC_stained3[i])/3
}

Tol_PVSCmoy_R <-c()

for(i in 1:9){
  Tol_PVSCmoy_R[i] <- sum(cher_stained$ToluPVSC_stained1[i],  cher_stained$ToluPVSC_stained2[i], cher_stained$ToluPVSC_stained3[i])/3
}

MixC_PVSCmoy_G <-c()

for(i in 1:9){
  MixC_PVSCmoy_G[i] <- sum(syto_stained$MixCPVSC_stained1[i],  syto_stained$MixCPVSC_stained2[i], syto_stained$MixCPVSC_stained3[i])/3
}

MixC_PVSCmoy_R <-c()

for(i in 1:9){
  MixC_PVSCmoy_R[i] <- sum(cher_stained$MixCPVSC_stained1[i],  cher_stained$MixCPVSC_stained2[i], cher_stained$MixCPVSC_stained3[i])/3
}

# means PV

MixC_PVmoy_R<- c()

for(i in 1:9){
  MixC_PVmoy_R[i] <- sum(cher_stained$MixCPV_stained1[i], cher_stained$MixCPV_stained2[i],cher_stained$MixCPV_stained3[i])/3
}

MixC_PVmoy_G<- c()

for(i in 1:9){
  MixC_PVmoy_G[i] <- sum(syto_stained$MixCPV_stained1[i],  syto_stained$MixCPV_stained2[i],syto_stained$MixCPV_stained3[i])/3
}

Tol_PVmoy_R <- c()
for(i in 1:9){
  Tol_PVmoy_R[i] <- sum(cher_stained$ToluPV_stained1[i],  cher_stained$ToluPV_stained2[i],cher_stained$ToluPV_stained3[i])/3
}

Tol_PVmoy_G <- c()
for(i in 1:9){
  Tol_PVmoy_G[i] <- sum(syto_stained$ToluPV_stained1[i],  syto_stained$ToluPV_stained2[i],syto_stained$ToluPV_stained3[i])/3
}
#cell count
list_cell_count2 = vector(mode = "list", length = 49)
list_cell_count2[1] = "time"
CvTol = rep(c("MixC", "Tol"), each = 24)
bac = rep(rep(c("SC", "PV","PVSC"), each = 9), time = 2)
Fluo = rep(rep(c("G", "R"), each = 4), time = 6)
n = rep(c(1,2,3,"moy"), times = 12)
for(i in 1:48){
  list_cell_count2[i+1] = paste(CvTol[i], paste(bac[i], n[i], sep = ""),Fluo[i], sep = "_")
}
cell_count2 = data.frame(time, syto_stained$MixCSC_stained1, syto_stained$MixCSC_stained2, syto_stained$MixCSC_stained3,MixC_SCmoy_G,
                        cher_stained$MixCSC_stained1, cher_stained$MixCSC_stained2, cher_stained$MixCSC_stained3, MixC_SCmoy_R,
                        syto_stained$MixCPV_stained1, syto_stained$MixCPV_stained2, syto_stained$MixCPV_stained3,MixC_PVmoy_G,
                        cher_stained$MixCPV_stained1, cher_stained$MixCPV_stained2, cher_stained$MixCPV_stained3, MixC_PVmoy_R,
                        syto_stained$MixCPVSC_stained1, syto_stained$MixCPVSC_stained2, syto_stained$MixCPVSC_stained3, MixC_PVSCmoy_G,
                        cher_stained$MixCPVSC_stained1, cher_stained$MixCPVSC_stained2, cher_stained$MixCPVSC_stained3, MixC_PVSCmoy_R, 
                        syto_stained$ToluSC_stained1, syto_stained$ToluSC_stained2, syto_stained$ToluSC_stained3, Tol_SCmoy_G,
                        cher_stained$ToluSC_stained1, cher_stained$ToluSC_stained2, cher_stained$ToluSC_stained3, Tol_SCmoy_R,
                        syto_stained$ToluPV_stained1, syto_stained$ToluPV_stained2, syto_stained$ToluPV_stained3, Tol_PVmoy_G, 
                        cher_stained$ToluPV_stained1, cher_stained$ToluPV_stained2, cher_stained$ToluPV_stained3, Tol_PVmoy_R,
                        syto_stained$ToluPVSC_stained1, syto_stained$ToluPVSC_stained2,syto_stained$ToluPVSC_stained3,Tol_PVSCmoy_G,
                        cher_stained$ToluPVSC_stained1,cher_stained$ToluPVSC_stained2,cher_stained$ToluPVSC_stained3, Tol_PVSCmoy_R)
names(cell_count2) = list_cell_count2
names(cell_count2)


MixC_PVSC1 <- cell_count$MixC_PVSC_SC1 + cell_count$MixC_PVSC_PV1
MixC_PVSC2 <- cell_count$MixC_PVSC_SC2 + cell_count$MixC_PVSC_PV2
MixC_PVSC3 <- cell_count$MixC_PVSC_SC3 + cell_count$MixC_PVSC_PV3
MixC_PVSCmoy <- (MixC_PVSC1+ MixC_PVSC2+ MixC_PVSC3)/3

Tol_PVSC1 <- cell_count$Tol_PVSC_SC1 + cell_count$Tol_PVSC_PV1
Tol_PVSC2 <- cell_count$Tol_PVSC_SC2 + cell_count$Tol_PVSC_PV2
Tol_PVSC3 <- cell_count$Tol_PVSC_SC3 + cell_count$Tol_PVSC_PV3
Tol_PVSCmoy <- (Tol_PVSC1+ Tol_PVSC2+ Tol_PVSC3)/3

cell_count2 <- cbind(cell_count, MixC_PVSC1, MixC_PVSC2,MixC_PVSC3, MixC_PVSCmoy, Tol_PVSC1, Tol_PVSC2, Tol_PVSC3, Tol_PVSCmoy)



##remplir les moyennes

for(i in 1:9){cell_count[i,5] = sum(c(syto_stained$MixCSC_stained1[i], syto_stained$MixCSC_stained2[i], syto_stained$MixCSC_stained3[i]))/3}
for(i in 1:9){cell_count[i,9] =mean(c(cher_stained$MixCPV_stained1[i],cher_stained$MixCPV_stained2[i],cher_stained$MixCPV_stained3[i]))}
for(i in 1:9){cell_count[i,13] =mean(c(syto_stained$MixCPVSC_stained1[i],syto_stained$MixCPVSC_stained2[i],syto_stained$MixCPVSC_stained3[i]))}
for(i in 1:9){cell_count[i,17] =mean(c(cher_stained$MixCPVSC_stained1[i], cher_stained$MixCPVSC_stained2[i], cher_stained$MixCPVSC_stained3[i]))}
for(i in 1:9){cell_count[i,21] =mean(c(syto_stained$ToluSC_stained1[i],syto_stained$ToluSC_stained2[i],syto_stained$ToluSC_stained3[i]))}
for(i in 1:9){cell_count[i,25] =mean(c(cher_stained$ToluPV_stained1[i],cher_stained$ToluPV_stained2[i],cher_stained$ToluPV_stained3[i]))}
for(i in 1:9){cell_count[i,29] =mean(c(syto_stained$ToluPVSC_stained1[i], syto_stained$ToluPVSC_stained2[i],syto_stained$ToluPVSC_stained3[i]))}
for(i in 1:9){cell_count[i,33] =mean(c(cher_stained$ToluPVSC_stained1[i],cher_stained$ToluPVSC_stained2[i],cher_stained$ToluPVSC_stained3[i]))}

## graphiques finaux, copié collé W1
#graphiques



par(mfrow = c(1,1))

totsc <- data.frame(time, MixC_SCmoy, Tol_SCmoy, MixC_PPSC_SCmoy, Tol_PPSC_SCmoy)

#ppmortes dans ppseul
Tol_deadPVmoy<- c()

for(i in 1:9){
  Tol_deadPVmoy[i] <- sum(syto_stained$ToluPV_stained1[time==time[i]],
                          syto_stained$ToluPV_stained2[time==time[i]],
                          syto_stained$ToluPV_stained3[time==time[i]])/3
}

#total dans ppseul
Tol_all_in_pv = c()
for(i in 1:9){
  Tol_all_in_pv[i]  <- Tol_deadPVmoy[i] + cell_count$Tol_PVmean[i]
}

#Plot des comptes de SC
par(mfrow = c(1,1))
plot(cell_count$time, cell_count$MixC_SCmean,lwd=1.3,log = "y",xlim=c(0,48),ylim=c(2950,4028683330), type = "o",main="SC growth\nWeek2 ",xlab= "Time [Hours]", ylab="log(SC count)")

points(cell_count$time,cell_count$Tol_SCmean,type="o", col="red",lwd=1.3)
points(cell_count$time,cell_count$MixC_PVSC_SCmean, type="o", col="blue",lwd=1.3)
points(cell_count$time, cell_count$Tol_PVSC_SCmean, type = "o", col = "orange",lwd=1.3)
#points(cell_count$time, Tol_deadPVmoy, type = "o", col = "green",lwd=1.3)
#points(cell_count$time, cell_count$MixC_PVSC_PVmean+cell_count$MixC_PVSC_SCmean,type = "o", col = "brown",lwd=1.3)
#points(cell_count$time, cell_count$Tol_PVSC_PVmean+ cell_count$Tol_PVSC_SCmean,type = "o", col = "grey" ,lwd=1.3)
#points(cell_count$time, Tol_all_in_pv, type = "o", col = "pink" ,lwd=1.5)#

#
legend("topleft",legend=c("SC alone in mixC","SC alone in Tol","SC in PV+SC in mixC","SC in PV+SC in Tol"),fill=c("black","red","blue","orange"))
#legend("bottomright", legend=c("PVSC total in mixC","PVSC total in Tol","total count in pv alone"), fill=c("brown", "gray", "pink"))


#SAME WITH PV



#totPV <- data.frame(time, MixC_PVmoy, Tol_PVmoy, MixC_PVSC_PVmoy, Tol_PVSC_PVmoy)
plot(cell_count$time, cell_count$MixC_PVmean,log="y",lwd=1.3,xlim=c(0,48),ylim=c(2950,4028683330), type="o",main="PV growth\nWeek2",xlab= "Time [Hours]", ylab="log(PV count)")

#Plot des comptes de PV
points(cell_count$time,cell_count$Tol_PVmean, type="o", col="red",lwd=1.3)
points(cell_count$time,cell_count$MixC_PVSC_PVmean, type="o", col="blue",lwd=1.3)
points(cell_count$time,cell_count$Tol_PVSC_PVmean, type="o", col="orange",lwd=1.3)
#points(cell_count$time, cell_count$MixC_PVSC_SCmean + cell_count$MixC_PVSC_PVmean, type="o", col="brown",lwd=1.3)
#points(cell_count$time, cell_count$Tol_PVSC_SCmean + cell_count$Tol_PVSC_PVmean, type="o", col="gray",lwd=1.3)
legend("topleft",legend=c("PV alone in mixC","PV alone in Tol","PV in PV+SC in mixC","PV in PV+SC in Tol"),fill=c("black","red","blue","orange"))

par(mfrow=c(1,1))

plot(cell_count$time, Tol_PVmoy_G+Tol_PVmoy_R,type = "o",
     log = "y", main = "Total count in Toluene \n Week 2",
     ylab = "Number of cells", xlab = "Time[hours]", col = "green",ylim=c(2950,4028683330))
points(cell_count$time, Tol_PVSCmoy_G+Tol_PVSCmoy_R, type = "o", col = "orange")
points(cell_count$time,  Tol_SCmoy_G+ Tol_SCmoy_R, type = "o", col = "red")
legend(x="topleft", legend = c("Tol SC alone", "Tol PV alone", "Tol PVSC"), fill=c("red", "green", "orange"))


plot(cell_count$time, MixC_PVSCmoy_G+ MixC_PVSCmoy_R,type = "o",
     log = "y", main = "Total count in Mixed Carbon \n Week 2",
     ylab = "Number of cells", xlab = "Time[hours]", col = "blue",ylim=c(2950,4028683330))
points(cell_count$time,  MixC_PVmoy_G+ MixC_PVmoy_R, type = "o", col = "black")
points(cell_count$time,  MixC_SCmoy_G+ MixC_SCmoy_R, type = "o", col = "purple")
legend(x="topleft",legend=c("MixC SC alone", "MixC PV alone", "MixC PVSC"), fill=c( "purple", "black", "blue"))

