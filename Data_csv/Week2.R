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

#STAINED noms de traitements: Toluène vs mix C, SC vs PP vs PPSC, replicat
mil_stain = rep(c("MixC","Tolu" ), each = 9)
tre_stain = rep(rep(c("SC", "PV", "PVSC"), each = 3),times = 2)
n_stain = rep(c(1,2,3), times= 6)
names_stain = c()
for(i in 1:18){
  names_stain[i] = paste(mil_stain[i], tre_stain[i], n_stain[i], sep = "")
}

#UNSTAINED noms de traitements: Toluène vs mix C, SC vs PP vs PPSC, replicat
mil_unstain = rep(c("MixC","Tolu" ), each = 9)
tre_unstain = rep(rep(c("SC", "PV", "PVSC"), each = 3),times = 2)
n_unstain = rep(c(1,2,3), times= 6)
names_unstain = c()
for(i in 1:18){
  names_unstain[i] = paste(mil_unstain[i], tre_unstain[i], n_unstain[i], sep = "")
}


###------------------------------------------------------------------ SYTO-9
##créer un data frame pour acceuilir les données dans le bon sens
#créer une liste contenant les noms de traitement et leur assigner un vecteur 8xNA 


List_treat_stain = vector(mode = "list", length = 18)
for(i in 1:18){
  List_treat_stain[[i]] = names_stain[i]
  assign(List_treat_stain[[i]], rep(NA, times = 8))
}

List_treat_unstain = vector(mode = "list", length = 18)
for(i in 1:18){
  List_treat_unstain[[i]] = names_unstain[i]
  assign(List_treat_unstain[[i]], rep(NA, times = 8))
}

#vecteur contenant le temps en heure
time = c(0, 14, 17, 20, 34, 39, 42 ,45 )

#création du data.frame STAIN
syto_stain = data.frame(time, MixCPV1)
for(i in 2:18){
  data_stain = get(List_treat_stain[[i]])
  syto_stain[,i+1] = data_stain
}
names(syto_stain) = c("time", List_treat_stain)

#création du data.frame UNSTAIN
syto_unstain = data.frame(time, MixCPV1)
for(i in 2:18){
  data_unstain = get(List_treat_unstain[[i]])
  syto_unstain[,i+1] = data_unstain
}
names(syto_unstain) = c("time", List_treat_unstain)


#créer un vecteur contenant les noms des puits d'intérêt STAIN
let = rep(c(rep("A", times = 3),rep("C", times = 3),rep("E", times = 3)),times = 2)
chi = c(rep(c(1,2,3), times = 3), rep(c(5,6,7),times= 3))
lech = c()
for(i in 1:18){
  lech[i] = paste(let[i], chi[i], sep = "")
} 
#créer un vecteur contenant les noms des puits d'intérêt UNSTAIN
chi_un = c(rep(c(7,8,9), times = 3), rep(c(10,11,12),times= 3))
lech_un = c()
for(i in 1:18){
  lech_un[i] = paste(let[i], chi_un[i], sep = "")
} 

#remplir avec SYTO 9 pour tous les traitements STAIN
for(i in 1:8){ 
  for(j in 1:18){
    Data = get(List[[i]]) #stocke T[I]
    print(lech[j]) 
    syto_stain[i,j+1] = mean(Data$SYTO.9.Abs..Count[Data$Well.ID== lech[j]][1:2])
    #parcours syto 2 en ligne, prend les comptes absolu syto9 de T[i] qui ont le nom lech[j] (deux replicat tech) et fait une moyenne 
  }}

#remplir avec SYTO 9 pour tous les traitements STAIN
for(i in 1:8){ 
  for(j in 1:18){
    Data = get(List[[i]]) #stocke T[I]
    print(lech_un[j]) 
    syto_unstain[i,j+1] = mean(Data$SYTO.9.Abs..Count[Data$Well.ID== lech_un[j]][1:2])
    #parcours syto 2 en ligne, prend les comptes absolu syto9 de T[i] qui ont le nom lech[j] (deux replicat tech) et fait une moyenne 
  }}


#graphiques SYTO-9
par(mfrow = c(3,3))
for(i in 2:10){
  plot(syto_stain[,1],syto_stain[,i], log = "y", main = List_treat_stain[[i-1]], xlab = "time[h]", ylab = "SYTO-9 count")
}

for(i in 11:19){
  plot(syto_stain[,1],syto_stain[,i], log = "y", main = List_treat_stain[[i-1]],xlab = "time[h]", ylab = "SYTO-9 count")
}


