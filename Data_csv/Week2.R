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






