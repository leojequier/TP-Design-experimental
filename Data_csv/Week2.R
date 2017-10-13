#charger les données, après avoir fait session, set working directory, to source file location
path = "Data_W2/T"
List = vector(mode = "list", length = 8) #sorte de liste qui peut contenir des variables
csv = c()
for(i in 1:8){
  List[[i]]= paste("T" , i-1, sep = "")
  csv[i] = paste(path,i-1,".csv", sep = "")
  assign(paste("T" , i-1, sep = ""), read.csv(csv[i], header = T))
  
}

