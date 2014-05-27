rankall = function (outcome , num = "best") {
     isDebugMode = F
     if (isDebugMode){
          outcome = "heart attack"
          num = 20
     }
     data = read.csv("outcome-of-care-measures.csv")

     if (outcome == "heart attack"){
          outcome = names(data)[11]
     }else if (outcome == "heart failure"){
          outcome = names(data)[17]
     }else if (outcome == "pneumonia"){
          outcome = names(data)[23]
     }else {
          stop ("invalid outcome")
     }
     data = data[,c("Hospital.Name",outcome,"State")]
     data = data[,][data[,outcome] != "Not Available" ,]
     output = data.frame()
     if (num == "best"){
          num = 1
     }
     
     
     for (i in seq(levels(data$State))){
          i = levels(data$State)[i]
          aux = data[data$State == i ,]
          aux = aux[order(aux[outcome] , aux$Hospital.Name) , ]
     
          if (num=="worst"){
               num = nrow(aux)
          }
          output = rbind(output , cbind(toString(aux[num,1]) , toString(aux[1,3]) ))
     }
     output
}
