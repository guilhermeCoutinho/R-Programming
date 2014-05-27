rankall = function (outcome , num = "best") {
     isDebugMode = F
     if (isDebugMode){
          outcome = "pneumonia"
          num = "worst"
     }
     data = read.csv("outcome-of-care-measures.csv" , stringsAsFactors=F)
     
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
     data$State = as.factor(data$State)
     output = data.frame()
     if (num == "best"){
          num = 1
     }
     isWorst = F
     if (num == "worst"){
          isWorst = T
     }
     
     for (i in seq(levels(data$State))){
          i = levels(data$State)[i]
          aux = data[data$State == i ,]
          aux[,outcome] = as.numeric(aux[,outcome])
          aux = aux[order(aux[outcome] , aux$Hospital.Name) , ]
          if (isWorst){
               num = nrow(aux)
          }
          output = rbind(output , cbind(toString(aux[num,1]) , toString(aux[1,3]) ))
     }
     output
}
