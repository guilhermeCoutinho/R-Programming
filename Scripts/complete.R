complete = function (directory , id) {
      ret = data.frame()
      for (i in id) {
            data = !is.na(read.csv( paste (directory, "\\" , dir(directory)[i] , sep="" )))
            count = 0
            for (j in 1:nrow(data)) {
                  if (data[j,2] == T & data[j,3] == T) {
                        count = count + 1
                  }
            }
            ret = rbind(ret , data.frame(i,count) )
            
      }
      
      names(ret) = c("id" , "nobs")
      ret      
}