pollutantmean <- function(directory, pollutant, id = 1:332) {
      data = data.frame()
      for (i in id) {
            data = rbind (data , read.csv( paste (directory, "\\" , dir(directory)[i] , sep="" )) )
      }       
      mean (data[,pollutant], na.rm=T)
}

