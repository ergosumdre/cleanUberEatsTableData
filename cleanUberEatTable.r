cleanUberEatsTable <- function(x){
  loc <- read.csv(x)
  loc2 <- loc
  # Searches for missing values
  emptyLocation2 <- loc2[,1] == ""
  emptyRecords <- loc2[,2] == ""
  
  # Parse out Location into df 'Location'
  emptyLocIndex <- which(emptyLocation2, TRUE)
  emptryRecordsIndex <- which(emptyRecords, TRUE)
  df1 <- loc2[-emptryRecordsIndex,]
  location <- data.frame(loc2[emptryRecordsIndex,][1])
  location <- data.frame(location)
  
  # Parse out Requested and Miles into df 'requestedandMiles'
  emptyRequested <- loc2[,2] == "Requested"
  emptyRequestedIndex <- which(emptyRequested, TRUE)
  requestedandMiles <- loc2[-emptyRequestedIndex,]
  requestedandMiles <- data.frame(requestedandMiles)
  locationData <- requestedandMiles[,1]
  requestedandMiles <- requestedandMiles[,-1]
  
  # Remove missing data
  missingData <- requestedandMiles[,2] == ""
  missingDataIndex <- which(missingData, TRUE)
  requestedandMiles <- requestedandMiles[-missingDataIndex,]
  
  # Move delivery col up
  missingLocationData <- locationData == ""
  missingLocationDataIndex <- which(missingLocationData, TRUE)
  locationData <- locationData[-missingLocationDataIndex]
  
  # Good 
  df5 <- data.frame(requestedandMiles, locationData)
}


x <- "/Users/dre/Downloads/Book5.csv"
ueData <- cleanUberEatsTable(x)
