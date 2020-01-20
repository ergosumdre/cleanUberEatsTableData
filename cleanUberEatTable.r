cleanUberEatsTable <- function(x){
  loc <- read.csv(x)
  loc2 <- loc
  # Searches for missing values
  emptyLocation2 <- loc2[,1] == ""
  emptyRecords <- loc2[,2] == ""
  
  # Parse out Location into df 'Location'
  emptyLocIndex <- which(emptyLocation2, TRUE)
  emptryRecordsIndex <- which(emptyRecords, TRUE)
  #df1 <- loc2[-emptryRecordsIndex,]
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
  locationData2 <- df5[,7]
  locationData2 <- gsub("Delivery ", "",locationData2)
  testList <- strsplit(as.character(locationData2), "EST ")
  # pingTime
  pingTime <- ""
  for(i in 1:length(testList)){
    pingTime[i] <- testList[[i]][1]
  }
  # pickupLocation
  pickupLocation <- ""
  for(i in 1:length(testList)){
    pickupLocation[i] <- testList[[i]][2]
  }
  # pickupTime
  pickupTime <- ""
  for(i in 1:length(testList)){
    pickupTime[i] <- testList[[i]][2]
  }
  # dropoff Location
  dropoffLocation <- ""
  for(i in 1:length(testList)){
    dropoffLocation[i] <- testList[[i]][4]
  }
  # dropoffTime 
  dropoffTime <- ""
  for(i in 1:length(testList)){
    dropoffTime[i] <- testList[[i]][3]
  }
  # pickupLocation <- str_trim(substr(pickupTime,nchar(pickupTime)-8,nchar(pickupTime)))
  pickupLocation <- as.character(pickupLocation)
  pickupLocation <- str_trim(substr(pickupLocation,1,nchar(pickupLocation)-8))
  pickupAddress <- (as.character(str_split_fixed(pickupLocation, ", ", 2)))
  pickupAddress <- pickupAddress[(length(pickupAddress)/2+1):length(pickupAddress)]
  pickupName <- pickupLocation[1:(length(pickupLocation)/2)]
  dropoffTime <- str_trim(substr(dropoffTime,nchar(dropoffTime)-8,nchar(dropoffTime)))
  pickupTime <- str_trim(substr(pickupTime,nchar(pickupTime)-8,nchar(pickupTime)))
  dfFinal <- data.frame(requestedandMiles,pingTime, pickupName, pickupAddress,pickupTime, dropoffLocation, dropoffTime)
  dir = list.files(pattern="statement*")
  df <- do.call(rbind.fill,lapply(dir,read.csv))
  df[,7] <- as.numeric(gsub("[\\$,]", "", df[,7]))
  df[,8]<- as.numeric(gsub("[\\$,]", "", df[,8]))
  df[,9] <- as.numeric(gsub("[\\$,]", "", df[,9]))
  df[,10] <- as.numeric(gsub("[\\$,]", "", df[,10]))
  df[,11] <- as.numeric(gsub("[\\$,]", "", df[,11]))
  df[,12] <- as.numeric(gsub("[\\$,]", "", df[,12]))
  df[,13] <- as.numeric(gsub("[\\$,]", "", df[,13]))
  df[,14] <- as.numeric(gsub("[\\$,]", "", df[,14]))
  df[,8][is.na(df[,8])] <- 0
  df[,9][is.na(df[,9])] <- 0
  df[,10][is.na(df[,10])] <- 0
  df[,11][is.na(df[,11])] <- 0
  df[,12][is.na(df[,12])] <- 0
  df[,13][is.na(df[,13])] <- 0
  df[,14][is.na(df[,14])] <- 0
  date1 <- df$Date.Time
  date1 <- gsub("Monday, ", "", date1)
  date1 <- gsub("Tuesday, ", "", date1)
  date1 <- gsub("Wednesday, ", "", date1)
  date1 <- gsub("Thursday, ", "", date1)
  date1 <- gsub("Friday, ", "", date1)
  date1 <- gsub("Saturday, ", "", date1)
  date1 <- gsub("Sunday, ", "", date1)
  date1 <- gsub("January ", "01/", date1)
  date1 <- gsub("December ", "12/", date1)
  date1 <- gsub(", 2019 ", "/2019 ", date1)
  date1 <- gsub(", 2020 ", "/2020 ", date1)
  date1 <- gsub("/1/", "/01/", date1)
  date1 <- gsub("/2/", "/02/", date1)
  date1 <- gsub("/3/", "/03/", date1)
  date1 <- gsub("/4/", "/04/", date1)
  date1 <- gsub("/5/", "/05/", date1)
  date1 <- gsub("/6/", "/06/", date1)
  date1 <- gsub("/7/", "/07/", date1)
  date1 <- gsub("/8/", "/08/", date1)
  date1 <- gsub("/9/", "/09/", date1)
  df$combineByDate <- date1
  dfFinal$Requested <- gsub(" EST", "", dfFinal$Requested)
  dfFinal$combineByDate <- dfFinal$Requested
  hellodz <- merge(df, dfFinal)
  hellodz2 <- hellodz$Trip.ID
  Dups <- duplicated(hellodz2)
  dupIndex <- which(Dups, TRUE)
  final <- hellodz[-dupIndex,]
  final <- data.frame(final$Driver.Name, final$Date.Time, final$Trip.ID, final$Base.Fare, final$Tip, final$Trip.Supplement, final$Toll, final$Surge, final$Boost, final$Total, final$Mileage, final$pingTime, final$pickupName, final$pickupAddress, final$pickupTime, final$dropoffLocation, final$dropoffTime)
}
