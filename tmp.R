trainData <- read.csv("ks-projects-processed-train.csv")
testData <- read.csv("ks-projects-processed-test.csv")



binAsign <- function(bins, values){
  resultVector <- vector()
  tempBin <- 0
  
  for (value in values) {
    tempBin <- 0
    
    for (bin in bins) {
      if(value >= bin) #TODO: optimizar el corte
        tempBin <- bin    
    }
    
    resultVector <- c(resultVector, tempBin)
  }
  
  return(resultVector)
}

bins2Text <- function(bins){
  labels <- vector()
  lastBin <- max(bins)
  
  for (i in seq(length(bins))) {
    if(bins[i] == lastBin)
      labels <- c(labels, paste(">", bins[i]))
    else
      labels <- c(labels, sprintf("%s a %s", bins[i], bins[i + 1] - 1))
  }
  
  return(labels)
}



days  <- trainData$days_funding
minimo <- min(days)
maximo <- max(days)
rango <- maximo - minimo
cantBins <- 10
tamanioBin <- round(rango / cantBins)
bins <- seq(minimo, cantBins * tamanioBin, tamanioBin)
textBins <- bins2Text(bins)
binsAssigned <- binAsign(bins, days)
assigned <- data.frame(days, binsAssigned, textBins[match(binsAssigned, bins)])

