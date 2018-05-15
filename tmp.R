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


discretizarIgualAncho <- function(df, nombreCol, cantBins, replaceCol){
  colIdx <- match(nombreCol, colnames(df))
  valores <- df[, colIdx]
  minimo <- min(valores)
  maximo <- max(valores)
  rango <- maximo - minimo
  tamanioBin <- round(rango / cantBins)
  bins <- seq(minimo, cantBins * tamanioBin, tamanioBin)
  textBins <- bins2Text(bins)
  binsAssigned <- binAsign(bins, valores)
  textBinsAssigned <- textBins[match(binsAssigned, bins)]
  
  if(replaceCol) {
    df[, colIdx] <- textBinsAssigned
  } else {
    df <- cbind(df, textBinsAssigned)
    colnames(df)[ncol(df)] <- paste(nombreCol, "_discrete", sep = "")
  }
  
  return(df)
}

trainCopy <- trainData
trainCopy <- discretizarIgualAncho(trainCopy, "days_funding", 5, TRUE)
head(discretizarIgualAncho(trainCopy, "usd_goal", 5, TRUE))






