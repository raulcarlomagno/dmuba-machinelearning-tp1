trainData <- read.csv("ks-projects-processed-train.csv")
testData <- read.csv("ks-projects-processed-test.csv")


#valores = c(0, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 6, 7, 7, 7, 8, 9, 9, 9, 19, 11, 12, 13)
#set.seed(unclass(Sys.time()))
#valoresDesorden = sample(valores)

valoresDesorden = trainData$usd_goal

cantUnicos = length(unique(valoresDesorden))
unicos = sort(unique(valoresDesorden))
cantBins = 1
cantPorBin = ceiling(cantUnicos/cantBins)
#valoresDesorden

bins = c()
cont = 0
for(nro in unicos){
  if(cont == cantPorBin | cont == 0){
    cont = 0
    bins = c(bins, nro)  
  }
  cont = cont + 1 
}
unicos
bins


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
textBins <- bins2Text(bins)



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
binsAssigned <- binAsign(bins, valoresDesorden)


textBinsAssigned <- textBins[match(binsAssigned, bins)]
#textBinsAssigned


df <-data.frame(valoresDesorden, binsAssigned, textBinsAssigned)
View(df)
View(table(df$textBinsAssigned))
