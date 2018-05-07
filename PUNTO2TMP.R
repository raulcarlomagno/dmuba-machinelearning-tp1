#install.packages("RWeka")
library("RWeka")
library("ggplot2")
library("partykit")

trainData <- read.csv("ks-projects-processed-train.csv")
testData <- read.csv("ks-projects-processed-test.csv")

#WOW("J48")

dfPerformance <- data.frame(minNumObj = double(), accuracy = double(), set = character(), stringsAsFactors = FALSE)
dfSize <- data.frame(minNumObj = double(), leavesNumber = integer(), treeSize = integer())

minNumObjStart <- round(0.005 * nrow(trainData))
minNumObjEnd <- round(0.10 * nrow(trainData))
minNumObjStep <- round(0.005 * nrow(trainData))


#confidenceFactor -- The confidence factor used for pruning (smaller values incur more pruning).
for(minNumObj in seq(minNumObjStart, minNumObjEnd, minNumObjStep)) {
  print(paste("ENTRENANDO ARBOL CON minNumObj =", minNumObj))
  J48Options <- Weka_control(M = minNumObj)
  model <- J48(funded ~ ., data = trainData, control = J48Options)
  partyModel <- as.party(model)
  dfSize[nrow(dfSize) + 1, ] <- c(minNumObj, width(partyModel), length(partyModel))
  
  crossValEval <- evaluate_Weka_classifier(model, numFolds = 10, class = TRUE)
  testEval <- evaluate_Weka_classifier(model, newdata = testData, class = TRUE)
  
  print(sprintf("TRAINING - PORCENTAJE CORRECTOS - %.2f", crossValEval$details[["pctCorrect"]]))
  print(sprintf("TESTING - PORCENTAJE CORRECTOS - %.2f", testEval$details[["pctCorrect"]]))
  
  dfPerformance[nrow(dfPerformance) + 1, ] <- c(minNumObj, round(crossValEval$details[["pctCorrect"]], 2), "training")
  dfPerformance[nrow(dfPerformance) + 1, ] <- c(minNumObj, round(testEval$details[["pctCorrect"]], 2), "testing")
}

ggplot(dfSize, aes(x = minNumObj)) +
  geom_line(aes(y = leavesNumber, color = "Cantidad de Hojas")) +
  geom_point(aes(y = leavesNumber, color = "Cantidad de Hojas")) +
  geom_line(aes(y = treeSize, color = "Tamaño de Arbol")) +
  geom_point(aes(y = treeSize, color = "Tamaño de Arbol")) +
  xlab("minNumObj") +
  ylab(NULL) +
  theme_bw()

ggplot(dfPerformance, aes_string(x = "minNumObj", y = "accuracy", colour = "set", group = "set")) +
  geom_line() +
  geom_point() +
  xlab("minNumObj") +
  ylab("Accuracy (%)") +
  theme_bw()
