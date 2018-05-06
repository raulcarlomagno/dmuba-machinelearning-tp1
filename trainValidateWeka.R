#install.packages("RWeka")
library("RWeka")
library("ggplot2")
library("partykit")

trainData <- read.csv("ks-projects-processed-train.csv")
testData <- read.csv("ks-projects-processed-test.csv")

#WOW("J48")

dfPerformance <- data.frame(cf = double(), accuracy = double(), set = character(), stringsAsFactors = FALSE)
dfSize <- data.frame(cf = double(), leavesNumber = integer(), treeSize = integer())

#confidenceFactor -- The confidence factor used for pruning (smaller values incur more pruning).
for(cf in seq(0.05, 0.5, 0.05)) {
  print(paste("ENTRENANDO ARBOL CON CF =", cf))
  J48Options <- Weka_control(C = cf)
  model <- J48(funded ~ ., data = trainData, control = J48Options)
  partyModel <- as.party(model)
  dfSize[nrow(dfSize) + 1, ] <- c(cf, width(partyModel), length(partyModel))
    
  crossValEval <- evaluate_Weka_classifier(model, numFolds = 10, class = TRUE)
  testEval <- evaluate_Weka_classifier(model, newdata = testData, class = TRUE)
  
  print(sprintf("TRAINING - PORCENTAJE CORRECTOS - %.2f", crossValEval$details[["pctCorrect"]]))
  print(sprintf("TESTING - PORCENTAJE CORRECTOS - %.2f", testEval$details[["pctCorrect"]]))
  
  dfPerformance[nrow(dfPerformance) + 1, ] <- c(cf, round(crossValEval$details[["pctCorrect"]], 2), "training")
  dfPerformance[nrow(dfPerformance) + 1, ] <- c(cf, round(testEval$details[["pctCorrect"]], 2), "testing")
}

ggplot(dfPerformance, aes_string(x = "cf", y = "accuracy", colour = "set", group = "set")) +
  geom_line() +
  geom_point() +
  xlab("Confidence Factor (CF)") +
  ylab("Accuracy (%)") +
  theme_bw()
