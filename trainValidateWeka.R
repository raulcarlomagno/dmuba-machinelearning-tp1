#install.packages("RWeka")
library("RWeka")
library("ggplot2")
library("partykit")

trainData <- read.csv("ks-projects-processed-train.csv")
testData <- read.csv("ks-projects-processed-test.csv")

#WOW("J48")

doTraining <- function(nombreParametro, valoresParametro){

  #nombreParametro <- "confidenceFactor"
  #valoresParametro <- seq(0.05, 0.5, 0.05)
  
  dfPerformance <- data.frame(valorParam = double(), accuracy = double(), set = character(), stringsAsFactors = FALSE)
  dfSize <- data.frame(valorParam = double(), leavesNumber = integer(), treeSize = integer())
  
  #confidenceFactor -- The confidence factor used for pruning (smaller values incur more pruning).
  for(valorParametro in valoresParametro) {
    print(paste("ENTRENANDO ARBOL CON", nombreParametro, valorParametro))
    J48Options <- Weka_control(C = valorParametro)
    model <- J48(funded ~ ., data = trainData, control = J48Options)
    partyModel <- as.party(model)
    dfSize[nrow(dfSize) + 1, ] <- c(valorParametro, width(partyModel), length(partyModel))
    
    crossValEval <- evaluate_Weka_classifier(model, numFolds = 10, class = TRUE)
    testEval <- evaluate_Weka_classifier(model, newdata = testData, class = TRUE)
    
    print(sprintf("TRAINING - PORCENTAJE CORRECTOS - %.2f", crossValEval$details[["pctCorrect"]]))
    print(sprintf("TESTING - PORCENTAJE CORRECTOS - %.2f", testEval$details[["pctCorrect"]]))
    
    dfPerformance[nrow(dfPerformance) + 1, ] <- c(valorParametro, round(crossValEval$details[["pctCorrect"]], 2), "training")
    dfPerformance[nrow(dfPerformance) + 1, ] <- c(valorParametro, round(testEval$details[["pctCorrect"]], 2), "testing")
  }
  
  ggplot(dfSize, aes_string(x = "valorParam")) +
    geom_line(aes(y = leavesNumber, color = "Cantidad de Hojas")) +
    geom_point(aes(y = leavesNumber, color = "Cantidad de Hojas")) +
    geom_line(aes(y = treeSize, color = "Tamaño de Arbol")) +
    geom_point(aes(y = treeSize, color = "Tamaño de Arbol")) +
    xlab(nombreParametro) +
    ylab(NULL) +
    theme_bw()
  
  ggplot(dfPerformance, aes_string(x = "valorParam", y = "accuracy", colour = "set", group = "set")) +
    geom_line() +
    geom_point() +
    xlab(nombreParametro) +
    ylab("Accuracy (%)") +
    theme_bw()
}

doTraining("confidenceFactor", seq(0.05, 0.5, 0.05))



