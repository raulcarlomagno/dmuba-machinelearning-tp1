#install.packages("RWeka")
library("RWeka")
library("ggplot2")
library("partykit")
library("rlist")

trainData <- read.csv("ks-projects-processed-train.csv")
testData <- read.csv("ks-projects-processed-test.csv")

#WOW("J48")

doTraining <- function(j48ParamName, nombreParametro, valoresParametro){
  
  j48Params <- setNames(list(1), j48ParamName)
    
  dfPerformance <- data.frame(valorParam = double(), accuracyTraining = double(), accuracyTesting = double())
  dfSize <- data.frame(valorParam = double(), leavesNumber = integer(), treeSize = integer())

  for(valorParametro in valoresParametro) {
    print(sprintf("entrenando arbol con %s (%s) = %.2f", nombreParametro, j48ParamName, valorParametro))
  
    j48Params[1] <- valorParametro
    J48Options <- do.call(Weka_control, as.list(j48Params))
    
    model <- J48(funded ~ ., data = trainData, control = J48Options)
    partyModel <- as.party(model)
    dfSize[nrow(dfSize) + 1, ] <- c(valorParametro, width(partyModel), length(partyModel))
    
    crossValEval <- evaluate_Weka_classifier(model, numFolds = 10, class = TRUE)
    testEval <- evaluate_Weka_classifier(model, newdata = testData, class = TRUE)
    
    print(sprintf("---accuracy training con %s (%s=%.2f) : %.2f%%", nombreParametro, j48ParamName, valorParametro, crossValEval$details[["pctCorrect"]]))
    print(sprintf("---accuracy testing con %s (%s=%.2f): %.2f%%", nombreParametro, j48ParamName, valorParametro, testEval$details[["pctCorrect"]]))
    
    dfPerformance[nrow(dfPerformance) + 1, ] <- c(valorParametro, round(crossValEval$details[["pctCorrect"]], 2), round(testEval$details[["pctCorrect"]], 2))
  }
  
  graficos <- list()
  
  graficos <- list(ggplot(dfSize, aes_string(x = "valorParam")) +
    geom_line(aes(y = leavesNumber, color = "Cantidad de Hojas")) +
    geom_line(aes(y = treeSize, color = "Tamaño de Arbol")) +
    scale_color_hue("Métrica") +
    xlab(nombreParametro) +
    ylab(NULL) +
    theme_bw())
  
  
  #dfPerformance$accuracy <- as.double(dfPerformance$accuracy) #lo reconvierto a numerico
  #dfPerformance$valorParam <- as.double(dfPerformance$valorParam) #lo reconvierto a numerico
  
  graficos <- list(graficos, ggplot(dfPerformance, aes_string(x = "valorParam")) +
    geom_line(aes(y = accuracyTraining, color = "Training")) +
    geom_line(aes(y = accuracyTesting, color = "Testing")) +
    scale_color_hue("Set") +
    xlab(nombreParametro) +
    ylab("Accuracy (%)") +
    theme_bw())
  
  return(graficos)
}

#confidenceFactor -- The confidence factor used for pruning (smaller values incur more pruning).
resultado <- doTraining("C", "confidenceFactor", seq(0.05, 0.5, 0.05))
resultado[[1]]
resultado[[2]]


minNumObjStart <- round(0.005 * nrow(trainData))
minNumObjEnd <- round(0.10 * nrow(trainData))
minNumObjStep <- round(0.005 * nrow(trainData))

resultado <- doTraining("M", "minNumObj", seq(minNumObjStart, minNumObjEnd, minNumObjStep))
resultado[[1]]
resultado[[2]]
