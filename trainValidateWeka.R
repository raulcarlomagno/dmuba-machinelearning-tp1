#install.packages("RWeka")
library("RWeka")
library("ggplot2")
library("partykit")
library("rlist")

trainDf <- read.csv("ks-projects-processed-train.csv")
testDf <- read.csv("ks-projects-processed-test.csv")

#WOW("J48")

doTraining <- function(trainData, testData, j48ParamName, nombreParametro, valoresParametro){
  
  j48Params <- setNames(list(1), j48ParamName)
    
  dfPerformance <- data.frame(valorParam = double(), accuracyTraining = double(), accuracyTesting = double())
  dfSize <- data.frame(valorParam = double(), leaves = integer(), nodes = integer())

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
  
  
  resultado <- list(ggplot(dfSize, aes_string(x = "valorParam")) +
    geom_line(aes(y = leaves, color = "Hojas")) +
    geom_line(aes(y = nodes, color = "Nodos")) +
    scale_color_hue("Métrica") +
    xlab(nombreParametro) +
    ylab("Cantidad") +
    theme_bw())
  
  resultado <- list.append(resultado, ggplot(dfPerformance, aes_string(x = "valorParam")) +
    geom_line(aes(y = accuracyTraining, color = "Training")) +
    geom_line(aes(y = accuracyTesting, color = "Testing")) +
    scale_color_hue("Set") +
    xlab(nombreParametro) +
    ylab("Accuracy (%)") +
    theme_bw())
  
  resultado <- list.append(resultado, dfSize, dfPerformance)
  
  return(resultado)
}

 #confidenceFactor -- The confidence factor used for pruning (smaller values incur more pruning).
 resultado <- doTraining(trainDf, testDf, "C", "confidenceFactor", seq(0.05, 0.5, 0.05))
 resultado[[1]]
 resultado[[2]]

  
 png("1.png", height = 666 , width=1230, units = "px", res = 300, type = "cairo")
 print(resultado[[1]])
 dev.off()
 
 png("2.png", height = 666 , width=1230, units = "px", res = 300, type = "cairo")
 print(resultado[[2]])
 dev.off()

 
minNumObjStart <- round(0.005 * nrow(trainDf))
minNumObjEnd <- round(0.10 * nrow(trainDf))
minNumObjStep <- round(0.005 * nrow(trainDf))

resultado <- doTraining(trainDf, testDf, "M", "minNumObj", seq(minNumObjStart, minNumObjEnd, minNumObjStep))
resultado[[1]]
resultado[[2]]


png("3.png", height = 666 , width=1230, units = "px", res = 300, type = "cairo")
print(resultado[[1]])
dev.off()

png("4.png", height = 666 , width=1230, units = "px", res = 300, type = "cairo")
print(resultado[[2]])
dev.off()

dfPerformanceFaltantes <- data.frame(valorParam = double(), accuracyTraining = double(), accuracyTesting = double(), porcentajeFaltantes = integer())
dfSizeFaltantes <- data.frame(valorParam = double(), leaves = integer(), nodes = integer(), porcentajeFaltantes = integer())

colName <- "month_launched" #nombre de columna para simular faltantes
colIndex <- match(colName, colnames(trainDf))
colModa <- names(tail(sort(table(trainDf[, colIndex])), 1))

#faltantesPorcent <- seq(0, 0.75, 0.05)
faltantesPorcent <- seq(0, 0.75, 0.05)
for(faltantePorcent in faltantesPorcent){
  print(sprintf("generando  %.2f%% faltantes", faltantePorcent * 100))
  
  if(faltantePorcent == 0){
    trainCopy <- trainDf
  } else {
    set.seed(unclass(Sys.time()))
    idxColFaltantes <- sample(nrow(trainDf), round(faltantePorcent * nrow(trainDf)))
    trainCopy <- trainDf
    trainCopy[idxColFaltantes, colIndex] <- colModa
  }
  
  #doTraining(trainCopy, testData, "C", "confidenceFactor", seq(0.05, 0.5, 0.05))
  resultado <- doTraining(trainCopy, testDf, "C", "confidenceFactor", seq(0.05, 0.5, 0.05))
  #resultado[[4]] #performance
  #resultado[[3]] #size

  dfPerformanceFaltantes <- rbind(dfPerformanceFaltantes, transform(resultado[[4]], porcentajeFaltantes = faltantePorcent * 100))
  dfSizeFaltantes <- rbind(dfSizeFaltantes, transform(resultado[[3]], porcentajeFaltantes = faltantePorcent * 100))

}

dfSizeFaltantes$porcentajeFaltantes <- as.factor(dfSizeFaltantes$porcentajeFaltantes)
dfPerformanceFaltantes$porcentajeFaltantes <- as.factor(dfPerformanceFaltantes$porcentajeFaltantes)

plot1 <- ggplot(dfSizeFaltantes, aes(x = valorParam, y = leaves, color = porcentajeFaltantes)) +
  geom_line() +
  scale_color_hue("% Faltantes") +
  xlab("confidenceFactor") +
  ylab("Hojas") +
  theme_bw()

png("5.png", height = 666 , width=1230, units = "px", res = 150, type = "cairo")
print(plot1)
dev.off()


plot2 <- ggplot(dfSizeFaltantes, aes(x = valorParam, y = nodes, color = porcentajeFaltantes)) +
  geom_line() +
  scale_color_hue("% Faltantes") +
  xlab("confidenceFactor") +
  ylab("Nodos") +
  theme_bw()

png("6.png", height = 666 , width=1230, units = "px", res = 150, type = "cairo")
print(plot2)
dev.off()

plot3 <- ggplot(dfPerformanceFaltantes, aes(x = valorParam, y = accuracyTraining, color = porcentajeFaltantes)) +
  geom_line() +
  scale_color_hue("% Faltantes") +
  xlab("confidenceFactor") +
  ylab("Accuracy (%)") +
  theme_bw()

png("7.png", height = 694 , width=1280, units = "px", res = 150, type = "cairo")
print(plot3)
dev.off()