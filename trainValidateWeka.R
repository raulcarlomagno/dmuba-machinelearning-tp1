#install.packages("RWeka")
library("RWeka")
library("ggplot2")
library("partykit")
library("rlist")

trainDf <- read.csv("ks-projects-processed-train.csv")
testDf <- read.csv("ks-projects-processed-test.csv")

savePlot <- function(plot, titulo, fileName){
  plot <- plot + ggtitle(titulo)
  print(sprintf("saving plot to %s", fileName))
  png(fileName, height = 666 , width=1230, units = "px", res = 150, type = "cairo")
  print(plot)
  dev.off()
}

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

trainFaltantes <- function(colFaltante, estrategiaModa = TRUE){
  dfPerformanceFaltantes <- data.frame(valorParam = double(), accuracyTraining = double(), accuracyTesting = double(), porcentajeFaltantes = integer())
  dfSizeFaltantes <- data.frame(valorParam = double(), leaves = integer(), nodes = integer(), porcentajeFaltantes = integer())
  
  colName <- colFaltante #nombre de columna para simular faltantes
  colIndex <- match(colName, colnames(trainDf))
  if(estrategiaModa){
    colModa <- names(tail(sort(table(trainDf[, colIndex])), 1))
  } else {
    colModaFunded <- names(tail(sort(table(trainDf[trainDf$funded == "yes", colIndex])), 1))
    colModaNotFunded <- names(tail(sort(table(trainDf[trainDf$funded == "no", colIndex])), 1))
  }
  
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
      if(estrategiaModa){
        trainCopy[idxColFaltantes, colIndex] <- colModa
      } else {
        trainCopy[idxColFaltantes, colIndex] <- ifelse(trainCopy[idxColFaltantes, ]$funded == "yes", colModaFunded, colModaNotFunded)
      }
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
  
  resultado <- list(dfSizeFaltantes, dfPerformanceFaltantes)
  
  plot1 <- ggplot(dfSizeFaltantes, aes(x = valorParam, y = leaves, color = porcentajeFaltantes)) +
    geom_line() +
    scale_color_hue("% Faltantes") +
    xlab("confidenceFactor") +
    ylab("Hojas") +
    theme_bw()
  
  plot2 <- ggplot(dfSizeFaltantes, aes(x = valorParam, y = nodes, color = porcentajeFaltantes)) +
    geom_line() +
    scale_color_hue("% Faltantes") +
    xlab("confidenceFactor") +
    ylab("Nodos") +
    theme_bw()
  
  plot3 <- ggplot(dfPerformanceFaltantes, aes(x = valorParam, y = accuracyTraining, color = porcentajeFaltantes)) +
    geom_line() +
    scale_color_hue("% Faltantes") +
    xlab("confidenceFactor") +
    ylab("Accuracy (%)") +
    theme_bw()
  
  resultado <- list.append(resultado, plot1, plot2, plot3)
  
  return(resultado)
}


toleranciaRuido <- function(colClase, valorClase){
  dfPerformanceRuido <- data.frame(valorParam = double(), accuracyTraining = double(), accuracyTesting = double(), porcentajeRuido = integer())
  dfSizeRuido <- data.frame(valorParam = double(), leaves = integer(), nodes = integer(), porcentajeRuido = integer())
  
  colIndex <- match(colClase, colnames(trainDf))
  colModa <- names(tail(sort(table(trainDf[, colIndex])), 1))
  
  ruidosPorcent <- seq(0, 0.35, 0.01)
  for(ruidoPorcent in ruidosPorcent){
    print(sprintf("generando  %.2f%% de ruido", ruidoPorcent * 100))
    
    if(ruidoPorcent == 0){
      trainCopy <- trainDf
    } else {
      set.seed(unclass(Sys.time()))
      idxRowRuido <- sample(nrow(trainDf), round(ruidoPorcent * nrow(trainDf)))
      trainCopy <- trainDf
      trainCopy[idxRowRuido, colIndex] <- valorClase
    }
    
    resultado <- doTraining(trainCopy, testDf, "C", "confidenceFactor", seq(0.05, 0.5, 0.05))

    dfPerformanceRuido <- rbind(dfPerformanceRuido, transform(resultado[[4]], porcentajeRuido = ruidoPorcent * 100))
    dfSizeRuido <- rbind(dfSizeRuido, transform(resultado[[3]], porcentajeRuido = ruidoPorcent * 100))
    
  }
  
  dfSizeRuido$porcentajeRuido <- as.factor(dfSizeRuido$porcentajeRuido)
  dfPerformanceRuido$porcentajeRuido <- as.factor(dfPerformanceRuido$porcentajeRuido)
  
  resultado <- list(dfSizeRuido, dfPerformanceRuido)
  
  plot1 <- ggplot(dfSizeRuido, aes(x = valorParam, y = leaves, color = porcentajeRuido)) +
    geom_line() +
    scale_color_hue("% Ruido") +
    xlab("confidenceFactor") +
    ylab("Hojas") +
    theme_bw()
  
  plot2 <- ggplot(dfSizeRuido, aes(x = valorParam, y = nodes, color = porcentajeRuido)) +
    geom_line() +
    scale_color_hue("% Ruido") +
    xlab("confidenceFactor") +
    ylab("Nodos") +
    theme_bw()
  
  plot3 <- ggplot(dfPerformanceRuido, aes(x = valorParam, y = accuracyTraining, color = porcentajeRuido)) +
    geom_line() +
    scale_color_hue("% Ruido") +
    xlab("confidenceFactor") +
    ylab("Accuracy (%)") +
    theme_bw()
  
  resultado <- list.append(resultado, plot1, plot2, plot3)
  
  return(resultado)
}

# PUNTO 3 ########################################################################################## PUNT0 3
#confidenceFactor -- The confidence factor used for pruning (smaller values incur more pruning).
resultado <- doTraining(trainDf, testDf, "C", "confidenceFactor", seq(0.05, 0.5, 0.05))
savePlot(resultado[[1]], "Sobreajuste y poda (CF) - Tamaño arbol", "3 - tamaño.png")
savePlot(resultado[[2]], "Sobreajuste y poda (CF) - Performance arbol", "3 - perfomance.png")

# PUNTO 4 ########################################################################################## PUNT0 4
minNumObjStart <- round(0.005 * nrow(trainDf))
minNumObjEnd <- round(0.10 * nrow(trainDf))
minNumObjStep <- round(0.005 * nrow(trainDf))

resultado <- doTraining(trainDf, testDf, "M", "minNumObj", seq(minNumObjStart, minNumObjEnd, minNumObjStep))
savePlot(resultado[[1]], "Sobreajuste y poda (minNumObj) - Tamaño arbol", "4 - tamaño.png")
savePlot(resultado[[2]], "Sobreajuste y poda (minNumObj) - Performance arbol", "4 - perfomance.png")


# PUNTO 5 ########################################################################################## PUNT0 5
faltantesModaTrain <- trainFaltantes("month_launched")
savePlot(faltantesModaTrain[[3]], "Tratamiento de datos faltantes - Moda - Hojas arbol", "5 - moda - hojas.png")
savePlot(faltantesModaTrain[[4]], "Tratamiento de datos faltantes - Moda - Nodos arbol", "5 - moda - nodos.png")
savePlot(faltantesModaTrain[[5]], "Tratamiento de datos faltantes - Moda - Performance arbol", "5 - moda - performance.png")

faltantesModaClaseTrain <- trainFaltantes("month_launched", FALSE)
savePlot(faltantesModaClaseTrain[[3]], "Tratamiento de datos faltantes - Modaclase - Hojas arbol", "5 - modaclase - hojas.png")
savePlot(faltantesModaClaseTrain[[4]], "Tratamiento de datos faltantes - Modaclase - Nodos arbol", "5 - modaclase - nodos.png")
savePlot(faltantesModaClaseTrain[[5]], "Tratamiento de datos faltantes - Modaclase - Performance arbol", "5 - modaclase - performance.png")


# PUNTO 6 ########################################################################################## PUNT0 6
toleranciaRuidoTrain <- toleranciaRuido("funded", "no")
savePlot(toleranciaRuidoTrain[[3]], "Tolerancia al ruido - Hojas arbol", "6 - hojas.png")
savePlot(toleranciaRuidoTrain[[4]], "Tolerancia al ruido  - Nodos arbol", "6 - nodos.png")
savePlot(toleranciaRuidoTrain[[5]], "Tolerancia al ruido - Performance arbol", "6 - performance.png")

