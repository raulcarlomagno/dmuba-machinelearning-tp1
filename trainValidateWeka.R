library("RWeka") #install.packages("RWeka")
library("ggplot2") #install.packages("ggplot2")
library("partykit") #install.packages("partykit")
library("rlist") #install.packages("rlist")

savePlot <- function(plot, titulo, fileName, ppi = 150){
  plot <- plot + ggtitle(titulo)
  plot <- plot + theme_light()
  #TODO: setear el theme aca
  print(sprintf("saving plot to %s", fileName))
  png(paste("plots\\", fileName, sep = ""), height = 666 , width=1230, units = "px", res = ppi, type = "cairo")
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
    
    model <- J48(as.formula(paste(classColumnName, "~ .")), data = trainData, control = J48Options)
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
    ylab("Cantidad"))
  
  resultado <- list.append(resultado, ggplot(dfPerformance, aes_string(x = "valorParam")) +
    geom_line(aes(y = accuracyTraining, color = "Training")) +
    geom_line(aes(y = accuracyTesting, color = "Testing")) +
    scale_color_hue("Set") +
    xlab(nombreParametro) +
    ylab("Accuracy (%)"))
  
  resultado <- list.append(resultado, dfSize, dfPerformance)
  
  return(resultado)
}

trainFaltantes <- function(faltantesPorcent, colFaltante, colClase, clasePositiveValue, estrategiaModa = TRUE){
  dfPerformanceFaltantes <- data.frame(valorParam = double(), accuracyTraining = double(), accuracyTesting = double(), porcentajeFaltantes = integer())
  dfSizeFaltantes <- data.frame(valorParam = double(), leaves = integer(), nodes = integer(), porcentajeFaltantes = integer())
  
  colName <- colFaltante #nombre de columna para simular faltantes
  colIndex <- match(colName, colnames(trainDf))
  colClaseIndex <- match(colClase, colnames(trainDf))
  
  if(estrategiaModa){
    colModa <- names(tail(sort(table(trainDf[, colIndex])), 1))
  } else {
    colModaPositive <- names(tail(sort(table(trainDf[trainDf[, colClaseIndex] == clasePositiveValue, colIndex])), 1))
    colModaNegative <- names(tail(sort(table(trainDf[trainDf[, colClaseIndex] != clasePositiveValue, colIndex])), 1))
  }
  
  for(faltantePorcent in faltantesPorcent){
    print(sprintf("generando %.2f%% faltantes", faltantePorcent))
    
    if(faltantePorcent == 0){
      trainCopy <- trainDf
    } else {
      set.seed(unclass(Sys.time()))
      idxColFaltantes <- sample(nrow(trainDf), round(faltantePorcent/100 * nrow(trainDf)))
      trainCopy <- trainDf
      if(estrategiaModa){
        trainCopy[idxColFaltantes, colIndex] <- colModa
      } else {
        trainCopy[idxColFaltantes, colIndex] <- ifelse(trainCopy[idxColFaltantes, colClaseIndex] == clasePositiveValue, colModaPositive, colModaNegative)
      }
    }
    
    resultado <- doTraining(trainCopy, testDf, "C", "confidenceFactor", seq(0.05, 0.5, 0.05))

    dfPerformanceFaltantes <- rbind(dfPerformanceFaltantes, transform(resultado[[4]], porcentajeFaltantes = faltantePorcent))
    dfSizeFaltantes <- rbind(dfSizeFaltantes, transform(resultado[[3]], porcentajeFaltantes = faltantePorcent))
  }
  
  dfSizeFaltantes$porcentajeFaltantes <- as.factor(dfSizeFaltantes$porcentajeFaltantes)
  dfPerformanceFaltantes$porcentajeFaltantes <- as.factor(dfPerformanceFaltantes$porcentajeFaltantes)
  
  resultado <- list(dfSizeFaltantes, dfPerformanceFaltantes)
  
  plot1 <- ggplot(dfSizeFaltantes, aes(x = valorParam, y = leaves, color = porcentajeFaltantes)) +
    geom_line() +
    scale_color_hue("% Faltantes") +
    xlab("confidenceFactor") +
    ylab("Hojas")
  
  plot2 <- ggplot(dfSizeFaltantes, aes(x = valorParam, y = nodes, color = porcentajeFaltantes)) +
    geom_line() +
    scale_color_hue("% Faltantes") +
    xlab("confidenceFactor") +
    ylab("Nodos")
  
  plot3 <- ggplot(dfPerformanceFaltantes, aes(x = valorParam, y = accuracyTraining, color = porcentajeFaltantes)) +
    geom_line() +
    scale_color_hue("% Faltantes") +
    xlab("confidenceFactor") +
    ylab("Accuracy (%)")
  
  resultado <- list.append(resultado, plot1, plot2, plot3)
  
  return(resultado)
}


toleranciaRuido <- function(ruidosPorcent, colClase, valorClase){
  dfPerformanceRuido <- data.frame(valorParam = double(), accuracyTraining = double(), accuracyTesting = double(), porcentajeRuido = integer())
  dfSizeRuido <- data.frame(valorParam = double(), leaves = integer(), nodes = integer(), porcentajeRuido = integer())
  
  colIndex <- match(colClase, colnames(trainDf))
  colModa <- names(tail(sort(table(trainDf[, colIndex])), 1))
  
  for(ruidoPorcent in ruidosPorcent){
    print(sprintf("generando  %.2f%% de ruido", ruidoPorcent))
    
    if(ruidoPorcent == 0){
      trainCopy <- trainDf
    } else {
      set.seed(unclass(Sys.time()))
      idxRowRuido <- sample(nrow(trainDf), round(ruidoPorcent/100 * nrow(trainDf)))
      trainCopy <- trainDf
      trainCopy[idxRowRuido, colIndex] <- valorClase
    }
    
    resultado <- doTraining(trainCopy, testDf, "C", "confidenceFactor", seq(0.05, 0.5, 0.05))

    dfPerformanceRuido <- rbind(dfPerformanceRuido, transform(resultado[[4]], porcentajeRuido = ruidoPorcent))
    dfSizeRuido <- rbind(dfSizeRuido, transform(resultado[[3]], porcentajeRuido = ruidoPorcent))
  }
  
  dfSizeRuido$porcentajeRuido <- as.factor(dfSizeRuido$porcentajeRuido)
  dfPerformanceRuido$porcentajeRuido <- as.factor(dfPerformanceRuido$porcentajeRuido)
  
  resultado <- list(dfSizeRuido, dfPerformanceRuido)
  
  plot1 <- ggplot(dfSizeRuido, aes(x = valorParam, y = leaves, color = porcentajeRuido)) +
    geom_line() +
    scale_color_hue("% Ruido") +
    xlab("confidenceFactor") +
    ylab("Hojas")
  
  plot2 <- ggplot(dfSizeRuido, aes(x = valorParam, y = nodes, color = porcentajeRuido)) +
    geom_line() +
    scale_color_hue("% Ruido") +
    xlab("confidenceFactor") +
    ylab("Nodos")
  
  plot3 <- ggplot(dfPerformanceRuido, aes(x = valorParam, y = accuracyTraining, color = porcentajeRuido)) +
    geom_line() +
    scale_color_hue("% Ruido") +
    xlab("confidenceFactor") +
    ylab("Accuracy (%)")
  
  resultado <- list.append(resultado, plot1, plot2, plot3)
  
  return(resultado)
}


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
    df[, colIdx] <- as.factor(textBinsAssigned)
  } else {
    df <- cbind(df, as.factor(textBinsAssigned))
    colnames(df)[ncol(df)] <- paste(nombreCol, "_discrete", sep = "")
  }
  
  return(df)
}

discretizarIgualCantidad <- function(df, nombreCol, cantBins, replaceCol){
  colIdx <- match(nombreCol, colnames(df))
  valores <- df[, colIdx]
  unicos = sort(unique(valores))
  cantUnicos = length(unicos)
  cantPorBin = ceiling(cantUnicos / cantBins)

  bins = c()
  cont = 0
  for(nro in unicos){
    if(cont == cantPorBin | cont == 0){
      cont = 0
      bins = c(bins, nro)  
    }
    cont = cont + 1 
  }
  
  textBins <- bins2Text(bins)
  binsAssigned <- binAsign(bins, valores)
  textBinsAssigned <- textBins[match(binsAssigned, bins)]
  
  if(replaceCol) {
    df[, colIdx] <- as.factor(textBinsAssigned)
  } else {
    df <- cbind(df, as.factor(textBinsAssigned))
    colnames(df)[ncol(df)] <- paste(nombreCol, "_discrete", sep = "")
  }
  
  return(df)
}

discretizar <- function(cantBins, colsDiscretizar, igualAncho = TRUE){
  copyTrainDf <- trainDf
  copyTestDf <- testDf
  
  dfPerformanceDiscretizado <- data.frame(valorParam = double(), accuracyTraining = double(), accuracyTesting = double(), cantBins = integer())
  dfSizeDiscretizado <- data.frame(valorParam = double(), leaves = integer(), nodes = integer(), cantBins = integer())  
  
  for(cantBin in cantBins){
    dfDiscretizadoTrain <- copyTrainDf
    dfDiscretizadoTest <- copyTrainDf
    print(sprintf("discretizando con %s y %i bins", ifelse(igualAncho, "Igual_ancho", "Igual_cantidad"), cantBin))
    for(colDiscretizar in colsDiscretizar){
      print(sprintf("discretizando con %s columna %s con %i bins", ifelse(igualAncho, "Igual_ancho", "Igual_cantidad"), colDiscretizar, cantBin))
      if(igualAncho) {
        dfDiscretizadoTrain <- discretizarIgualAncho(dfDiscretizadoTrain, colDiscretizar, cantBin, TRUE)
        dfDiscretizadoTest <- discretizarIgualAncho(dfDiscretizadoTest, colDiscretizar, cantBin, TRUE)  
      } else {
        dfDiscretizadoTrain <- discretizarIgualCantidad(dfDiscretizadoTrain, colDiscretizar, cantBin, TRUE)
        dfDiscretizadoTest <- discretizarIgualCantidad(dfDiscretizadoTest, colDiscretizar, cantBin, TRUE)  
      }
    }
    resultado <- doTraining(dfDiscretizadoTrain, dfDiscretizadoTest, "C", "confidenceFactor", seq(0.05, 0.5, 0.05))
    
    dfPerformanceDiscretizado <- rbind(dfPerformanceDiscretizado, transform(resultado[[4]], cantBins = cantBin))
    dfSizeDiscretizado <- rbind(dfSizeDiscretizado, transform(resultado[[3]], cantBins = cantBin))
  }

  dfSizeDiscretizado$cantBins <- as.factor(dfSizeDiscretizado$cantBins)
  dfPerformanceDiscretizado$cantBins <- as.factor(dfPerformanceDiscretizado$cantBins)
  
  resultado <- list(dfSizeDiscretizado, dfPerformanceDiscretizado)
  
  plot1 <- ggplot(dfSizeDiscretizado, aes(x = valorParam, y = leaves, color = cantBins)) +
    geom_line() +
    scale_color_hue("Cant. bins") +
    xlab("confidenceFactor") +
    ylab("Hojas")
  
  plot2 <- ggplot(dfSizeDiscretizado, aes(x = valorParam, y = nodes, color = cantBins)) +
    geom_line() +
    scale_color_hue("Cant. bins") +
    xlab("confidenceFactor") +
    ylab("Nodos")
  
  plot3 <- ggplot(dfPerformanceDiscretizado, aes(x = valorParam, y = accuracyTesting, color = cantBins)) +
    geom_line() +
    scale_color_hue("Cant. bins") +
    xlab("confidenceFactor") +
    ylab("Accuracy (%)")
  
  resultado <- list.append(resultado, plot1, plot2, plot3)
  
  return(resultado)
}