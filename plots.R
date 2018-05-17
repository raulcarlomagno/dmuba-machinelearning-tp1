classColumnName <- "funded"
source("trainValidateWeka.R")

trainDf <- read.csv("ks-projects-processed-train.csv")
testDf <- read.csv("ks-projects-processed-test.csv")

# PUNTO 3 ########################################################################################## PUNT0 3
#confidenceFactor -- The confidence factor used for pruning (smaller values incur more pruning).
resultado3 <- doTraining(trainDf, testDf, "C", "confidenceFactor", seq(0.05, 0.5, 0.05))
savePlot(resultado3[[1]], "Sobreajuste y poda (confidenceFactor)", "Tamaño arbol", "3 - tamaño.png")
savePlot(resultado3[[2]], "Sobreajuste y poda (confidenceFactor)", "Performance arbol", "3 - perfomance.png")

# PUNTO 4 ########################################################################################## PUNT0 4
minNumObjStart <- round(0.005 * nrow(trainDf))
minNumObjEnd <- round(0.10 * nrow(trainDf))
minNumObjStep <- round(0.005 * nrow(trainDf))

resultado4 <- doTraining(trainDf, testDf, "M", "minNumObj", seq(minNumObjStart, minNumObjEnd, minNumObjStep))
savePlot(resultado4[[1]], "Sobreajuste y poda (minNumObj)", "Tamaño arbol", "4 - tamaño.png")
savePlot(resultado4[[2]], "Sobreajuste y poda (minNumObj)", "Performance arbol", "4 - perfomance.png")

# PUNTO 5 ########################################################################################## PUNT0 5
faltantesModaTrain <- trainFaltantes(seq(0, 75, 5), "month_launched", classColumnName, "yes")
savePlot(faltantesModaTrain[[3]], "Tratamiento de datos faltantes", "Moda - Hojas arbol", "5 - moda - hojas.png")
savePlot(faltantesModaTrain[[4]], "Tratamiento de datos faltantes", "Moda - Nodos arbol", "5 - moda - nodos.png")
savePlot(faltantesModaTrain[[5]], "Tratamiento de datos faltantes", "Moda - Performance arbol", "5 - moda - performance.png")

faltantesModaClaseTrain <- trainFaltantes(seq(0, 75, 5), "month_launched", classColumnName, "yes", FALSE)
savePlot(faltantesModaClaseTrain[[3]], "Tratamiento de datos faltantes", "Modaclase - Hojas arbol", "5 - modaclase - hojas.png")
savePlot(faltantesModaClaseTrain[[4]], "Tratamiento de datos faltantes", "Modaclase - Nodos arbol", "5 - modaclase - nodos.png")
savePlot(faltantesModaClaseTrain[[5]], "Tratamiento de datos faltantes", "Modaclase - Performance arbol", "5 - modaclase - performance.png")


# PUNTO 6 ########################################################################################## PUNT0 6
toleranciaRuidoTrain <- toleranciaRuido(0:35, classColumnName, "no")
savePlot(toleranciaRuidoTrain[[3]], "Tolerancia al ruido", "Hojas arbol", "6 - hojas.png", 125)
savePlot(toleranciaRuidoTrain[[4]], "Tolerancia al ruido", "Nodos arbol", "6 - nodos.png", 125)
savePlot(toleranciaRuidoTrain[[5]], "Tolerancia al ruido", "Performance arbol", "6 - performance.png", 125)


# PUNTO 7 ########################################################################################## PUNT0 7
discretizadoIgualAncho <- discretizar(1:20, c("usd_goal", "days_funding"))
savePlot(discretizadoIgualAncho[[3]], "Discretización de atributos numéricos", "Igual_ancho - Hojas arbol", "7 - igual_ancho - hojas.png", 125)
savePlot(discretizadoIgualAncho[[4]], "Discretización de atributos numéricos", "Igual_ancho - Nodos arbol", "7 - igual_ancho - nodos.png", 125)
savePlot(discretizadoIgualAncho[[5]], "Discretización de atributos numéricos", "Igual_ancho - Performance arbol", "7 - igual_ancho - performance testing.png", 125)

discretizadoIgualCantidad <- discretizar(1:20, c("usd_goal", "days_funding"), FALSE)
savePlot(discretizadoIgualCantidad[[3]], "Discretización de atributos numéricos", "Igual_cantidad - Hojas arbol", "7 - igual_cantidad - hojas.png", 125)
savePlot(discretizadoIgualCantidad[[4]], "Discretización de atributos numéricos", "Igual_cantidad - Nodos arbol", "7 - igual_cantidad - nodos.png", 125)
savePlot(discretizadoIgualCantidad[[5]], "Discretización de atributos numéricos", "Igual_cantidad - Performance arbol", "7 - igual_cantidad - performance testing.png", 125)
