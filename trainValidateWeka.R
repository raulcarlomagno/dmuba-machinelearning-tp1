#install.packages("RWeka")
library("RWeka")

trainData <- read.csv("ks-projects-processed.csv")

#WOW("J48")

J48Options <- Weka_control(R = TRUE, M = 5)
m <- J48(funded ~ ., data = trainData, control = J48Options)
#summary(m) #calls evaluate_Weka_classifier()
table(trainData$funded, predict(m)) 
#pag 20


crossValEval <- evaluate_Weka_classifier(m,
                              #cost = matrix(c(0,1,1,0), ncol = 2),
                              numFolds = 10, class = TRUE)

crossValEval
summary(crossValEval)
e$details[["pctCorrect"]]


