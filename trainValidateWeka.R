#install.packages("RWeka")
library("RWeka")

trainData <- read.csv("ks-projects-processed.csv")

WOW("J48")

J48Options <- Weka_control(R = TRUE, M = 5)
m <- J48(funded ~ ., data = trainData, control = J48Options)
summary(m)

e <- evaluate_Weka_classifier(m,
                              cost = matrix(c(0,2,1,0), ncol = 2),
                              numFolds = 10, complexity = TRUE,
                              seed = 123, class = TRUE)

e
summary(e)
e$details


