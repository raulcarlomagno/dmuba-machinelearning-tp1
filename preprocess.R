#install.packages("countrycode")
library("countrycode")

df <- read.csv("ks-projects-201801.csv")
options(scipen = 10) #para que no imprima en notacion cientifica

columnsName2Remove = c("ID", "name", "category", "backers", "pledged", "usd.pledged", "usd_pledged_real")
columnsIdx2Remove = match(columnsName2Remove, colnames(df));

df <- df[,-columnsIdx2Remove]
df <- df[df$country != "N,0\"",] #saco las rows de country N,0"
df$launched <- as.Date(df$launched)
df$deadline <- as.Date(df$deadline)
df <- df[df$launched != "1970-01-01",]
df$days_funding <- as.numeric(df$deadline - df$launched)
df <- df[df$days >= 7 & df$days <= 62,] #me quedo con los proyectos de mas de 7 dias de funding
df$year_launched <- as.factor(format(df$launched, format="%Y"))
Sys.setlocale("LC_TIME", "C") #para el nombre del mes en ingles
df$month_launched <- as.factor(format(df$launched, format="%B"))
df <- df[, -which(colnames(df) %in% c("launched","deadline"))]
df <- df[df$state %in% c("successful","failed"),]
df$funded <- as.factor(ifelse(df$state == "successful", "yes", "no"))
df <- df[, -which(colnames(df) == "state")]


currencies <- c("AUD", "CAD", "CHF", "DKK", "EUR", "GBP", "HKD", "JPY", "MXN", "NOK", "NZD", "SEK", "SGD", "USD")
usd_values <- c(0.76038, 0.77924, 1.02181, 0.16403, 1.22191, 1.39564, 0.12746, 0.00919, 0.05298, 0.12665, 0.71193, 0.11749, 0.75569, 1)
dfCurrencies <- data.frame(currency = currencies, usd_value = usd_values)
df$usd_goal <- round((df$goal * dfCurrencies[match(df$currency, dfCurrencies$currency), 2]), 0)
df$goal <- NULL #remove column
df$usd_goal_real <- NULL #remove column
df$currency <- NULL
df <- df[df$usd_goal >= 1000 & df$usd_goal <= 50000, ] #nos quedamos con los proyectos entre 1000 y 100000 de objetivo a recaudar
df <- df[, c(setdiff(names(df), c("funded")), c("funded"))] #columna funded al final
df$country <- countrycode(df$country, "iso2c", "country.name")
cantNoUSA <- nrow(df[df$country != "United States",])
#cantUSA <- nrow(df) - cantNoUSA
#cantTotal <- nrow(df)
dfUsa <- df[df$country == "United States", ]
dfUsa <- dfUsa[sample(1:nrow(dfUsa), cantNoUSA), ] 
df <- df[df$country != "United States", ]
df <- rbind(df, dfUsa)
df <- df[sample(1:nrow(df)), ]
df$year_launched <- as.character(df$year_launched)
df <- droplevels(df) #refresh factors

set.seed(unclass(Sys.time()))
sampledIds <- sample(1:nrow(df), 6000)
cutoff <- round(0.8 * length(sampledIds))
sampledIdsTrain <- sampledIds[1:cutoff]
sampledIdsTest <- setdiff(sampledIds, sampledIdsTrain)

dfSampledTrain <- df[sampledIdsTrain, ]
dfSampledTest <- df[sampledIdsTest, ]

write.csv(dfSampledTrain, "ks-projects-processed-train.csv", row.names=FALSE)
write.csv(dfSampledTest, "ks-projects-processed-test.csv", row.names=FALSE)


# goal <- df$usd_goal
# goal.iqr <- IQR(goal)
# goal.q3 = quantile(goal, 0.75)
# goal.q1 = quantile(goal, 0.25)
# df[df$usd_goal > goal.q3 + goal.iqr * 3, ]
# df[df$usd_goal < goal.q1 - goal.iqr * 1.5, ]
# 
# 
# View(df[df$currency != "USD",])
# 
# str(dfCurrencies)
# str(df)
# head(df)
# summary(df)
# hist(df$usd_goal)
# hist(df$days)
# plot(df$month_launched)
# plot(df$country)
# pie(table(df$main_category));
# boxplot(usd_goal ~ country, data = df)
