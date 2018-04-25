df <- read.csv("ks-projects-201801.csv")
str(df)
summary(df)

columnsName2Remove = c("ID", "name", "category", "backers", "pledged", "usd.pledged", "usd_pledged_real")
columnsIdx2Remove = match(columnsName2Remove, colnames(df));

df <- df[,-columnsIdx2Remove]
df <- df[df$country != "N,0\"",] #saco las rows de country N,0"
df$launched <- as.Date(df$launched)
df$deadline <- as.Date(df$deadline)
df <- df[df$launched != "1970-01-01",]
df$days_funding <- as.numeric(df$deadline - df$launched)
df <- df[df$days >= 7,] #me quedo con los proyectos de mas de 7 dias de funding
df$year_launched <- as.factor(format(df$launched, format="%Y"))
Sys.setlocale("LC_TIME", "C") #para el nombre del mes en ingles
df$month_launched <- as.factor(format(df$launched, format="%B"))
df <- df[, -which(colnames(df) %in% c("launched","deadline"))]
df <- df[df$state %in% c("successful","failed"),]
df$funded <- as.logical(ifelse(df$state == "successful", 1, 0))
df <- df[, -which(colnames(df) == "state")]

df <- droplevels(df) #refresh factors

write.csv(df, "ks-projects-processed.csv")


summary(df$currency)

hist(df$launched,breaks = "month")
hist(df$days)
plot(df$main_category)

boxplot(df$usd_goal_real)

