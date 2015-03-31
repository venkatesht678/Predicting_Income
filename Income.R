train <- read.csv("train.csv")
test <- read.csv("test.csv")
train$Open.Date <- as.Date(train$Open.Date , format = "%m/%d/%Y")
str(train$Open.Date)
summary(train$Open.Date)
test$Open.Date <- as.Date(test$Open.Date , format = "%m/%d/%Y")
summary(test$Open.Date)

train <- train[c(-1,-2)]
test <- test[c(-1,-2)]
write.csv(train ,"train1.csv" , row.names = FALSE)
write.csv(test ,"test1.csv" , row.names = FALSE)

train1 <- read.csv("train1.csv")
test1 <- read.csv("test1.csv")
levels(test1$Type)[levels(test1$Type)=="MB"] <- "IL"

library(caret)
library(psych)

str(train1)
pairs.panels(train1[c(10:20)])
pairs.panels(train1[c(20:30)])
pairs.panels(train1[c(20:40)])
pairs.panels(train1[c(30:40)])

model <- lm(revenue ~ P1+P2+P4+P6+P7+P11+P17+P19+P21+P22+P23+P24+P25+P28+P30 ,data = train1)
find <- predict(model , test1)
summary(find)