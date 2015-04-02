train <- read.csv("train.csv")
test <- read.csv("test.csv")
year <- as.POSIXlt(as.Date(train$Open.Date , format = "%m/%d/%Y"))
train$Year <- unclass(year$year)
train$Open.Date <- unclass(as.Date(train$Open.Date , format = "%m/%d/%Y"))
str(train$Open.Date)
summary(train$Open.Date)

year <- as.POSIXlt(as.Date(test$Open.Date , format = "%m/%d/%Y"))
test$Year <- unclass(year$year)
test$Open.Date <- unclass(as.Date(test$Open.Date , format = "%m/%d/%Y"))
summary(test$Open.Date)

train <- train[c(-1)]
test <- test[c(-1)]
write.csv(train ,"train2.csv" , row.names = FALSE)
write.csv(test ,"test2.csv" , row.names = FALSE)

train2 <- read.csv("train2.csv")
test2 <- read.csv("test2.csv")
levels(test2$Type)[levels(test2$Type)=="MB"] <- "IL"
 

library(caret)
library(psych)

str(train1)
pairs.panels(train2[c(10:20)])


tr <- trControl(method = "cv" , number = 10)
grid <- expand.grid(.mtry  = c(2,11,12,13,14,15,16,17))
rf1 <- train(revenue ~ City.Group*Year*P26+P30+P32+P28+P26+P18+P16+P21+P24+P3+P25+P14+P20+P5+P27 , data = train2 , method = "rf" , trControl = tr , tuneGrid = grid)
rf2 <- predict(rf1 , test2)
summary(rf1)
summary(rf2)
write.csv(rf2, "result3.csv" , row.names = FALSE)




#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#high - result3
#2567000 4141000 4538000 4602000 4994000 8358000 
#medium - result3A
#2658000 4150000 4553000 4607000 5003000 7929000 
#low - result3B
#2601000 4137000 4540000 4593000 4980000 7898000 





