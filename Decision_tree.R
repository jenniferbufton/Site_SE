library(rpart)
library(rpart.plot)
library(RWeka) #for m5p funtion. may need java installation 
library(caret)
library(dplyr)
library(readxl)

setwd("C:/Users/jenniferb/OneDrive - Sport England/GitHub/Small Grants")
me_df <- read_xlsx('MandE.xlsx', na = "NA")

data <- me_df%>%
  select(AWARD, Total_Project_Cost, Partnership_Funding, GOVERNMENT_OFFICE_REGION, Focus, Sport,
         IMD_DECILE, `12 MONTH PARTICIPANTS`)

data <- as.data.frame(data)

inTrain <- createDataPartition(y=data$`12 MONTH PARTICIPANTS`, p=0.75, list=FALSE) 
Train <- data[inTrain,]
Test <- data[-inTrain,]

tree <- rpart(`12 MONTH PARTICIPANTS` ~., data=Train, maxdepth = 2)
rpart.plot(tree)
tree

pred <- predict(tree, Test)
str(pred)
summary(pred)
summary(Test$prediction)
cor(pred[[1]], Test$prediction)
