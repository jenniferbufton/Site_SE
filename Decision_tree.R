library(rpart)
library(rpart.plot)
library(RWeka) #for m5p funtion. may need java installation 
library(caret)
library(dplyr)
library(readxl)

setwd("C:/Users/jenniferb/OneDrive - Sport England/GitHub/Small Grants")
me_df <- read_xlsx('MandE.xlsx', na = "NA")

data <- me_df%>%
  mutate(Over_Target = `12 MONTH PARTICIPANTS` > `YEAR 1 PARTICIPANTS TARGET`)%>%
  select(AWARD, Total_Project_Cost, Partnership_Funding, GOVERNMENT_OFFICE_REGION, Focus, Sport, Over_Target,
         IMD_RANK,  ALLOCATED_TO) #`12 MONTH PARTICIPANTS`,

data <- as.data.frame(data)
data$IMD_RANK <- as.numeric(data$IMD_RANK)
data$GOVERNMENT_OFFICE_REGION <- as.factor(data$GOVERNMENT_OFFICE_REGION)
data$Focus <- as.factor(data$Focus)
data$ALLOCATED_TO <- as.factor(data$ALLOCATED_TO)
data$Sport <- as.factor(data$Sport)
data$Over_Target <- as.factor(data$Over_Target)
str(data)

##Partition
set.seed(101)
inTrain <- createDataPartition(y=data$Over_Target, p=1, list=FALSE) 
Train <- data[inTrain,]
Test <- data[-inTrain,]

tree <- rpart(`Over_Target` ~., data=Train, maxdepth = 4)
rpart.plot(tree)
tree

m <- C5.0(Train[,-7], Train[,7]) #training dataset with predictors (arg 1), and the predictor (arg 2)
m
plot(m) #shows how it decided.

pred <- predict(tree, Test)
str(pred)
summary(pred)
summary(Test$prediction)
cor(pred[[1]], Test$prediction)


### KNN

colnames(me_df)
data <- me_df%>%
  mutate("Over_Target" = `12 MONTH PARTICIPANTS` > `YEAR 1 PARTICIPANTS TARGET`)%>%
  select(AWARD, Total_Project_Cost, Partnership_Funding, IMD_RANK, `PARTICIPANTS BASELINE`, `YEAR 1 PARTICIPANTS TARGET`,
         `FEMALE TARGET PERCENTAGE`, `MALE TARGET PERCENTAGE`, Over_Target)
  

data$IMD_RANK <-  as.numeric(data$IMD_RANK)

str(data)

d <- lapply(data[,-9], scale)
d <- as.data.frame(d)
d$Over_Target <- data$Over_Target  

d <- na.omit(d)


#sampling method 2, using data partition function from caret package. It attempts to retain similar distributions of species across the train and test datasets. Times argument is number of partitions to create , List defines how you want the multiple partitions stored (list or matrix) 
# more info on this method here https://topepo.github.io/caret/data-splitting.html
train_index <- createDataPartition(d$Over_Target, p = 0.8, times=1, list = FALSE) # from caret package. !st argument is dependent varaible, p = split, times = number it runs, list = false if 1 tmes

train_d <- d[train_index,]
test_d <- d[-train_index,]

# testing distribution
table((d$Over_Target))
prop.table(table(d$Over_Target))
prop.table(table(train_d$Over_Target))
prop.table(table(test_d$Over_Target))

#knn
train_d_no_class <- train_d[,-9] # no target variable (Over_Target in this case)
test_d_no_class <- test_d[,-9] # no target variable (Over_Target in this case)
train_d_labels <- train_d[,9] # Only target variable (Over_Target in this case)


# 8 predictors in this model
d_test_pred <- knn(train = train_d_no_class, test = test_d_no_class,
                   cl = train_d_labels, k = 27)

sqrt(nrow(d)) # find square root of nrows 

#evaluation
test_d$pred <- d_test_pred # add new prediction column

test_d$correct <- 'n'
test_d$correct[test_d$Over_Target == test_d$pred] <- 'y' # Add another column which shows if correct

table(test_d$correct) # 29/30 correct
prop.table(table(test_d$correct)) #97% correct


CrossTable(x = test_d$Over_Target, y = d_test_pred,
           prop.chisq=FALSE) # shows false positives / negatives - data-dependent on whether false positives / negatives are important e.g. cancer screening

# looks good! Could test different k values and normalising to see if improvements could be made. Could also cross validate by running again with a different random sample of train and test 

