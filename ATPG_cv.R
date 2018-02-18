library(randomForest)
library(caret)
library(rfUtilities)

#loading data
my.data <- read.table("ATPG1.csv", header = TRUE, sep = ',')
my.data$Algorithm <- as.factor(my.data$Algorithm)

#preparing the data
set.seed(31)
my_sample <- sort(sample(x = 1:nrow(my.data), replace = FALSE, size = nrow(my.data)*0.90))
my_sample_comp <- setdiff(1:nrow(my.data), my_sample)

#splitting to train and test data
test <- my.data[my_sample, ]
train <- my.data[my_sample_comp, ]

#random forest algorithm
r <- randomForest(Algorithm ~ ., data=train, importance=TRUE, do.trace=100)
summary(r)

#rcv <- rf.crossValidation(r, train, p = 0.1, n = 10)

#10 fold cross validation
folds <- cut(seq(1,nrow(my.data)),breaks=10,labels=FALSE)
for(k in 1:10)
{
  test_i <- which(folds == k)
  train_data <- my.data[-test_i, ]
  test_data <- my.data[test_i, ]
  rcv <- randomForest(Algorithm~., data = train_data, importance = TRUE, do.trace = 100)
  print(rcv)
}

#error rate is 39.34%
print(r)
print(rcv)

#prediction result for test data. here you can use the data to be predicted as test data
predict(r, data = test)

#plot
plot(r)
plot(rcv)  
