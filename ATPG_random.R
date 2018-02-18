library(randomForest)

#loading data
my.data <- read.table("ATPG1.csv", header = TRUE, sep = ',')
my.data$Algorithm <- as.factor(my.data$Algorithm)

#preparing the data
set.seed(31)
my_sample <- sort(sample(x = 1:nrow(my.data), replace = FALSE, size = nrow(my.data)*0.90))

#splitting to train and test data
test <- my.data[my_sample, ]
train <- my.data[-my_sample, ]

#random forest algorithm
r <- randomForest(Algorithm ~ ., data=train, importance=TRUE, do.trace=100)
summary(r)

#error rate is 39.34%
print(r)

#prediction result for test data. here you can use the data to be predicted as test data
predict(r, data = test)