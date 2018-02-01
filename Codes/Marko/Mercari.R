library(data.table)
library(caret)

mercari.data.train <- fread('../../../train.tsv', sep='\t')

sample.submision <- fread('../../../sample_submission.csv', sep=',')

str(mercari.data.train)

summary(mercari.data.train)

# Making a baseline submission
mean(mercari.data.train$price)

write.csv(sample.submision, file = "../../../baseline.submission.csv", row.names = FALSE)

# inspect data types
sapply(mercari.data.train, class)

# checking number of NAs
apply(X = mercari.data.train,
      MARGIN = 2,
      FUN = function(x) length(which(is.na(x))))

# checking empty strings
apply(X = mercari.data.train[, c("name", "category_name", "brand_name", "item_description")],
      MARGIN = 2,
      FUN = function(x) length(which(x == "")))

# Creating train and test subsets
set.seed(69)

train.indices <- createDataPartition(y = mercari.data.train$price, 
                                     p = .8, 
                                     list = FALSE)

train.data <- mercari.data.train[train.indices, ]
test.data <- mercari.data.train[-train.indices, ]

# making secong baseline submission
baseline.lm <- lm(formula = price ~ item_condition_id + shipping, data = train.data)

baseline.pred <- predict(object = baseline.lm, newdata = test.data)
head(baseline.pred)

##### TO DO: test error on these predictions
