library(data.table)
library(caret)
library(MLmetrics)

mercari.data.train <- fread('../../../train.tsv', sep='\t')
mercari.data.test <- fread('../../../test.tsv', sep='\t')

sample.submision <- fread('../../../sample_submission.csv', sep=',')

str(mercari.data.train)

summary(mercari.data.train)



#-----------------------------------------------------------------------------------
#---------------- BASELINE MODELS --------------------------------------------------

# Creating train and test subsets
set.seed(69)

train.indices <- createDataPartition(y = mercari.data.train$price, 
                                     p = .8, 
                                     list = FALSE)

train.data <- mercari.data.train[train.indices, ]
test.data <- mercari.data.train[-train.indices, ]


# Making a baseline submission
mean.mercari.price <- mean(train.data$price)

# Evaluation of baseline model
rmsle.baseline.model <- RMSLE(y_pred = mean.mercari.price, y_true = test.data$price)

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

# checking blank spaces
apply(X = mercari.data.train[, c("name", "category_name", "brand_name", "item_description")],
      MARGIN = 2,
      FUN = function(x) length(which(x == " ")))



#----------------------------------------------------------------------------#
#------------- Making second baseline submission ----------------------------#

# baseline linear regression model
baseline.linear.model <- lm(formula = price ~ item_condition_id + shipping, data = train.data)

# making prediction on subseted test data
baseline.pred <- predict(object = baseline.linear.model, newdata = test.data)
head(baseline.pred)

# making predictions on mercuri test data, using baseline linear regression model
baseline.linear.model.pred <- predict(object = baseline.lm, newdata = mercari.data.test)

# making data frame out of test_id and baseline linear regression model predictions
baseline.linear.model.submission <- cbind(mercari.data.test$test_id, baseline.linear.model.pred)

column.names <- c("test_id", "price")

baseline.linear.model.submission <- as.data.frame(x = baseline.linear.model.submission)

colnames(x = baseline.linear.model.submission) <- column.names

write.csv(baseline.linear.model.submission, file = "../../../baseline.linear.model.submission.csv", row.names = FALSE)

# Evaluation of baseline linear regression model
rmsle.baseline.linear.model <- RMSLE(y_pred = baseline.pred, y_true = test.data$price)


#-----------------------------------------------------------------------------------
#---------------- EXPLORATORY ANALYSIS ---------------------------------------------

# Variable:   price

range(mercari.data.train$price)

ggplot(data = mercari.data.train, mapping = aes(x = price)) +
  geom_histogram() +
  xlab("Price") + 
  ylab("count") +
  ggtitle("Histogram of item price")

ggplot(data = mercari.data.train, mapping = aes(x = log(price + 1))) +
  geom_histogram() +
  xlab("Price") + 
  ylab("count") +
  ggtitle("Histogram of log item price + 1")

length(which(mercari.data.train$price == 0))

summary(mercari.data.train$price)

# - raspon cena proizvoda je od 0$ do 2009$
# - 874 artikala ima cenu 0$
# - prosecna cena artikla je 26.74$
# - medijana je postavljena na 17$
# - raspodela cena proizvoda je asimetricna udesno
# - cene proizvoda se gomilaju na levo, ka 0
# - prirodni logaritam (cena + 1) centriran je oko 3, i ima veci raspon vrednosti ka desnoj strani, pre svega zbog ogranicenja od 0 sa leve strane




# Variable:   Item condition

table(mercari.data.train$item_condition_id)

prop.table(table(mercari.data.train$item_condition_id)) * 100

ggplot(data = mercari.data.train, mapping = aes(item_condition_id)) +
  geom_histogram()

ggplot(data = mercari.data.train, mapping = aes(x = item_condition_id, y = price)) +
  geom_boxplot()

head(mercari.data.train$item_description[mercari.data.train$item_condition_id == 1])
head(mercari.data.train$item_description[mercari.data.train$item_condition_id == 5])

# After combining item_condition_id and item_description, I came to a conclusion that
# items with item_condition_id == 1, are the new items, brand new items etc.
# Items with item_condition_id == 5, are items that are damaged, out of order items...
