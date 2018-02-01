library(data.table)
library(caret)

mercari.data.train <- fread('../../../train.tsv', sep='\t')

sample.submision <- fread('../../../sample_submission.csv', sep=',')

str(mercari.data.train)

summary(mercari.data.train)

#-----------------------------------------------------------------------------------
#---------------- BASELINE MODELS --------------------------------------------------

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

# checking blank spaces
apply(X = mercari.data.train[, c("name", "category_name", "brand_name", "item_description")],
      MARGIN = 2,
      FUN = function(x) length(which(x == " ")))

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

##### TO DO: make file with baseline prediction using linear regression model!!!
##### TO DO: test error on these predictions




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