setwd("C:\\Desktop sa starog racunara\\Data Science\\Mercary\\Mercari_Price\\Codes\\Marija")
library(data.table)
mercari.train.set <- fread(input = "../../../train.tsv", sep = "\t")

library(caret)
summary(mercari.train.set)
str(mercari.train.set)

#Imam 8 feature-a, za linearnu regresiju su mi potrebni samo int i numeric tipovi
#Outcome varijabla ce biti price, prediktori item_condition_id i shipping

#-----------------Linear regression------------------

set.seed(1234)
train.indices <- createDataPartition(y = mercari.train.set$price, p = 0.8, list = FALSE)
training.set <- mercari.train.set[train.indices, ]
test.set <- mercari.train.set[-train.indices, ]

#provera da li su slicno raspodeljeni - jesu
summary(training.set$price)
summary(test.set$price)

#----------------Linear regression model------------------

linear.model <- lm(price ~ item_condition_id + shipping, data = training.set)
summary(linear.model)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       31.92053    0.09467  337.19   <2e-16 ***
#item_condition_id -0.88435    0.03975  -22.25   <2e-16 ***
#shipping          -7.83356    0.07217 -108.54   <2e-16 ***

# item_condition_id ima moguce vrednosti 1,2,3,4,5
# shipping moze da ima vrednost 0 ili 1
# Iz modela se vidi da svako povecanje item_condition_id za jednu jedinicu dovodi do pada cene proizvoda za 0.88435 jedinica
# Ukoliko prodavac placa isporuku (shipping = 1), cena proizvoda se smanjuje za 7.83356 jedinica, a ako je placa kupac (shipping = 0), ne smanjuje se cena

#----------------------Prediction-------------------------

linear.model.prediction <- predict(linear.model, test.set)
head(linear.model.prediction) #predicted
#actual je test.set$price

#----------------------Evaluation-------------------------

#  R^2 = 1 - RSS/TSS (R squared) - predictive power of the model
RSS <- sum((linear.model.prediction - test.set$price)^2) #Residual sum of squares
TSS <- sum((mean(training.set$price) - test.set$price)^2) #Total sum of squares
R.squared <- 1 - RSS/TSS
R.squared # 0.00985122 , veci je nego R^2 na training setu, ali ovo je svakako jako los model koji ima malu prediktivnu moc

# RMSE (Root mean squared error) - greska koju pravimo tokom predikcije
RMSE <- sqrt(RSS/nrow(test.set))
RMSE / mean(test.set$price) #1.440129 , znaci da je greska 144% - model je los

#RMSLE - Kaggle formula
?rmsle
install.packages("MLmetrics")
library(MLmetrics)

rmsle <- RMSLE(linear.model.prediction, test.set$price)
rmsle #0.8016423

#------------------Exploratory analysis-------------------

prop.table(table(mercari.train.set$shipping))
# u proseku za 55.2% proizvoda isporuku placa kupac, dok za 44.7% placa prodavac
table(Shipping = mercari.train.set$shipping, Condition = mercari.train.set$item_condition_id)
#ovde se vidi da se drasticno vise isporucuju proizvodi kojima je condition 1 ili 2, nego oni kojima je condition 5, sto znaci da na skali 1-5
#veca vrednost oznacava sve losije stanje, a to se isto vidi i iz linearnog modela

#unique(mercari.train.set$category_name)

#Pravim movu varijablu MainCategory koja ce dodeljivati svakoj opservaciji glavnu kategoriju u koju spada
#head(mercari.train.set$category_name)
#unlist(strsplit(x = mercari.train.set$category_name[1], split = "/"))[1] #izbacuje prvu rec
#mercari.train.set$MainCategory <- sapply(X = mercari.train.set$category_name, FUN = function(x) unlist(strsplit(x = mercari.train.set$category_name, split = "/"))[1])

?boxplot
ggplot(data = mercari.train.set, mapping = aes(x = shipping, y = price, fill = shipping))+geom_boxplot()
apply(X = mercari.train.set[,"price"], MARGIN = 2, FUN = function(x) length(boxplot.stats(x)$out))
#Varijabla price ima 119352 outliera
summary(mercari.train.set$price)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   10.00   17.00   26.74   29.00 2009.00 

min(boxplot.stats(mercari.train.set$price)$out)
#najmanji outlier je 58USD

price.outliers <- which(mercari.train.set$price > 57) #indeksi
shipping.price.out <- mercari.train.set[price.outliers, ] #podskup dataseta koji sadrzi samo ops. sa outlierima
summary(shipping.price.out$price)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#57.5    66.0    84.0   112.9   121.0  2009.0 

shipping.price.out$brand_name[shipping.price.out$price > 1500 & shipping.price.out$price < 2010]
shipping.price.out$brand_name[shipping.price.out$price > 1000 & shipping.price.out$price < 1500]
shipping.price.out$brand_name[shipping.price.out$price > 500 & shipping.price.out$price < 1000]
#Brendovi sa najvisim cenama su LV, YSL, Apple, Gucci, Chanel itd. - logicno 
#ima puno opservacija kod kojih u polju brand_name stoji prazan string??
shipping.price.out$brand_name[shipping.price.out$price > 100 & shipping.price.out$price < 500]
#ponavljaju se ovi gore brendovi, ali ima i Nike, Canon, Adidas, Samsung, Air Jordan itd.
#Zakljucak je da su najskuplji proizvodi(outlieri) uglavnom sportska oprema, telefoni, fotoaparati i racunarska oprema i garderoba

mercari.train.set$shipping <- factor(mercari.train.set$shipping, levels = c(0,1), labels = c("Buyer","Seller"))
str(mercari.train.set)


