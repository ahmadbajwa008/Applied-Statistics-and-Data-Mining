winequality<- read.csv("winequality-red.csv", sep = ",")
names(winequality)
head(winequality)
tail(winequality)
summary(winequality)
str(winequality)
nrow(winequality)
ncol(winequality)
dim(winequality)
# as.factor function convert a column into a factor column. 
winequality$qualities <- as.factor(winequality$quality)

str(winequality)

set.seed(1234)
pd <- sample(2, nrow(winequality),replace=TRUE,prob=c(0.8,0.2)) 
pd
train <- winequality[pd==1,] 
validate<- winequality[pd==2,]

dim(train) 
dim(validate)
# Retrieve the dimension of the train data set dim(validate)
# Retrieve the dimension of the validate data set
install.packages("party")
library(party)

wine_tree <- ctree(qualities ~ alcohol+density+pH+chlorides+residual_sugar+citric_acid+volatile_acidity+fixed_acidity+free_sulfur_dioxide+total_sulfur_dioxide+sulphates,data =train)
wine_tree
# Plot the tree
plot(wine_tree)
#simple tree
plot(wine_tree, type="simple")

#predictions of the model
predict(wine_tree)

tab <- table(predict(wine_tree), train$qualities)
tab
#diag() function extracts the diagonal of a matrix
sum(diag(tab))/sum(tab) 


1-sum(diag(tab))/sum(tab)
#validate the model on test data set
test_predict <- table(predict(wine_tree, newdata= validate), validate$qualities)
test_predict
#accuracy of the data
sum(diag(test_predict)/sum(test_predict))
#error 
1-sum(diag(tab))/sum(tab)
