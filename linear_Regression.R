
#----------------------------------Linear Regression Model--------------------------------
#load the data into  r
library(MASS)
data("Boston")
View(Boston)


#Data discription
?Boston

#splitting the data into testing and training
set.seed(2)

library(caTools) #sample.split function is present in this package
split <-  sample.split(Boston$medv, SplitRatio = 0.7)
split #splitting the data in 70:30 ratio

training_data<- subset(Boston,split = "TRUE")
testing_data <- subset(Boston, split = "False")

#to view the correlation of variables
plot(Boston$crim,Boston$medv, cex=1 , xlab = "Crime Rate", ylab="Price")
cr <- cor(Boston)

#finding the correlation between the different variable in the dataset
install.packages("corrplot")
library(corrplot)
cr<- cor(Boston)
corrplot(cr,type = "lower")
corrplot(cr,method = "number")

#creating scatter plot mattrix
attach(Boston)
library(lattice)
splom(-Boston[c(1:6)],groups=NULL,axis.line.tck=0)

#studying rm and medv
plot(rm,medv)
abline(lm(medv~rm),col="red")


#finding collinearity
library(caret)

#to exclude medv(output)
Boston_a=subset(Boston,select = -c(medv))
numericData <- Boston_a[sapply(Boston_a,is.numeric)]
descrCor <- cor(numericData)
View(training_data)
#vif 
install.packages("car")
library(car)
model <- lm(medv~.,data=training_data)
vif(model)
#from the above output we can find the values with higher value have the greater correlation
#here in this rad and tax are highly correlated

#now to create model we will use all the columns

model <- lm(medv~.,data = training_data)
#medv~. or medv~(sum of all other columns)
summary(model)

#removing the variables which are less then 0.05 or with no stars 
model <- lm(medv~ crim +zn+chas+rm+rad+nox+dis+ptratio+black+lstat,data = training_data)

#now we use this model to predict to output the testing data
predic <- predict(model,testing_data)
predic

#to compare the predict values and actual values
plot(testing_data$medv,type = "l",lty = 1.8,col="green")
lines(predic,type="l",col="blue")

#now we can use this model to predict the output of the sample dataset
#predic <- predict(model, sample_data)
#predic