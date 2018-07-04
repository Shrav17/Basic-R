#Set Working Directory
setwd("D:/National College of Ireland/Sem-2/ADM")

#Load the Data
titanicData <- read.csv("titanic.csv", header = TRUE, na.strings = c("", stringsAsFactors= TRUE))

#Create a y labels varible
y <- titanicData$Survived

#Check y
table(y)

#Plot Yesses and Nos in y
barplot(table(y), main = "Distribution of Titanic Surivial", ylab="Frequency")

#Create a sample of the data (subset)
set.seed(42)
index <- sample(1:length(y), length(y) * .25, replace=FALSE)
testing <- y[index]

#Create a null model
perishModel <- rep("No", length(testing))

coinModel <- round(runif(length(testing), min=0, max=1))
coinModel <- factor(coinModel, levels = c(0,1), labels = c("No", "Yes"))

#Create Labels for PerishModel
perishModel <- factor(perishModel, levels = c("No", "Yes"), labels = c("No", "Yes"))
#Check PerishModel
table(testing, perishModel)

#Check CoinModel
table(testing, coinModel)

testing <- factor(testing, levels = c(0, 1), labels = c("No", "Yes"))
(coinAccuracy <- 1 - mean(coinModel != testing))

(perishAccuracy <- 1 - mean(perishModel != testing))


#Run 1000 Models 
perish <- c()
coin <- c()
for (i in 1:1000) {
  index <- sample(1:length(y), length(y) * .25, replace=FALSE)
  testing <- y[index]
  testing <- factor(testing, levels = c(0,1), labels = c("No", "Yes"))
  coinModel <- round(runif(length(testing), min=0, max=1))
  coinModel <- factor(coinModel, levels = c(0,1), labels = c("No", "Yes"))
  coin[i] <- 1 - mean(coinModel != testing)
  perish[i] <- 1 - mean(perishModel != testing)
}

#See Results of 1000 models
results <- data.frame(coin, perish)
names(results) <- c("Coin Toss Accuracy", "Everyone Perishes Accuracy")
summary(results)

#Plotting the Model Outcomes
install.packages("ggplot2")
install.packages("reshape")
library(ggplot2)
library(reshape)
ggplot(melt(results), mapping = aes (fill = variable, x = value)) + geom_density (alpha = .5)

boxplot(results)

#Subsetting the Original Dataset
df <- titanicData[, c("Survived", "Sex")]
df$Survived <- factor(df$Survived, levels = c(0,1), labels = c("No", "Yes"))

#Creating new samples
index <- sample(1:dim(df)[1], dim(df)[1] * .75, replace=FALSE)
training <- df[index, ]
testing <- df[-index, ]
table(training$Survived, training$Sex)

#Lets build a little Naive model around that
predictSurvival <- function(data) {
  model <- rep("No", dim(data)[1])
  model[data$Sex == 'female'] <- "Yes"
  return(model)
}

#Make 1000 again for this dumb model too
women <- c()
for (i in 1:1000) {
  index <- sample(1:dim(df)[1], dim(df)[1] * .75, replace=FALSE)
  testing <- df[-index, ]
  womenModel <- predictSurvival(testing)
  women[i] <- 1 - mean(womenModel != testing$Survived)
}
results$`Women Accuracy` <- women
names(results) <- c("Coin", "All Perish", "Women")
boxplot(results)


#Lets build a little Naive model around that
predictKidsSurvival <- function(data) {
  model <- rep("No", dim(data)[1])
  model[data$Age <= 2] <- "Yes"
  return(model)
}

#Make 1000 again for this dumb model too
kids <- c()
for (i in 1:1000) {
  index <- sample(1:dim(df)[1], dim(df)[1] * .75, replace=FALSE)
  testing <- df[-index, ]
  kidsModel <- predictKidsSurvival(testing)
  kids[i] <- 1 - mean(kidsModel != testing$Survived)
}
results$`Kids Accuracy` <- kids
names(results) <- c("Coin", "All Perish", "Women", "Kids")

boxplot(results)




#Lets build a little Naive model around that
predictWomenKidsSurvival <- function(data) {
  model <- rep("No", dim(data)[1])
  model[data$Age <= 10 | data$Sex == "Female"] <- "Yes"
  return(model)
}

#Make 1000 again for this dumb model too
womenkids <- c()
for (i in 1:1000) {
  index <- sample(1:dim(df)[1], dim(df)[1] * .75, replace=FALSE)
  testing <- df[-index, ]
  womenkidsModel <- predictKidsSurvival(testing)
  womenkids[i] <- 1 - mean(womenkidsModel != testing$Survived)
}
results$`Women and Kids Accuracy` <- womenkids
names(results) <- c("Coin", "All Perish", "Women", "Kids")
boxplot(results)

#-------------Evaluating Models - Lab2---------------#
install.packages("gmodels")
library(gmodels)
womenModel <- as.factor(womenModel)
CrossTable(testing$Survived, womenModel)

CrossTable(testing$Survived, womenModel, prop.chisq = F, prop.c = F, prop.r = F)


install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

caret::confusionMatrix(womenModel, testing$Survived, positive = "Yes")

install.packages("ModelMetrics")
library(ModelMetrics)
auc(testing$Survived, womenModel)



install.packages("ROCR")
library(ROCR)
pWomenModel <- prediction(as.numeric(womenModel), as.numeric(testing$Survived))
perfWomenModel <- performance(pWomenModel, measure = "tpr", x.measure = "fpr")
plot(perfWomenModel)



auc <- performance(pWomenModel, measure = "auc")
auc <- auc@y.values[[1]]
auc

install.packages("randomForest")
install.packages("mice")
library(mice)
titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
titanicData$Survived <- factor(titanicData$Survived, levels = c(0,1), labels = c("No", "Yes"))
titanicData$Pclass <- as.factor(titanicData$Pclass)
titanicData <- titanicData[, -c(1,11)] #remove feature 1 and 11
titanicData$Embarked[c(62, 830)] <- 'C' #result of Embarked imputation exercise

#use a random forest to impute missing age values
mice_mod <- mice(titanicData[, !names(titanicData) %in%
                               c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
mice_output <- complete(mice_mod)
titanicData$Age <- mice_output$Age
#feature engineering: make a feature to represent a passenger is a child
titanicData$Child[titanicData$Age < 18] <- "Yes"
titanicData$Child[titanicData$Age >= 18] <- "No"
titanicData$Child <- factor(titanicData$Child)
#feature engineer a title feature
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
titanicData$Title <- gsub('(.*, )|(\\..*)', '', titanicData$Name)
titanicData$Title[titanicData$Title == 'Mlle'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Ms'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Mme'] <- 'Mrs'
titanicData$Title[titanicData$Title %in% rare_title] <- 'Rare Title'
titanicData$Title <- as.factor(titanicData$Title)
#feature engineer a few more things using the passenger name
titanicData$Name <- as.character(titanicData$Name)
titanicData$Surname <- sapply(titanicData$Name,
                              FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
titanicData$Fsize <- titanicData$SibSp + titanicData$Parch + 1
#remove features 3, 7, and 11
titanicData[3] <- NULL
titanicData[7] <- NULL
titanicData[11] <- NULL
# feature engineer a family size categorical variable
titanicData$FsizeD[titanicData$Fsize == 1] <- 'singleton'
titanicData$FsizeD[titanicData$Fsize < 5 & titanicData$Fsize > 1] <- 'small'
titanicData$FsizeD[titanicData$Fsize > 4] <- 'large'
titanicData$FsizeD <- as.factor(titanicData$FsizeD)



set.seed(1337)
index <- sample(1:length(y), length(y) * .20, replace=FALSE)

training <- titanicData[-index, ]
testing <- titanicData[index, ]

#----------------------------------Naive Bayes--------------------------------------------------
getwd()
setwd("D:/National College of Ireland/Sem-2/ADM")
getwd()
titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
View(titanicData)
str(titanicData)

table(titanicData$SibSp)

table(titanicData$Parch)

par(mfrow=c(1,2))

hist(titanicData$Fare, breaks = 30)

hist(titanicData$Age)


titanicData$FareBinned <- cut(titanicData$Fare,
                              breaks = c(0,10,50,max(titanicData$Fare)),
                              labels=c("low", "middle", "high"))

table(titanicData$FareBinned, titanicData$Pclass)

aggregate(Fare ~ Pclass, data=titanicData, FUN=summary)

titanicData$AgeBinned <- cut(titanicData$Age,
                             breaks = c(0,10,20,30,40,50,60,70,max(titanicData$Age)),
                             labels=c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70+"))
table(titanicData$AgeBinned)
str(titanicData)
titanicData <- titanicData[-c(4:7,11)]
str(titanicData)

library(e1071)
nb <- naiveBayes(training[-1], training$Survived)
nb_predict <- predict(nb,testing[-1])
caret::confusionMatrix(nb_predict,testing$Survived,positive ="Yes")
