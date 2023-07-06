                               ###### BUSINESS DATA ANALYTICS ######
              ############################## SECTION 2 ##############################


       ############################## LINEAR REGRESSION MODEL ##############################


#Load the package caret...
library(caret)


#Set the seed (Replication purposes)
set.seed(999)


#Load the dataset...
df <- read.csv("analysis.csv", header = T)

#Performing regression modelling:
#Reducing the variables to avoid overfitting...
#Split the data into training data and test data...
inTrain <- createDataPartition(df$age, p = 0.8, list = F)
dfTrain <- df[inTrain,]
dfTest <- df[-inTrain,]


#Full model...
fit <- lm(age ~ ., data = dfTrain)
summary(fit)
plot(fit)
#Plot is not a straight line due to the presence of outliers.

#Stepwise model...
fit_step <- step(fit, k = log(nrow(dfTrain)))
summary(fit_step) 
#R-squared = 0.4169, which implies good results

#Bayes Factor...
#exp((BIC1 - BIC2)/2) 
BF <- ((151904.1-151854.8)/2)
BF #24.65
#The final model is 24.65 times more likely to fit the data than the full model.
BF10 = 24.65

#Diagnostic Plots...
plot(fit_step)
#The results are acceptable (little variation due to the presence of outliers)

#Using predict function to make preditions...
pred <- predict.lm(fit_step, dfTest)
pred
#Results show various deviations!

#R-squared...
cor(pred, dfTest$age)^2
#0.4246174, the higher R-squared shows that the model performed well!


#Load the package sjPlot...
library(sjPlot)

#Plot the predicted values of age...
dfTest$pred <- pred
fitresults <- lm(age ~ pred, data = dfTest)
plot_model(fitresults, type = "pred")
#Less area reflects high R-squared.


      ############################## LOGISTIC REGRESSION MODEL ##############################


#Set the seed (Replication purposes)
set.seed(999)


#Converting "y" into a factor
df$y <- as.factor(df$y)
fact <- factor(df$y)
fact


#Split the data into train and test
intrain <- createDataPartition(df$y, p = 0.8, list = F)
trainData <- df[intrain,]
testData <- df[-intrain,]


#Build the model (Logit function)
fit2 <- glm(y ~ ., data = trainData, family = "binomial")
summary(fit2)


#Reducing the variables to avoid overfitting
#Stepwise model 
fit2step <- step(fit2, k = log(nrow(trainData)))
summary(fit2step)


#Pseudo R^2
fitNull <- glm(y ~ 1, data = trainData, family = "binomial")
pseudoR <- 1- (logLik(fit2)/logLik(fitNull))
pseudoR 
#'log Lik.' 0.1672009 (df=42)
#Results give an indication of 17% goodness of fit, which is low!


#Let's see how well the model performs on the unseen data i.e., Test data...
pred2 <- predict(fit2step, testData, type = "response")
head(pred2)
#Probabilities predicted!


#There are probabilities... y's probability is 0.5
pred2 <- ifelse(pred2 > 0.3, 1, 0)
print(confusionMatrix(as.factor(pred2), as.factor(testData$y), positive = "1"))



#ROC Curve
#Load the package verification
library(verification)
pred2 <- predict(fit2step, testData, type = "response")
roc.plot(testData$y == 1, pred2)

