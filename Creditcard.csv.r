
df <- read.csv("C:\\Users\\ramae\\Documents\\creditcard.csv", header = TRUE,
                 stringsAsFactors = FALSE,
                 strip.white = TRUE ,
                 sep = ',')

head(str(df))

#under sampling of the dataset
table(df$Class)
library(ROSE)
under.sample <- ovun.sample(Class ~ ., data = df, method="under", N=984)$data
print('Number of transactions in  dataset after applying Under sampling method')
table(under.sample$Class)
head(under.sample)
fix(under.sample)
str(under.sample)

#Decision Tree
set.seed(123)
library(rpart)
classifier1 = rpart(formula = Class~.,data = under.sample)
summary(classifier1)
#plot the decision tree
plot(classifier1)
text(classifier1,pretty = 0)
library(rattle)
fancyRpartPlot(classifier1)
plotcp(classifier1)

#Boruta
set.seed(123)
library(Boruta)
set.seed(123)
boruta.sample <- Boruta(Class~., data = under.sample,doTrace = 2)
print(boruta.sample)
final.boruta <- TentativeRoughFix(boruta.sample)
print(final.boruta)
plot(final.boruta)

#train data set

traindata <-sample(1:nrow(under.sample),nrow(under.sample)*.8)
head(traindata)
testdata  <- under.sample[-traindata, ]   # test data

#Random  Forest Tree

library(randomForest)
set.seed(1)
rf.under.sample <- randomForest(Class~.,data=under.sample,subset=traindata,mtry=6,importance=TRUE)
rf.under.sample
yhat.rf <- predict(rf.under.sample,newdata=under.sample[-traindata,])
varImpPlot(rf.under.sample)


#Linear Regression Prediction using Time and Amount
under.sample1 <- under.sample[c(-31)]
head(under.sample1)
str(under.sample1)

# Feature Scaling for 'amount'
library(scales)
under.sample[,29] = rescale(under.sample[,29], to = c(0,1))
summary(under.sample$Amount)

set.seed(100)  # setting seed to reproduce results of random sampling
traindata <-sample(1:nrow(under.sample1),nrow(under.sample1)*.8)
head(traindata) # row indices for training data
trainingdata <- under.sample1[traindata, ]  # model training data
testdata  <- under.sample1[-traindata, ]   # test data
head(trainingdata)
dim(trainingdata)
head(testdata)
dim(testdata)

linearModel <- lm(Amount ~ ., data=trainingdata) #. is used to get all the x/column values
summary(linearModel)

linearModel <- lm(Amount ~ V14+V10+V12+V3+V4, data=trainingdata) #multi linear regression
summary(linearModel)

linearModel <- lm(Time ~ ., data=trainingdata) #. is used to get all the x/column values
summary(linearModel)

linearModel <- lm(Time ~ V14+V10+V12+V3+V4, data=trainingdata) #multi linear regression
summary(linearModel)
#Predict the linear model with newdata set
linearPred <- predict(linearModel, testdata)  # predict difference
#with Amount
result <- data.frame(Amount = testdata$Amount , PredAmt= linearPred , 
                     diff = (testdata$Amount-linearPred) )
head(result)
#with Time
result1 <- data.frame(Time = testdata$Time , PredTime = linearPred,
                      diff = (testdata$Time-linearPred))
head(result1)

model1 <- lm(Amount ~ V10+V12+V14+V3+V4, data=under.sample1)
summary(model1)

regression <- lm(Amount ~ V10, data=under.sample1)
summary(regression)
coeff = coefficients(regression)
eq = paste0("y = ", round(coeff[1],2), "+", round(coeff[2],2), "*x")
#Plot Regression Amount~V10
plot(Amount ~ V10, data=under.sample1, col="red", 
     main = paste0("Regression Equation = ",eq))

abline(lm(under.sample1$Amount ~ under.sample1$V10), lty=2, lwd=4, col="blue")
#Plot Regression Time~V10
plot(Time ~ V10, data=under.sample1, col="red", 
     main = paste0("Regression Equation = ",eq))

abline(lm(under.sample1$Time ~ under.sample1$V10), lty=2, lwd=4, col="blue")


#Logistic Regression against Class variable
table(under.sample$Class)
set.seed(100)  # setting seed to reproduce results of random sampling
traindata <-sample(1:nrow(under.sample),nrow(under.sample1)*.8)
head(traindata) # row indices for training data
trainingdata <- under.sample[traindata, ]  # model training data
testdata  <- under.sample[-traindata, ]   # test data
head(trainingdata)

#logistic regression model
model <- glm (Class ~ ., data = trainingdata, family = binomial)
summary(model)

predict <- predict(model, type = 'response')
#confusion matrix
table(trainingdata$Class, predict > 0.5)
#ROC Curve
library("ggplot2")
pl <- ggplot(under.sample, aes(under.sample$Class, as.numeric(V10+V12+V3+V4+V14),
                       color=V10+V12+V3+V4+V14)) +
  
  geom_point(position=position_jitter(height=0.5, width=0)) +
  
  xlab("Pr (Class)") + ylab("V10+V12+V3+V4+V14")

print(pl)

var = under.sample$V10+under.sample$V12+under.sample$V3+under.sample$V4+under.sample$V14

ggplot(under.sample, aes(x=var, y=V10+V12+V3+V4+V14)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)
library(car)
vif(model)
options(warn=-1)
library(InformationValue)
plotROC(under.sample, predict)

model1 <- glm(Class ~ V10+V12+V3+V4+V14, 
                data=trainingdata, 
                family=binomial(link="logit"))
predicted <- predict(model1, testdata, type="response")  # predicted scores
predicted
#confusion matrix
table(testdata$Class, predicted > 0.8)
model1
summary(model)
library(car)
vif(model1)
plotROC(testdata,predicted)



