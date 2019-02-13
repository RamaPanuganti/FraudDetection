
df <- read.csv("C:\\Users\\ramae\\Documents\\creditcard.csv", header = TRUE,
                 stringsAsFactors = FALSE,
                 strip.white = TRUE ,
                 sep = ',')

head(str(df))
library(ROSE)
under.sample <- ovun.sample(Class ~ ., data = df, method="under", N=984)$data
print('Number of transactions in  dataset after applying Under sampling method')
table(under.sample$Class)
under.sample1 <- under.sample[c(-31)]
head(under.sample1)
x <- under.sample1#filtering rest 30  coulmns to x 
head(under.sample1)
y <- under.sample$Class #y axis Class

library(ggplot2)
qplot(V10,V14,data=under.sample,color = Class)
qplot(V11,V3, data=under.sample,color = Class)

library("e1071")
svm.model1 <- svm(Class ~ ., data=under.sample, kernel="radial", cost=1, gamma=0.05)
summary(svm.model1)

pred <- predict(svm.model1,x) 
tab <- table(predicted=pred,actual=y)

1-sum(diag(tab))/sum(tab)

library(caTools)
classifier2 = svm(formula = Class ~ V10+V14, data = under.sample, type = 'C-classification',
                  kernel = 'radial',gamma=0.05)
classifier2
test.pred1 = predict(classifier2, type = 'radial', data = under.sample)
summary(test.pred1)
# Making Confusion Matrix
confusionm = table(under.sample$Class, test.pred1)
confusionm
1-sum(diag(tab))/sum(tab)

classifier3 = svm(formula = Class ~ V11 + V3, data = under.sample,
                  type = 'C-classification', kernel = "linear")
classifier3
test.pred2 = predict(classifier3, type = 'linear', data = under.sample)
summary(test.pred2)
# Making Confusion Matrix
confusionm1 = table(under.sample$Class, test.pred2)
confusionm1
1-sum(diag(tab))/sum(tab)

library(caret)
plot(under.sample$V10, under.sample$V14, col = as.integer(under.sample[, 31]), 
     pch = c("o","+")[1:284807 %in% classifier2$index + 1], cex = 2, 
     xlab = "V10", ylab = "V14")
legend(x = 3.37, y=7.5, legend = c("V10","V14"),fill = c('blue','green'))

plot(classifier2,under.sample, V10 ~ V14)
plot(classifier3, under.sample, V11 ~ V3)




#train data set
traindata <- sample(1:nrow(under.sample), 0.8*nrow(under.sample)) 
trainingdata <- under.sample[traindata, ]  # model training data
testdata  <- under.sample[-traindata, ]   # test data
head(trainingdata)
head(testdata)

classifier4 = svm(formula = Class ~ V11 + V3, data = trainingdata,
                  type = 'C-classification', kernel = "linear")
classifier4
test.pred3 = predict(classifier4, type = 'linear', data = trainingdata)
summary(test.pred3)
# Making Confusion Matrix
confusionm2 = table(trainingdata$Class, test.pred3)
confusionm2
1-sum(diag(confusionm2))/sum(confusionm2)

plot(classifier4, trainingdata, V11 ~ V3)

classifier5 = svm(formula = Class ~ V11 + V3, data = testdata,
                  type = 'C-classification', kernel = "linear")
classifier5
test.pred4 = predict(classifier5, type = 'linear', data = testdata)
summary(test.pred4)
# Making Confusion Matrix
confusionm3 = table(testdata$Class, test.pred4)
confusionm3
1-sum(diag(confusionm3))/sum(confusionm3)

plot(classifier4, trainingdata, V11 ~ V3)

plot(classifier5,testdata,V11 ~ V3)


