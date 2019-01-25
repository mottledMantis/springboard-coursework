setwd("C:/Users/Jim/Google Drive/Documents/gits/springboard-coursework")
quality <- read.csv("quality.csv")
# str(quality)
# View(quality)

table(quality$PoorCare)
library(caTools)

set.seed(88) ## only if you want to control the split so others can get same result

split <- sample.split(quality$PoorCare, SplitRatio = 0.75) # set to .75
# so that 75% goes in training set

qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)


QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family = binomial)
#family = binomial tells glm to build a logistic regression model
summary(QualityLog)

#Then look at coeeficients - # of stars, and they are positive, suggsting higher
# values in these variables are indicitave of poorCare
# also look at AIC score, similar to adjusted R-squared in linear, in that it
# factors in number of variables used compared to the number of observations
# used to comapre models, the better model has the lower AIC

# here comes the prediction
predictTrain <- predict(QualityLog, type = "response") #tells predict fcnt to give us probailities
summary(predictTrain)
str(predictTrain)
str(qualityTrain)
#to test actual vs. presdicted
table(qualityTrain$PoorCare, predictTrain > 0.5)

# rows = true outcome, cols = predicted outcome - so 70 correct goodcare, 10 correct poorcare

sensitivity <- 10/25 # correct positives/total positives
sensitivity
specificity <- 70/74 #true negatives/total negatives
specificity


library(ROCR)
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
#predicition is a fucntion from package ROCR, predictTrain is our prediction fuction
#from above, qualityTrain$Poorcare is our TRUE OUTCOMES
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
#tpr - true positive rate, fpr - false positive rate, mapped to x and y axes
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,.1), text.adj = c(-0.2, 1.7))


#Now run prediction on test set
predictTest <- predict(QualityLog, type = "response", newdata = qualityTest)
table(qualityTest$PoorCare, predictTest > 0.3)