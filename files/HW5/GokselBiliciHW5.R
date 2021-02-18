require(lubridate)
require(readxl)
require(zoo)
require(data.table)
require(readxl)
require(dplyr)
require(ggplot2)
require(stats)
require(readr)
require(tidyverse)
require(corrplot)

data_path <- "D:/MyUbuntu/GitHub/fall20-blc-gksl/files/HW5"
setwd(data_path)

theData <- read_excel("sales.xlsx")
theData <- as.data.table(theData)
theData <- theData %>% rename(ANXIETY = ANX,
                   EXPERIENCE = EXP,
                   SELLING_APTITUDE = APT)
theData
#aaaaaaaaaaaaaaaaaaaaaaaaaa
#theCorrelation <- cor(theData)
#corrplot(theCorrelation, type = "upper", order = "hclust", 
#          tl.col = "red", tl.srt = 45)
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(theData, type="upper")

#bbbbbbbbbbbbbbbbbbbbbbbbbb
#currentModel
#newModel
#reducedModel

#First Adding Step
currentModel <- lm(SALES ~ AGE, data=theData)
newModel1 <- lm(SALES ~ AGE + ANXIETY, data=theData)
newModel2 <- lm(SALES ~ AGE + GPA, data=theData)
newModel3 <- lm(SALES ~ AGE + SELLING_APTITUDE, data=theData)
newModel4 <- lm(SALES ~ AGE + EXPERIENCE, data=theData)

anova(currentModel,newModel1)
anova(currentModel,newModel2)
anova(currentModel,newModel3)
anova(currentModel,newModel4)

#newModel3 was statistically important
currentModel <- newModel3

#First Reducing Step
reduceModel1 <- lm(SALES ~ SELLING_APTITUDE, data=theData)
anova(currentModel, reduceModel1)

# AGE was important so no removing needed

#Second Adding Step
newModel1 <- lm(SALES ~ AGE + SELLING_APTITUDE + ANXIETY, data=theData)
newModel2 <- lm(SALES ~ AGE + SELLING_APTITUDE + GPA, data=theData)
newModel3 <- lm(SALES ~ AGE + SELLING_APTITUDE + EXPERIENCE, data=theData)

anova(currentModel,newModel1)
anova(currentModel,newModel2)
anova(currentModel,newModel3)
#No significant p-value and F-test so no adding

#Second Reduction Step

reduceModel1 <- lm(SALES ~ AGE, data=theData)
reduceModel2 <- lm(SALES ~ SELLING_APTITUDE, data=theData)
reduceModel3 <- lm(SALES ~ 1, data=theData)

anova(currentModel, reduceModel1)
anova(currentModel, reduceModel2)
anova(currentModel, reduceModel3)
#All current variables are important so no reduction
fitBestFromStepImplementation <- currentModel

#cccccccccccccccccccccccccc
fitStart <- lm(SALES ~ 1, data = theData) # only intercept included
fitAll <- lm(SALES ~ ., data = theData) # every parameter included
stepResults <- step(fitStart, direction = "both", scope=formula(fitAll))
stepResults
fitBestFromStepFunction <- lm(formula = SALES ~ AGE + SELLING_APTITUDE, data = theData)
fitBestFromStepFunction

# compare
fitBestFromStepImplementation
fitBestFromStepFunction

#dddddddddddddddddddddddddd
finalModel <- lm(SALES ~ AGE + SELLING_APTITUDE, data=theData)
finalModel

#eeeeeeeeeeeeeeeeeeeeeeeeee
finalModelWithGPA <- lm(SALES ~ AGE + SELLING_APTITUDE + GPA, data=theData)
anova(finalModel, finalModelWithGPA)
#Hypothesis 0 => GPA has no effect for sales
#Hypothesis 1 => GPA is significant for sales
#p-value => 0.6611



