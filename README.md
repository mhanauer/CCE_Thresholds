---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
```{r}
library(MASS)
library(caret)
library(pROC)
library(semTools)
```


So think about BARC-10 with scores ranging from 1-7 so total score ranges from 10 to 70.  

So we could have a dicotomous variable of relpase or not (not sure how this is defined)

Also have medical costs? Maybe medical costs over the average of a person for their demographics if possible?  Or statistically significantly over the average person?

```{r}
set.seed(1234)
n = 300
BARC_10Samp = c(10:70)
BARC_10 = sample(BARC_10Samp, n, replace = TRUE)
BARC_10

randomEffectsCorr = matrix(c(1,.6,.6, 1), ncol = 2)
randomEffectsCorr

randomEffects = mvrnonnorm(n, mu = c(40,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)

randomEffects$X2 = ifelse(randomEffects$X2 > 0, 1, 0)
colnames(randomEffects) = c("BARC10", "Relapse")
cor(randomEffects)
datSenSpec = randomEffects
datSenSpec$Relapse
MedCostsSamp = c(0:100000)
MedCosts = sample(Med_costsSamp, n , replace = TRUE)
datSenSpec$MedCosts = MedCosts
```
Build a sensitivity model
Have a logisitic model that predicts based on BARC-10 scores the probability 
https://statinfer.com/203-4-2-calculating-sensitivity-and-specificity-in-r/
```{r}
BARC10Model = glm(Relapse ~ BARC10, family = binomial, data = datSenSpec)
threshold = .5
predValues = ifelse(predict(BARC10Model, type = "response") > threshold, 1, 0)
conf_matrix = table(predValues, Recid)
conf_matrix
```
Now use CARET.  You want the auc to be close to one, which means you are accurately classifying people into the correct categories 
```{r}
sensitivity(conf_matrix)
specificity(conf_matrix)
roccurve= roc(Recid, predValues)
plot(roccurve)
auc(roccurve)
```
So basicaly you would do this for different ranges.  So if we say 50-60 is the good, then we want to see how this threshold does with auc and roc for different criteria.

So now we want to validate this?
So I think with this model, we can identify only those variables that will accurately classify whether someone relpased or not.  So if we have multiple variable 
```{r}
datSenSpec40Plus = subset(datSenSpec, BARC10 > 39)
BARC10Model40Plus = glm(Relapse ~ BARC10, family = binomial, data = datSenSpec40Plus)

threshold = .5
predValues40 = ifelse(predict(BARC10Model40Plus, type = "response") > threshold, 1, 0)
conf_matrix40 = table(predValues40, datSenSpec40Plus$Relapse)
conf_matrix40

sensitivity(conf_matrix40)
specificity(conf_matrix40)
roccurve40= roc(datSenSpec40Plus$Relapse, predValues40)
plot(roccurve40)

auc(roccurve)
auc(roccurve40)
```
The question I want to answer is at level of the BARC-10 are more than 50% likely to go to the hospitial.  Sounds like an IRT problem.  Because we want the theta level 
Maybe just item difficulty in general??
Can we translate a theta into a score?  
No that is look at total score not what we want.

So use a machine learning algorithm and then figure out where the BACR-10 level is with 50% going into relapse
```{r}
datSenSpec$MedCosts = NULL
inTrain = createDataPartition(y = datSenSpec$Relapse, p = .75, list = FALSE)
training = datSenSpec[inTrain,]
testing = datSenSpec[-inTrain,]
```
Now doing cross validation
```{r}
fitControl = trainControl(method = "repeatedcv", number = 10)
gmbFit1 = train(Relapse ~., data = datSenSpec, method = "gbm", trControl = fitControl, verbose =FALSE)

```


