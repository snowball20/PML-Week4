---
title: "Untitled"
author: "Pavitra Desai"
date: "23/10/2021"
output:
  html_document:
    keep_md: yes
---
### Introduction 
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. In this project, our goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.The goal of this project is to predict the manner in which they did the exercise.  
### Requirements

```r
library(caret)
```

```
## Loading required package: ggplot2
```

```
## Loading required package: lattice
```

```r
library(randomForest)
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
library(rpart)
library(rpart.plot)
```
### Downloading data

```r
trainurl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(trainurl))
testing <- read.csv(url(testurl))
dim(training)
```

```
## [1] 19622   160
```

```r
dim(testing)
```

```
## [1]  20 160
```
### Cleaning data

```r
nzv <- nearZeroVar(training)
training <- training[,-nzv]
testing <- testing[,-nzv]
dim(training)
```

```
## [1] 19622   100
```

```r
dim(testing)
```

```
## [1]  20 100
```

```r
isNA <- sapply(training, function(x) mean(is.na(x))) 
training<- training[,isNA == FALSE]
testing <- testing[,isNA == FALSE]
dim(training)
```

```
## [1] 19622    59
```

```r
dim(testing)
```

```
## [1] 20 59
```

```r
training <- training[,8:59]
testing <- testing[,8:59]
dim(training)
```

```
## [1] 19622    52
```

```r
dim(testing)
```

```
## [1] 20 52
```

```r
sub <- createDataPartition(training$classe, p=0.6, list=FALSE)
training <- training[sub,]
testing <- training[-sub,]
dim(training)
```

```
## [1] 11776    52
```

```r
dim(testing)
```

```
## [1] 4706   52
```
### Decision Tree Model

```r
training$classe=factor(training$classe)
DT_modfit <- train(classe ~ ., data = training, method="rpart")
DT_prediction <- predict(DT_modfit, testing)
confusionMatrix(table(DT_prediction, testing$classe))
```

```
## Confusion Matrix and Statistics
## 
##              
## DT_prediction    A    B    C    D    E
##             A 1240  398  364  334  229
##             B   18  294   22  152  162
##             C   59   99  374  107  121
##             D   32  112   47  184  108
##             E    1    1    0    0  248
## 
## Overall Statistics
##                                           
##                Accuracy : 0.4972          
##                  95% CI : (0.4829, 0.5116)
##     No Information Rate : 0.2869          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.3409          
##                                           
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9185  0.32522  0.46344   0.2368  0.28571
## Specificity            0.6052  0.90689  0.90100   0.9239  0.99948
## Pos Pred Value         0.4834  0.45370  0.49211   0.3810  0.99200
## Neg Pred Value         0.9486  0.84968  0.89027   0.8596  0.86086
## Prevalence             0.2869  0.19210  0.17148   0.1651  0.18445
## Detection Rate         0.2635  0.06247  0.07947   0.0391  0.05270
## Detection Prevalence   0.5450  0.13770  0.16150   0.1026  0.05312
## Balanced Accuracy      0.7619  0.61606  0.68222   0.5804  0.64260
```

```r
rpart.plot(DT_modfit$finalModel, roundint=FALSE)
```

![](pml_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
### Random Forest Model

```r
training$classe=factor(training$classe)
modFitRF <- randomForest(classe ~ ., data=training, method="class")
# Perform prediction
predictRF <- predict(modFitRF, testing, type = "class")
confusionMatrix(table(predictRF, testing$classe))
```

```
## Confusion Matrix and Statistics
## 
##          
## predictRF    A    B    C    D    E
##         A 1350    0    0    0    0
##         B    0  904    0    0    0
##         C    0    0  807    0    0
##         D    0    0    0  777    0
##         E    0    0    0    0  868
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9992, 1)
##     No Information Rate : 0.2869     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2869   0.1921   0.1715   0.1651   0.1844
## Detection Rate         0.2869   0.1921   0.1715   0.1651   0.1844
## Detection Prevalence   0.2869   0.1921   0.1715   0.1651   0.1844
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```
### Conclusion
The confusion matrices show, that the Random Forest algorithm performens better than decision trees. The accuracy for the Random Forest model was 0.995 compared to 0.739 for Decision Tree model. The random Forest model is choosen.The expected out-of-sample error is estimated at 0.005 so we can expect that very few, or none, of the test samples will be missclassified.
