Prediction Assignment Writeup
================
Pavitra Desai
23/10/2021

### Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
possible to collect a large amount of data about personal activity
relatively inexpensively. In this project, our goal will be to use data
from accelerometers on the belt, forearm, arm, and dumbell of 6
participants. They were asked to perform barbell lifts correctly and
incorrectly in 5 different ways.The goal of this project is to predict
the manner in which they did the exercise.  
\#\#\# Requirements

``` r
library(caret)
```

    ## Loading required package: ggplot2

    ## Loading required package: lattice

``` r
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(rpart)
library(rpart.plot)
```

### Downloading data

``` r
trainurl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl  <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(trainurl))
testing <- read.csv(url(testurl))
dim(training)
```

    ## [1] 19622   160

``` r
dim(testing)
```

    ## [1]  20 160

### Cleaning data

``` r
nzv <- nearZeroVar(training)
training <- training[,-nzv]
testing <- testing[,-nzv]
dim(training)
```

    ## [1] 19622   100

``` r
dim(testing)
```

    ## [1]  20 100

``` r
isNA <- sapply(training, function(x) mean(is.na(x))) 
training<- training[,isNA == FALSE]
testing <- testing[,isNA == FALSE]
dim(training)
```

    ## [1] 19622    59

``` r
dim(testing)
```

    ## [1] 20 59

``` r
training <- training[,8:59]
testing <- testing[,8:59]
dim(training)
```

    ## [1] 19622    52

``` r
dim(testing)
```

    ## [1] 20 52

``` r
sub <- createDataPartition(training$classe, p=0.6, list=FALSE)
training <- training[sub,]
testing <- training[-sub,]
dim(training)
```

    ## [1] 11776    52

``` r
dim(testing)
```

    ## [1] 4699   52

### Decision Tree Model

``` r
training$classe=factor(training$classe)
DT_modfit <- train(classe ~ ., data = training, method="rpart")
DT_prediction <- predict(DT_modfit, testing)
confusionMatrix(table(DT_prediction, testing$classe))
```

    ## Confusion Matrix and Statistics
    ## 
    ##              
    ## DT_prediction    A    B    C    D    E
    ##             A 1232  356  361  356  185
    ##             B   18  316   30  135  180
    ##             C   73   96  366  105  113
    ##             D   26  132   65  166  133
    ##             E    0    0    0    0  255
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.4969          
    ##                  95% CI : (0.4825, 0.5113)
    ##     No Information Rate : 0.2871          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.3416          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9133  0.35111  0.44526  0.21785  0.29446
    ## Specificity            0.6245  0.90445  0.90018  0.90958  1.00000
    ## Pos Pred Value         0.4948  0.46539  0.48606  0.31801  1.00000
    ## Neg Pred Value         0.9470  0.85473  0.88444  0.85731  0.86251
    ## Prevalence             0.2871  0.19153  0.17493  0.16216  0.18429
    ## Detection Rate         0.2622  0.06725  0.07789  0.03533  0.05427
    ## Detection Prevalence   0.5299  0.14450  0.16025  0.11109  0.05427
    ## Balanced Accuracy      0.7689  0.62778  0.67272  0.56371  0.64723

``` r
rpart.plot(DT_modfit$finalModel, roundint=FALSE)
```

![](Predict_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> \#\#\#
Random Forest Model

``` r
training$classe=factor(training$classe)
modFitRF <- randomForest(classe ~ ., data=training, method="class")
# Perform prediction
predictRF <- predict(modFitRF, testing, type = "class")
confusionMatrix(table(predictRF, testing$classe))
```

    ## Confusion Matrix and Statistics
    ## 
    ##          
    ## predictRF    A    B    C    D    E
    ##         A 1349    0    0    0    0
    ##         B    0  900    0    0    0
    ##         C    0    0  822    0    0
    ##         D    0    0    0  762    0
    ##         E    0    0    0    0  866
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9992, 1)
    ##     No Information Rate : 0.2871     
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
    ## Prevalence             0.2871   0.1915   0.1749   0.1622   0.1843
    ## Detection Rate         0.2871   0.1915   0.1749   0.1622   0.1843
    ## Detection Prevalence   0.2871   0.1915   0.1749   0.1622   0.1843
    ## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

### Conclusion

The confusion matrices show, that the Random Forest algorithm performens
better than decision trees. The accuracy for the Random Forest model was
\~1 compared to 0.497 for Decision Tree model. The random Forest model
is choosen.
