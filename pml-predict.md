Practical Machine Learning - Prediction Assignment Writeup
==========================================================

Introduction
------------
The objective of this assignment is to build a predictive model of the manner in which barbell lifts exercices are performed.  
Data from acceleromoters on the belt, forearm, arm and dumbell were recorded and associated with an varible "classe" which is the outcome to predict.  
Data and informations are available from this website http://groupware.les.inf.puc-rio.br/har

Packages used
-------------

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(randomForest)
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```


Reading the training data
-------------------------
The training data are downloaded directly from the web url provided by Coursera.  
All variables with an empty string are considered as NA.

```r
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url, destfile = "trainingdata.csv", method = "curl", quiet = TRUE)
trainingdata <- read.csv("trainingdata.csv", stringsAsFactors = FALSE, , header = TRUE, 
    sep = ",", na.string = c("NA", ""))
```


Processing the training data
----------------------------
The "classe" variable is the outcome to prectict, and is converted to a factor variable

```r
trainingdata$classe <- as.factor(trainingdata$classe)
```


All features with NA are removed

```r
NAcols <- apply(trainingdata, 2, function(x) {
    sum(is.na(x))
})
trainingdata <- trainingdata[, which(NAcols == 0)]
```


The 7 fist columns are removed. These features are not directly related to sensors activity.

```r
names(trainingdata[, 1:7])
```

```
## [1] "X"                    "user_name"            "raw_timestamp_part_1"
## [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
## [7] "num_window"
```

```r
trainingdata <- trainingdata[, 8:60]
```


Splitting the training set
--------------------------
The training set is splitted in two parts :  
1. The training set (70% of the original data set). It will be used to train the model.  
2. The cross-validation set (30% of the original data set). It will be used to check the model and to estimate the out-of-sample error rate.

```r
inTrain <- createDataPartition(y = trainingdata$classe, p = 0.7, list = FALSE)
training <- trainingdata[inTrain, ]
crosstesting <- trainingdata[-inTrain, ]
```


Training the model with random forest
-------------------------------------

```r
modelRF <- train(classe ~ ., data = training, method = "rf", allowParallel = T)
modelRF
```

```
## Random Forest 
## 
## 13737 samples
##    52 predictors
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##   2     1         1      0.002        0.002   
##   30    1         1      0.002        0.002   
##   50    1         1      0.004        0.005   
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

calculating the in sample error rate of the final model

```r
in_error_rate <- 1 - (sum(predict(modelRF, training) == training$classe)/dim(training)[1])
paste("The in sample error rate is", in_error_rate)
```

```
## [1] "The in sample error rate is 0"
```


Using the trained model on the cross validation set
---------------------------------------------------

```r
pred <- predict(modelRF, crosstesting)
table(pred, crosstesting$classe)
```

```
##     
## pred    A    B    C    D    E
##    A 1673    6    0    0    0
##    B    1 1128   14    2    0
##    C    0    4 1006   10    0
##    D    0    1    6  952    1
##    E    0    0    0    0 1081
```

```r
out_error_rate <- 1 - (sum(pred == crosstesting$classe)/dim(crosstesting)[1])
paste("The out of sample error rate is", out_error_rate)
```

```
## [1] "The out of sample error rate is 0.00764655904842826"
```


For the submission part of the assignment, this method predicted the correct answer for the 20 requested submission.
