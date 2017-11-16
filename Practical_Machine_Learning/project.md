### Setting up files and packages

    fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    download.file(fileUrl, destfile = "./pml_training.csv")
    fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(fileUrl,destfile = "pml_testing.csv")
    lib_names <- list("tidyverse","caret","corrplot","gbm","forecast","rattle","stringr","rebus","rpart")
    lapply(lib_names,library,character.only=TRUE)

### Loading files

(Class A) exactly according to the specification, (Class B) throwing the
elbows to the front, (Class C) lifting the dumbbell only halfway, (Class
D) lowering the dumbbell only halfway and (Class E) throwing the hips to
the front .

    df <- read.csv("pml_training.csv")
    test <- read.csv("pml_testing.csv")

### Cleaning data

First we will clean out variables that is irrelevant to the study such
names, times. We also use near zero variance to weed out more variables.
Lastly, we can substitute features with lots of NA with 0 but it will
create a near zero variance variable. Thus, we will also delete these
features from our dataset.

    ##Irrelevant variables
    pattern <- c("X","user_name","timestamp")
    irrIndex <- str_detect(names(df),pattern=or1(pattern))
    df <- df[,!irrIndex]
    test <- test[,!irrIndex]
    ##Near zero variance
    nzvIndex <- nzv(df,saveMetrics = TRUE)$nzv
    df <- df[,!nzvIndex]
    test <- test[,!nzvIndex]
    ##No NA values
    naIndex <- colSums(is.na(df))!=0
    df <- df[,!naIndex]
    test <- test[,!naIndex]

### Cross Validation

Now we will separate our df into training (0.8) and testing (0.2) sets.
To keep the paper short, we won't resample more than once.

    inTrain <- createDataPartition(df$classe, p=0.8, list=FALSE)
    training <- df[inTrain,]
    validation <- df[-inTrain,];rm(inTrain)

Our Cross Validation setup is as followed: *1. Training set: 15699
observations *2. Validation set: 3923 observations \*3. Test set: 20
observations

### Exploratory Data Analysis

First, we want to get a general feel for how each variable relates to
each other through GGally package.

    corrplot(cor(training[,-54]),tl.cex = 0.5)

![](project_files/figure-markdown_strict/correlation-1.png)

There are not many variables correlate to each other

### Model

We will attemp to fit 3 models using classification tree (since the
classe is a factor and not numerical), random forest, and boosting.
method.  
\#\#\#\#Classification Tree

    modTree <- train(classe~., method="rpart",data=training)
    fancyRpartPlot(modTree$finalModel)

![](project_files/figure-markdown_strict/tree-1.png)

    predTree <- predict(modTree,validation)
    statTree <- c(Accuracy = confusionMatrix(predTree,validation$classe)$overall[[1]],
                  out_of_sample_error = 1-confusionMatrix(predTree,validation$classe)$overall[[1]])
    statTree

    ##            Accuracy out_of_sample_error 
    ##           0.4906959           0.5093041

We can see that classification tree somehow completely miss the Class:D
which brings its accuracy down subtaintially.

### Random Forest

    modForest <- train(classe~., method="rf",data=training)

    ## Warning: package 'randomForest' was built under R version 3.4.1

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:rattle':
    ## 
    ##     importance

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    modForest

    ## Random Forest 
    ## 
    ## 15699 samples
    ##    53 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 15699, 15699, 15699, 15699, 15699, 15699, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.9928258  0.9909216
    ##   27    0.9960192  0.9949631
    ##   53    0.9907014  0.9882340
    ## 
    ## Accuracy was used to select the optimal model using  the largest value.
    ## The final value used for the model was mtry = 27.

    predForest <- predict(modForest,validation)
    statForest <- c(Accuracy = confusionMatrix(predForest,validation$classe)$overall[[1]],
                  out_of_sample_error = 1-confusionMatrix(predForest,validation$classe)$overall[[1]])
    statForest

    ##            Accuracy out_of_sample_error 
    ##        0.9994901861        0.0005098139

We can see the Random forest methods give us almost 100% accuracy.
However, the big trade off is the running speed. It tooks much longer
than classification tree.

### Boosting

Final method is boosting should give almost the same accuracy as random
forest and suffers from the same weakness of running time!

    modBoost <- train(classe~., method="gbm",data=training,verbose=FALSE)

    ## Warning: package 'plyr' was built under R version 3.4.1

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

    modBoost

    ## Stochastic Gradient Boosting 
    ## 
    ## 15699 samples
    ##    53 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 15699, 15699, 15699, 15699, 15699, 15699, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  Accuracy   Kappa    
    ##   1                   50      0.7632812  0.6995517
    ##   1                  100      0.8301188  0.7848408
    ##   1                  150      0.8668507  0.8314411
    ##   2                   50      0.8822459  0.8508587
    ##   2                  100      0.9375659  0.9209950
    ##   2                  150      0.9615854  0.9514002
    ##   3                   50      0.9307717  0.9123670
    ##   3                  100      0.9688609  0.9606051
    ##   3                  150      0.9838809  0.9796113
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## Accuracy was used to select the optimal model using  the largest value.
    ## The final values used for the model were n.trees = 150,
    ##  interaction.depth = 3, shrinkage = 0.1 and n.minobsinnode = 10.

    predBoost <- predict(modBoost,validation)
    statBoost <- c(Accuracy = confusionMatrix(predBoost,validation$classe)$overall[[1]],
                  out_of_sample_error = 1-confusionMatrix(predBoost,validation$classe)$overall[[1]])
    statBoost

    ##            Accuracy out_of_sample_error 
    ##          0.98801937          0.01198063

Our comparison between methods are as followed

    rbind(statTree,statForest,statBoost)

    ##             Accuracy out_of_sample_error
    ## statTree   0.4906959        0.5093041040
    ## statForest 0.9994902        0.0005098139
    ## statBoost  0.9880194        0.0119806271

Based on the accuracy rate, we will choost random forest as the final
model to test

### Test results

    predict(modForest,test)

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E
