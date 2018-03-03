# PracticalMachineLearning
This is an assignment given from Coursera and John Hopkins Practical Machine Learning course.

Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

Data
The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

What you should submit
The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

Peer Review Portion
Your submission for the Peer Review portion should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).

Course Project Prediction Quiz Portion
Apply your machine learning algorithm to the 20 test cases available in the test data above and submit your predictions in appropriate format to the Course Project Prediction Quiz for automated grading.

Reproducibility
Due to security concerns with the exchange of R code, your code will not be run during the evaluation by your classmates. Please be sure that if they download the repo, they will be able to view the compiled HTML version of your analysis.

# Code


library(caret)
library(randomForest)
library(ISLR)


# load data sets replace missing values with NA
trainingSet <- read.csv("C:/Users/Cat/Documents/pml-training.csv", na.strings = c("NA","#DIV/0!",""))
testingSet <- read.csv("C:/Users/Cat/Documents/pml-testing.csv", na.strings = c("NA","#DIV/0!",""))

# remove columns with all missing data
trainingSet <- trainingSet[,colSums(is.na(trainingSet))==0]
testingSet <- testingSet[,colSums(is.na(trainingSet))==0]

# remove unnessesary columns 
trainingSet <- trainingSet[,-c(1:7)]
testingSet <- testingSet[,-c(1:7)]

# separate the training set file into anther training and test set to prevent overfitting to the original file
# 70% of data will be in the training subset and the rest will be in a testing subset
intrain <- createDataPartition(y= trainingSet$classe, p=0.7, list=FALSE)

training <- trainingSet[intrain,]
testing <- trainingSet[-intrain,]

# Use random forest method on the trainng data 
modeltrain <- randomForest(classe ~., data=training, method = "class")

prediction <- predict(modeltrain, training, type="class")

confusionMatrix(prediction, training$classe)

# confusion Maxtirx shown below shows that there is a 95% acuracy using randomForest
# I am happy with the 95% accuracy so I continued using the randomforest method

Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 3906    0    0    0    0
         B    0 2658    0    0    0
         C    0    0 2396    0    0
         D    0    0    0 2252    0
         E    0    0    0    0 2525

Overall Statistics
                                     
               Accuracy : 1          
                 95% CI : (0.9997, 1)
    No Information Rate : 0.2843     
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
 Mcnemar's Test P-Value : NA         

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000


predictionfinal <- predict(modeltrain, testingSet, type="class")

predictionfinal

# Here is the final prodiction using the RandomForest method
 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
 B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
Levels: A B C D E

#Write those results to text files 
pml_write_files = function(x){
  n=length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
  write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}
pml_write_files(predictionfinal)
