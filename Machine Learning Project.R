#Prep R
library(caret)
library(randomForest)

#Get data from webpage and load CSVs.
#Note that there are #DIV/0s that have to go
trainingAddress<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingAddress<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training<-read.csv(trainingAddress, na.strings=c("#DIV/0!","NA",""))
testing<-read.csv(testingAddress, na.strings=c("#DIV/0!","NA",""))

#Setting a seed for reproducibility
set.seed(111)

#Cut training data into 2 groups: 60% train, 40% test
#This is done for the purpose of cross-validation later.
inTrain<-createDataPartition(training$classe, p=0.6, list=FALSE)
myTraining<-training[inTrain,] 
myTesting<-training[-inTrain,]

#Cleaning the data
#Remove columns that have nearly no variance
NZV <- nearZeroVar(myTraining)
NZV<-c(1,NZV) #Remove the X column so it isn't included in the model 
myTraining<-myTraining[-NZV]

#While not particularly scientific, if the first 10 values of a variable were NA, 
#then I removed the variable.
thenames<-c("amplitude_roll_forearm", "amplitude_pitch_forearm", "var_accel_forearm",
            "max_picth_forearm", "max_yaw_forearm", "min_roll_forearm", "min_pitch_forearm",
            "min_yaw_forearm","kurtosis_roll_forearm", "kurtosis_picth_forearm", 
            "skewness_roll_forearm", "skewness_pitch_forearm", "var_pitch_dumbbell", 
            "avg_yaw_dumbbell", "stddev_yaw_dumbbell", "var_yaw_dumbbell",
            "stddev_roll_dumbbell", "var_roll_dumbbell", "avg_pitch_dumbbell", 
            "stddev_pitch_dumbbell","var_accel_dumbbell", "avg_roll_dumbbell",
            "amplitude_pitch_dumbbell","min_roll_dumbbell", "min_pitch_dumbbell", 
            "min_yaw_dumbbell", "amplitude_roll_dumbbell","skewness_pitch_dumbbell", 
            "max_roll_dumbbell", "max_picth_dumbbell", "max_yaw_dumbbell",
            "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "skewness_roll_dumbbell",
            "min_yaw_arm", "amplitude_pitch_arm", "amplitude_yaw_arm","skewness_pitch_arm",
            "skewness_yaw_arm", "max_picth_arm", "max_yaw_arm", "min_roll_arm", 
            "min_pitch_arm","kurtosis_roll_arm", "kurtosis_picth_arm", "kurtosis_yaw_arm",
            "skewness_roll_arm","var_accel_arm","stddev_pitch_belt", "var_pitch_belt", 
            "avg_yaw_belt", "stddev_yaw_belt", "var_yaw_belt", "var_total_accel_belt", 
            "avg_roll_belt", "stddev_roll_belt", "var_roll_belt", "avg_pitch_belt",
            "min_roll_belt", "min_pitch_belt", "min_yaw_belt", "amplitude_roll_belt", 
            "amplitude_pitch_belt","skewness_roll_belt", "skewness_roll_belt.1", 
            "max_roll_belt", "max_picth_belt", "max_yaw_belt","kurtosis_roll_belt", 
            "kurtosis_picth_belt")

notnames<-!(names(myTraining) %in% thenames)
myTraining<-myTraining[notnames]

#Doing the same for testing data.
myTesting<-myTesting[-NZV]
myTesting<-myTesting[notnames]
testing<-testing[-NZV]
testing<-testing[notnames]

#A little more data fixing to help clean up the Random Forests
testing[5:58]<-as.numeric(as.character(unlist(testing[5:58])))
myTraining[5:57]<-as.numeric(as.character(unlist(myTraining[5:57])))
myTesting[5:57]<-as.numeric(as.character(unlist(myTesting[5:57])))

#The factor in column 4 was messing EVERYTHING up
#This takes that out.
testing<-testing[-4]
myTesting<-myTesting[-4]
myTraining<-myTraining[-4]

#Model it!
model <- randomForest(classe ~. , data=myTraining)
prediction <- predict(model, myTesting, type = "class")

#Earlier, we split the training data into test and training data
#Here, I evaluate the model using test data (from the training data) 
confusionMatrix(prediction, myTesting$classe)
#Here we see a 99.75% accuracy. This would be an out-of-sample
#error rate since this does not use the training data.

#Now we use this on the real testing data.
finalPredict<-predict(model,testing)

#The function below was used to generate the files for submission

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(finalPredict)
