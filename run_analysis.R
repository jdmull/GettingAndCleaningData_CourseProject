

library("dplyr")

## Get the variable descriptions from the features.txt file and expand the descriptions
getDescriptiveVariableNames<-function(){
  featureLabels<-read.table("UCI HAR DATASET\\features.txt")[,2]
  featureLabels<-sub("\\(","",featureLabels)
  featureLabels<-sub("\\,","And",featureLabels)
  featureLabels<-sub("\\)","",featureLabels)
  featureLabels<-sub("\\-m","M",featureLabels)
  featureLabels<-sub("\\-std","StandardDeviation",featureLabels)
  featureLabels<-sub("\\-bandsEnergyOf-","\\.EnergyBands.",featureLabels)
  featureLabels<-sub("Gyro","Gyroscopic",featureLabels)
  featureLabels<-sub("Mag","Magnitude",featureLabels)
  featureLabels<-sub("gravity","Gravity",featureLabels)
  featureLabels<-sub("Freq","Frequency",featureLabels)
  featureLabels<-sub("acc","Acceleration.",featureLabels)
  featureLabels<-sub("Acc","Acceleration",featureLabels)
  featureLabels<-sub("angleOf","AngleBetween",featureLabels)
  featureLabels<-sub("\\-","",featureLabels)
  featureLabels<-sub("\\..","\\.",featureLabels)
  featureLabels<-sub("\\..","\\.",featureLabels)
  featureLabels<-sub("\\..","\\.",featureLabels)
  featureLabels<-sub("\\..","\\.",featureLabels)  
  featureLabels
}

## Read in the raw data tables returning a combined data table
## Also included at this stage is:
##     Merges the training and the test sets to create one data set. (1)
##     Uses descriptive activity names to name the activities in the data set (3)
##     Appropriately labels the data set with descriptive variable names. (4)
readData<-function()
{
    ## get the descriptive labels for the activities listed in the Y_*.txt files (4)
    dtActivityDescriptiveLabels<-read.table("UCI HAR DATASET\\activity_labels.txt",
                                            col.names=c("level","activityName"))
    descriptiveVariableNames <- getDescriptiveVariableNames()
    
    ## Combine the tables within a test or train directory into one table
    dtTablesCombinedFromDirectory<-function(testOrTrain){

        ## format filename string for files in the test or train folders
        sfName<-function(prefix){  
            paste0("UCI HAR DATASET\\",testOrTrain,"\\",prefix,testOrTrain,".txt")
        }  
    
        ## reading in activity names and subject ID
        dtY<-read.table(sfName("Y_"),col.names=c("activity"))
        dtY<-mutate(dtY,activity,description=dtActivityDescriptiveLabels[activity,"activityName"]) ## (3)  
        dtSubject<-read.table(sfName("subject_"),col.names=c("subjectID"))

        ## Read in X, assigning descriptive variable names to the columns (4)
        dtX<-read.table(sfName("X_"), col.names=descriptiveVariableNames)
        ## Extract only the measurements for the mean and standard deviation on each measurement (2)
        dtX<-select(dtX,contains("StandardDeviation"),contains("Mean")) 
        ##     not selecting an angle measurement to a std or mean just std or mean meansurments
        dtX<-select(dtX,1:79)
        
        ## the Y & Subject ID tables had a 1 to 1 match with the rows in the X, so can directly assign the column
        dtX["activity"]<-dtY$description ## names of the activities from the Y and activity_labels tables (3)
        dtX["subjectID"]<-dtSubject$subjectID ## subject ID from the subject table
        dtX ## returning the combined table
    }
    
    ## Merges the training and the test sets to create one data set. (1)
    dtTrain<-dtTablesCombinedFromDirectory("train")
    dtTest<-dtTablesCombinedFromDirectory("test")
    dtMergedRows<-rbind(dtTrain,dtTest)
    dtMergedRows
}

## 5) From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
tidySummaryTable<-function(){
  dt<-readData()
  newTable<-data.frame()
  
  ## Getting subtotals
  library("dplyr")
  library("tidyr")
  dt<-group_by(dt,activity,subjectID)
  tidyData<-rbind(
    gather(summarise(dt,mean(tBodyAccelerationStandardDeviationX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationStandardDeviationY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationStandardDeviationZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tGravityAccelerationStandardDeviationX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tGravityAccelerationStandardDeviationY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tGravityAccelerationStandardDeviationZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationJerkStandardDeviationX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationJerkStandardDeviationY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationJerkStandardDeviationZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicStandardDeviationX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicStandardDeviationY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicStandardDeviationZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicJerkStandardDeviationX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicJerkStandardDeviationY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicJerkStandardDeviationZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationMagnitudeStandardDeviation)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tGravityAccelerationMagnitudeStandardDeviation)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationJerkMagnitudeStandardDeviation)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicMagnitudeStandardDeviation)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicJerkMagnitudeStandardDeviation)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationStandardDeviationX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationStandardDeviationY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationStandardDeviationZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationJerkStandardDeviationX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationJerkStandardDeviationY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationJerkStandardDeviationZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyGyroscopicStandardDeviationX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyGyroscopicStandardDeviationY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyGyroscopicStandardDeviationZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationMagnitudeStandardDeviation)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyBodyAccelerationJerkMagnitudeStandardDeviation)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyBodyGyroscopicMagnitudeStandardDeviation)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyBodyGyroscopicJerkMagnitudeStandardDeviation)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationMeanX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationMeanY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationMeanZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tGravityAccelerationMeanX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tGravityAccelerationMeanY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tGravityAccelerationMeanZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationJerkMeanX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationJerkMeanY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationJerkMeanZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicMeanX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicMeanY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicMeanZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicJerkMeanX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicJerkMeanY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicJerkMeanZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationMagnitudeMean)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tGravityAccelerationMagnitudeMean)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyAccelerationJerkMagnitudeMean)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicMagnitudeMean)),metricSourceName,mean,3),
  gather(summarise(dt,mean(tBodyGyroscopicJerkMagnitudeMean)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationMeanX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationMeanY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationMeanZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationMeanFrequencyX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationMeanFrequencyY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationMeanFrequencyZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationJerkMeanX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationJerkMeanY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationJerkMeanZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationJerkMeanFrequencyX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationJerkMeanFrequencyY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationJerkMeanFrequencyZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyGyroscopicMeanX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyGyroscopicMeanY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyGyroscopicMeanZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyGyroscopicMeanFrequencyX)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyGyroscopicMeanFrequencyY)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyGyroscopicMeanFrequencyZ)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationMagnitudeMean)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyAccelerationMagnitudeMeanFrequency)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyBodyAccelerationJerkMagnitudeMean)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyBodyAccelerationJerkMagnitudeMeanFrequency)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyBodyGyroscopicMagnitudeMean)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyBodyGyroscopicMagnitudeMeanFrequency)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyBodyGyroscopicJerkMagnitudeMean)),metricSourceName,mean,3),
  gather(summarise(dt,mean(fBodyBodyGyroscopicJerkMagnitudeMeanFrequency)),metricSourceName,mean,3) )
  tidyData
}  

writeTidyData<-function(dtTidy){
  dtTidy<-tidySummaryTable()
  write.table(dtTidy,file="tidyData.txt",row.name=FALSE)
}


## Please upload the tidy data set created in step 5 of the instructions. 
## Please upload your data set as a txt file created with write.table() using row.name=FALSE
## (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).
