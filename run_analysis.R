

library("dplyr")

## Get the variable descriptions from the features.txt file and expand the descriptions
getDescriptiveVariableNames<-function(){
  featureLabels<-read.table("UCI HAR DATASET\\features.txt")[,2]
  featureLabels<-sub("\\(",".",featureLabels)
  featureLabels<-sub("\\,","And",featureLabels)
  featureLabels<-sub("\\)","",featureLabels)
  featureLabels<-sub("\\-m","M",featureLabels)
  featureLabels<-sub("\\-std",".StandardDeviation.",featureLabels)
  featureLabels<-sub("\\-bandsEnergyOf-",".EnergyBands.",featureLabels)
  featureLabels<-sub("Gyro",".Gyroscopic.",featureLabels)
  featureLabels<-sub("Mag",".Magnitude.",featureLabels)
  featureLabels<-sub("gravity",".Gravity.",featureLabels)
  featureLabels<-sub("Freq",".Frequency.",featureLabels)
  featureLabels<-sub("acc",".Acceleration.",featureLabels)
  featureLabels<-sub("Acc",".Acceleration.",featureLabels)
  featureLabels<-sub("Body",".Body.",featureLabels)
  featureLabels<-sub("angleOf",".AngleBetween.",featureLabels)
  featureLabels<-sub("\\-",".",featureLabels)
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



## 2) Extracts only the measurements on the mean and standard deviation for each measurement. 

## 5) From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
tidyActivityData<-function(dtMessy){
  dtTidy<-dtMessys
  dtTidy
}

writeTidyData<-function(dtTidy){
  dtMessy<-readData()
  dtTidy<-tidyActivityData(dtMessy())
  write.table(dtTidy,file="tidyData.txt",row.name=FALSE)
  writeCodeBook(dtTidy)
}

writeCodeBook<-function(dt){
  
}

## Please upload the tidy data set created in step 5 of the instructions. 
## Please upload your data set as a txt file created with write.table() using row.name=FALSE
## (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).
