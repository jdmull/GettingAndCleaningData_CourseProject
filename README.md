GettingAndCleaningData_CourseProject
====================================

Creating a tidy summerizing dataset from a collection of university datasets on wearable accelerometer measurements, for implementation details please see run_analysis.r.

run_analysis.r
    readData() - Merges the training and the test sets to create one data set.
    readData() - Extracts only the measurements on the mean and standard deviation for each measurement. 
    readData() - Uses descriptive activity names to name the activities in the data set
    getDescriptiveNames() Appropriately labels the read data set with descriptive variable names. 

    writeTidyData() tidySummaryTable() - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

code book.txt 
   contains column names & data types, number of rows

Subjective decisions:
  Treating measurement types as a variable:  for what I am using this dataset Jerk & Acceleration are pretty interchangeable.  They are some generic measurement which I do not need details for once their the means were computed for their values.  This is why I am treating what were seperate columns in the original dataset as a named variable in the tidy dataset, for my uses they are really not seperate measurements.

  Removing columns with angle of <something> with mean <something>.  This seems to be an angle between a measurement and another meaurement with a mean type in it, eg this seems to be an angle instead of a mean measurement, so these columns were removed from the raw data as part of step 2.

