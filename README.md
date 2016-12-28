# getting_and_cleaning_data peer assignment:
Coursera Getting and Cleaning Data Course Assignment

This read-me describes the data and overview of the transformations and actions that have been applied to the data set in the process of creating a tidy data set.
The Data Source

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
Original description of the data

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.
The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Attribute Information

For each record in the dataset it is provided:
•	Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
•	Triaxial Angular velocity from the gyroscope.
•	A 561-feature vector with time and frequency domain variables.
•	Its activity label.
•	An identifier of the subject who carried out the experiment.

Transformation Details

There are 5 activities that are being undertaken:
1.	Merges the training and the test sets to create one data set
2.	Extract the measurements on the mean and standard deviation for each measurement
3.	Use descriptive activity names to name the activities in the data set
4.	Label the data set with descriptive variable names
5.	From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject

How run_analysis.R executes the above transformations

Libraries Used

The libraries used in the R Script are data.table and dplyr and the library is loaded at the start of the script

Loading of Supporting Metadata

The metadata is loaded into variables featurenames and activities and are referenced later in the script to update the data labels

Reading Training and Test Data

The data for the training and test sets are split up into three specific sections being subject, activity and features. Each of these are in different folders and files.
The data is then loaded into files for each of the sections based on Training and Test and then this is merged into a single data file for each section.
In the merged file the columns are named using the meta data that was previously loaded and is contained in the variable featurenames and merged into a single file named complete
Extract measurement for Means and Standard Deviation
The use of the grep command is used to filter columns that contain Mean or Std. The extract variable is defined that consists of complete data information.

Assigning Descriptive Names

Through the use of the gsub command to replace assigned names with more descriptive and friendly names to make the data set easy to use and in preparation for exporting as a tidy data set.

Export data to new Data Set

A variable is created called tidy as a data set with average for each activity and subject. Then the entries are ordered and then written out as data file tidy.txt that contains the processed data.

