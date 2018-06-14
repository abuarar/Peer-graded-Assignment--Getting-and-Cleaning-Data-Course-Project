#Variables in "TidyData_Point5.txt" as requested by assignement
==========
A)-"activity_number","activity_labels" respectivily: 
    1              WALKING
    2              WALKING_UPSTAIRS
    3              WALKING_DOWNSTAIRS
    4              SITTING
    5              STANDING
    6              LAYING
    
B)-"subject_number": one of the 1:30 subject who conducted the experiment.
#Data was grouped by variables: ("activity_number","activity_labels" and "subject_number")respectivily

C)-AVG calculated for variables in the following list:
"AVG: tBodyAcc-mean()-X"                   "AVG: tBodyAcc-mean()-Y"                    "AVG: tBodyAcc-mean()-Z"                   
"AVG: tBodyAcc-std()-X"                    "AVG: tBodyAcc-std()-Y"                     "AVG: tBodyAcc-std()-Z"                     
"AVG: tGravityAcc-mean()-X"                "AVG: tGravityAcc-mean()-Y"                 "AVG: tGravityAcc-mean()-Z"                
"AVG: tGravityAcc-std()-X"                 "AVG: tGravityAcc-std()-Y"                  "AVG: tGravityAcc-std()-Z"                  
"AVG: tBodyAccJerk-mean()-X"               "AVG: tBodyAccJerk-mean()-Y"                "AVG: tBodyAccJerk-mean()-Z"               
"AVG: tBodyAccJerk-std()-X"                "AVG: tBodyAccJerk-std()-Y"                 "AVG: tBodyAccJerk-std()-Z"                 
"AVG: tBodyGyro-mean()-X"                  "AVG: tBodyGyro-mean()-Y"                   "AVG: tBodyGyro-mean()-Z"                  
"AVG: tBodyGyro-std()-X"                   "AVG: tBodyGyro-std()-Y"                    "AVG: tBodyGyro-std()-Z"                    
"AVG: tBodyGyroJerk-mean()-X"              "AVG: tBodyGyroJerk-mean()-Y"               "AVG: tBodyGyroJerk-mean()-Z"              
"AVG: tBodyGyroJerk-std()-X"               "AVG: tBodyGyroJerk-std()-Y"                "AVG: tBodyGyroJerk-std()-Z"                
"AVG: tBodyAccMag-mean()"                  "AVG: tBodyAccMag-std()"                    "AVG: tGravityAccMag-mean()"               
"AVG: tGravityAccMag-std()"                "AVG: tBodyAccJerkMag-mean()"               "AVG: tBodyAccJerkMag-std()"                
"AVG: tBodyGyroMag-mean()"                 "AVG: tBodyGyroMag-std()"                   "AVG: tBodyGyroJerkMag-mean()"             
"AVG: tBodyGyroJerkMag-std()"              "AVG: fBodyAcc-mean()-X"                    "AVG: fBodyAcc-mean()-Y"                    
"AVG: fBodyAcc-mean()-Z"                   "AVG: fBodyAcc-std()-X"                     "AVG: fBodyAcc-std()-Y"                    
"AVG: fBodyAcc-std()-Z"                    "AVG: fBodyAcc-meanFreq()-X"                "AVG: fBodyAcc-meanFreq()-Y"                
"AVG: fBodyAcc-meanFreq()-Z"               "AVG: fBodyAccJerk-mean()-X"                "AVG: fBodyAccJerk-mean()-Y"               
"AVG: fBodyAccJerk-mean()-Z"               "AVG: fBodyAccJerk-std()-X"                 "AVG: fBodyAccJerk-std()-Y"                 
"AVG: fBodyAccJerk-std()-Z"                "AVG: fBodyAccJerk-meanFreq()-X"            "AVG: fBodyAccJerk-meanFreq()-Y"           
"AVG: fBodyAccJerk-meanFreq()-Z"           "AVG: fBodyGyro-mean()-X"                   "AVG: fBodyGyro-mean()-Y"                   
"AVG: fBodyGyro-mean()-Z"                  "AVG: fBodyGyro-std()-X"                    "AVG: fBodyGyro-std()-Y"                   
"AVG: fBodyGyro-std()-Z"                   "AVG: fBodyGyro-meanFreq()-X"               "AVG: fBodyGyro-meanFreq()-Y"               
"AVG: fBodyGyro-meanFreq()-Z"              "AVG: fBodyAccMag-mean()"                   "AVG: fBodyAccMag-std()"                   
"AVG: fBodyAccMag-meanFreq()"              "AVG: fBodyBodyAccJerkMag-mean()"           "AVG: fBodyBodyAccJerkMag-std()"            
"AVG: fBodyBodyAccJerkMag-meanFreq()"      "AVG: fBodyBodyGyroMag-mean()"              "AVG: fBodyBodyGyroMag-std()"              
"AVG: fBodyBodyGyroMag-meanFreq()"         "AVG: fBodyBodyGyroJerkMag-mean()"          "AVG: fBodyBodyGyroJerkMag-std()"           
"AVG: fBodyBodyGyroJerkMag-meanFreq()"     "AVG: angle(tBodyAccMean,gravity)"          "AVG: angle(tBodyAccJerkMean),gravityMean)"
"AVG: angle(tBodyGyroMean,gravityMean)"    "AVG: angle(tBodyGyroJerkMean,gravityMean)" "AVG: angle(X,gravityMean)"                 
"AVG: angle(Y,gravityMean)"                "AVG: angle(Z,gravityMean)" 

The names are well explained (before taking AVG for them) are well explained in the following lines:
Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. 
These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. 
Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. 
Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). 
Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. 
(Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value
std(): Standard deviation
mad(): Median absolute deviation 
max(): Largest value in array
min(): Smallest value in array
sma(): Signal magnitude area
energy(): Energy measure. Sum of the squares divided by the number of values. 
iqr(): Interquartile range 
entropy(): Signal entropy
arCoeff(): Autorregresion coefficients with Burg order equal to 4
correlation(): correlation coefficient between two signals
maxInds(): index of the frequency component with largest magnitude
meanFreq(): Weighted average of the frequency components to obtain a mean frequency
skewness(): skewness of the frequency domain signal 
kurtosis(): kurtosis of the frequency domain signal 
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean
