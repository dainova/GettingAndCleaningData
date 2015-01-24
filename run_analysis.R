
#  https://github.com/dainova
# Wrap process as a function with sinlge paramete = Directory
# Check if source dir exists, download /create if needed
# Load training and test data 
# Get needed columns for activity
# Write output to csv files.
# use plyr



run_analysis <- function(directory = "UCI HAR Dataset") {

##  Check dplyr package installed?
    mypkg <- "dplyr"
    is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
    is.installed(mypkg)
  
    if (!is.installed(mypkg)){
      install.packages(mypkg)
    }
    library(mypkg)


##  check if source data is loaded
    directory <- "UCI HAR Dataset"
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    if (!file.exists(directory)) {
      if (!file.exists("data")) {
        dir.create("data")
      }
      download.file(fileUrl, destfile="data/har.zip", method="curl")
      unzip("data/har.zip", exdir="./")
    } 
          # getwd()
          # dir()
    
    

file.feature <- paste(directory, "/features.txt", sep = "")
file.label.activity <- paste(directory, "/labels.activityx.txt", sep = "")
features <- read.table(file.feature, colClasses = c("character"))
labels.activity <- read.table(file.label.activity, col.names = c("ActivityId", "Activity"))


## process  train data 
directory <- "UCI\ HAR\ Dataset" 
file.x.train <- paste(directory, "/train/X_train.txt", sep = "")
file.y.train <- paste(directory, "/train/y_train.txt", sep = "")
file.train.subj <- paste(directory, "/train/subject_train.txt", sep = "")

## process  test data 
file.x.test  <- paste(directory, "/test/X_test.txt", sep = "")
file.y.test  <- paste(directory, "/test/y_test.txt", sep = "")
file.test.subj <- paste(directory, "/test/subject_test.txt", sep = "")

## read data into table 

x.train <- read.table(file.x.train)
y.train <- read.table(file.y.train)
subject.train <- read.table(file.train.subj)
x_test <- read.table(file.x.test)
y.test <- read.table(file.y.test)
subject_test <- read.table(file.test.subj)


##  Merge test and training together

training_sensor_data <- cbind(cbind(x.train, subject_train), y.train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y.test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels


##  Get only mean + stg columns

tidy.data.mean <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

##   Attach readable labels, clean names.

tidy.data.mean <- join(tidy.data.mean, labels.activity, by = "ActivityId", match = "first")
tidy.data.mean <- tidy.data.mean[,-1]

##getwd()

names(tidy.data.mean) <- gsub('\\(|\\)',"",names(tidy.data.mean), perl = TRUE)

names(tidy.data.mean) <- make.names(names(tidy.data.mean))

names(tidy.data.mean) <- gsub('Acc',"Acceleration",names(tidy.data.mean))
names(tidy.data.mean) <- gsub('GyroJerk',"AngularAcceleration",names(tidy.data.mean))
names(tidy.data.mean) <- gsub('Gyro',"AngularSpeed",names(tidy.data.mean))
names(tidy.data.mean) <- gsub('Mag',"Magnitude",names(tidy.data.mean))
names(tidy.data.mean) <- gsub('^t',"TimeDomain.",names(tidy.data.mean))

names(tidy.data.mean) <- gsub('^f',"FrequencyDomain.",names(tidy.data.mean))
names(tidy.data.mean) <- gsub('\\.mean',".Mean",names(tidy.data.mean))
names(tidy.data.mean) <- gsub('\\.std',".StandardDeviation",names(tidy.data.mean))
names(tidy.data.mean) <- gsub('Freq\\.',"Frequency.",names(tidy.data.mean))
names(tidy.data.mean) <- gsub('Freq$',"Frequency",names(tidy.data.mean))

# Write output


tidy.data.avg = ddply(tidy.data.mean, c("Subject","Activity"), numcolwise(mean))
write.table(tidy.data.avg, file = "TidyDataAvg.txt")



