# Reproducible Research: Peer Assessment 1

# Objectives:

## 1) Data analysis of Dataset: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
  
  The variables included in this dataset are:
  * steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
  * date: The date on which the measurement was taken in YYYY-MM-DD format
  * interval: Identifier for the 5-minute interval in which measurement was taken

## 2) Write a literate report to answer the questions.
	   
### Loading and preprocessing the data

    Note Ignore the missing values in the dataset.
   

### What is mean total number of steps taken per day?

    1) Make a histogram of the total number of steps taken each day
    2) Calculate and report the mean and median total number of steps taken per day.


### What is the average daily activity pattern?

    1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


### Imputing missing values

    1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    2) Devise a strategy for filling in all of the missing values in the dataset. 
	   The strategy does not need to be sophisticated. 
	   For example: In the document provided I use the mean for that 5-minute interval across all days.
    3) Create a new dataset that is equal to the original dataset but with the missing data filled in.
    4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


### Are there differences in activity patterns between weekdays and weekends?

	Note: Use the dataset with the filled-in missing values for this part.
    1) Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
	2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
	   and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 





# Execution Steps to reproduce the document PA1_template.html:

## Platform and RStudio version used for this assignment:

The analysis and document are produced with 
RStudio Version 0.98.1028 – 
© 2009-2013 RStudio, Inc.

In HP Pavillon g series 
with intel core i3 and 4 Go memories
OS Windows 7 


## R packages required:

* library(knitr)
* library(dplyr)
* library(ggplot2)
* library(lattice)
* library(xtable)


## How to generate the result html

1. Download the dataset https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
2. Ensure you have the analytical data in the file activity.csv under your local directory
3. Download the file markdown file PA1_template.Rmd into your local directory.
4. Open R Console
5. Verify you have all the required libraries above installed, if not please run install.packages(<package_name>) and follow the instructions online.
6. In R Console run:
  > setwd("your local directory")
  > library(knitr)
  > knit2html("PA1_template.Rmd","PA1_template.html")
7. The file PA1_template.html will be generated under your local directory


