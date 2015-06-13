---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


We will load the activity data captured from the cellphones from file activity.zip. We will cache the dataset captured for future use.
We will change teh date column to a Date field and also transform the data_frame to tbl_df format used by dplyr.


```r
read.zip <- function(zipFile) {
        zipFileDir <- dirname(zipFile)
        setwd(zipFileDir)
        zipdir <- tempfile()
        dir.create(zipdir)
        unzip(zipFile, exdir = zipdir)
        filesInArchive <- list.files(zipdir)
        csvFile <- paste(zipdir, filesInArchive[1], sep = "/")
        read.csv(csvFile, header = T)
}

activityData <- read.zip("~/Documents/Coursera/ReproducibleResearch/RepData_PeerAssessment1/activity.zip")
dim(activityData)
```

```
## [1] 17568     3
```

```r
colnames(activityData)
```

```
## [1] "steps"    "date"     "interval"
```

```r
library(dplyr, quietly = TRUE)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activityData <- tbl_df(activityData)
activityData <- mutate(activityData, date = as.Date(date))
activityData
```

```
## Source: local data frame [17,568 x 3]
## 
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## ..   ...        ...      ...
```

## What is mean total number of steps taken per day?

Now, we will try to explore the data to calculate the daily total steps taken by the person. The following is the code.


```r
library(dplyr, quietly = TRUE)
dailySteps <- activityData %>%
        group_by(date) %>%
        summarize(totalSteps = sum(steps, na.rm = T))
dailySteps
```

```
## Source: local data frame [61 x 2]
## 
##          date totalSteps
## 1  2012-10-01          0
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08          0
## 9  2012-10-09      12811
## 10 2012-10-10       9900
## ..        ...        ...
```

```r
noOfDays <- dim(dailySteps)[1]
noOfDays
```

```
## [1] 61
```

Let us discover the person's daily steps through some basic statistical summaries.


```r
summary(dailySteps$totalSteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

Now, we will plot this daily steps on a histogram to see discover the frequency of number of steps taken over the 61 day period.


```r
printHistogram <- function() {
        par(mar = c(5, 4, 1, 1))
        with (
                dailySteps, 
                hist(totalSteps, main = "Histogram of total number of steps taken each day", breaks = 15, xlab = "Steps per day")
        )
}
printHistogram()
```

![plot of chunk dailyStepsHistogram](figure/dailyStepsHistogram-1.png) 

Let us also look at the daily steps over the 61 day period with the help of a bar plot.


```r
printBarPlot <- function() {
        par(mar = c(8, 4, 1, 1))
        nolabels <- dim(dailySteps)[1]
        datelabels <- rep(NA, nolabels)
        datelabels[seq(1,nolabels,3)] <- strftime(dailySteps$date[seq(1,nolabels,3)], "%Y-%m-%d")
        datelabels <- as.Date(datelabels, "%Y-%m-%d")
        barplot(dailySteps$totalSteps, space = 1, las = 2, names.arg = datelabels)
}
printBarPlot()
```

![plot of chunk dailyStepsBarPlot](figure/dailyStepsBarPlot-1.png) 

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
