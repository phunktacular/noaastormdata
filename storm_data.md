# NOAA Storm Data
## JHU-DS Reproducible Research Peer Assesment 2  
This is the second peer assessment required for the JHU Reproducible Research course through Coursera. The project examines data from the National Oceanic and Atmospheric Administrations' severe weather database in order to answer basic questions about storm events. This project uses R throughout.

##Data Processing
###Dataset
The data come from the NOAA National Climatic Data Center Storm Events database, and cover years 1950 to 2011.  
###Prep the workspace
Load required libraries:

```r
wd <- "C:/Users/Kitt/Documents/GitHub/noaastormdata"
if(getwd()!=wd) setwd(wd)
library(data.table)
library(R.utils)
```

```
## Loading required package: R.oo
## Loading required package: R.methodsS3
## R.methodsS3 v1.6.1 (2014-01-04) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.18.0 (2014-02-22) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## 
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## 
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## 
## R.utils v1.34.0 (2014-10-07) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## 
## The following object is masked from 'package:utils':
## 
##     timestamp
## 
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:data.table':
## 
##     between, last
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

###Download the data
Then dowload the data, and load the data into memory:

```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
fn <- "stormdata.csv"
if(!file.exists(fn)) download.file(url, fn, mode="wb")
if(!file.exists(fn)) bunzip2("stormdata.bz2", fn, remove=FALSE)
storm <- read.csv(fn)
dt <- data.table(storm)
rm(storm)
```

###Data Cleaning

Function to convert the separate date and time fields into one POSIXct date and time. The time zone was not included because the data set does not contain standard time zone abbreviations; incorporating the time zones would have involved guessing at most of the values.


```r
# unifTime converts all the weird time entries into a uniform time format
unifTime <- function(char.time){
    if(nchar(char.time) > 4){
        t <- substr(char.time, 0, 5)
    } else if(nchar(char.time) <= 4){
        y <- sprintf("%04s", char.time)
        t <- paste(substr(y, 0, 2), substr(y, 3, 4), sep=":")
    } else {
        break()
    }
    return(t)
}
```


```r
setnames(dt, tolower(names(dt)))
to.keep <- c("state", "evtype", "bgn_date", "bgn_time", "mag", "fatalities", "injuries", "propdmg", "propdmgexp", "cropdmg", "cropdmgexp")
dt1 <- dt[, to.keep, with=FALSE] #new table with only selected columns

dt1[, bgn_date:=as.character(bgn_date)]
dt1[, bgn_time:=as.character(bgn_time)]
dt1[, date:=as.Date(bgn_date, format="%m/%d/%Y")]
dt1[, time:=unifTime(bgn_time), by=1:nrow(dt1)]
```

##Data Analysis
Determine fatalities and injuries by event type and by year.

```r
fatal <- dt1[, sum(fatalities), by=list(evtype, year(date))]
f1 <- fatal[, sum(V1), by=evtype] #summed by event type over all years
f1.max <- f1[which.max(f1$V1)]
f2 <- fatal[, .SD[which.max(V1)], by=year]
f2.max <- count(f2, evtype)
f2.an <- f2[which.max(V1)]

injured <- dt1[, sum(injuries), by=list(evtype, year)]
```

```
## Error in `[.data.table`(dt1, , sum(injuries), by = list(evtype, year)): column or expression 2 of 'by' or 'keyby' is type closure. Do not quote column names. Usage: DT[,sum(colC),by=list(colA,month(colB))]
```

```r
i1 <- injured[, sum(V1), by=evtype] #summed by event type over all years
```

```
## Error in eval(expr, envir, enclos): object 'injured' not found
```

```r
i1.max <- i1[which.max(f1$V1)]
```

```
## Error in eval(expr, envir, enclos): object 'i1' not found
```

```r
i2 <- injured[, .SD[which.max(V1)], by=year]
```

```
## Error in eval(expr, envir, enclos): object 'injured' not found
```

```r
<<<<<<< HEAD
# convert propdmexp and cropdmgexp into multiplicative factors
# adjust propdmg and cropdmg to actual values
# overall
# by year
# worst year
```
##Results
Across the US, which types of events are the most harmful to population health?
###Population Health Impact
####Total over all years
Tornadoes are the deadliest and most injurious adverse weather events, as measured by the total number of people killed and injured. In total, 5633 people were killed by tornadoes between 1950 and 2011. 9.1346\times 10^{4} people were injured by tornadoes during the same time period.
####Annually
Tornadoes were the most deadly weather most, but not all years. For 45 out of the 61 years in the dataset, tornadoes were the deadliest adverse weather condion tracked. Excessive heat was the deadliest weather condition for 8 out fo the 61 years (with just plain 'heat' addiding an additional year), and flash floods were the deadliest for 4 of the years in the dataset.  
The dealiest year in the dataset was 1995, when 687 people died from heat.
###Economic Impact
###Recommendations
=======
i2.max <- count(i2, evtype)
```

```
## Error in group_by_(x, .dots = vars): object 'i2' not found
```

>>>>>>> parent of bb03374... Last night in Raleigh
