---
output:
  html_document:
    keep_md: yes
---
# NOAA Storm Data
## JHU-DS Reproducible Research Peer Assesment 2  
This is the second peer assessment required for the JHU Reproducible Research course through Coursera. The project examines data from the National Oceanic and Atmospheric Administrations' severe weather database in order to answer basic questions about storm events. This project uses R throughout.

##Data Processing
###Dataset
The data come from the NOAA National Climatic Data Center Storm Events database, and cover years 1950 to 2011.  
###Prep the workspace
Load required libraries:

```r
library(data.table)
library(R.utils)
library(ggplot2)
library(dplyr)
```

###Download the data
Then dowload the data, and load the data into memory:

```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
fn <- "stormdata.csv"
if(!file.exists(fn)) download.file(url, fn, mode="wb")
if(!file.exists(fn)) bunzip2("stormdata.bz2", fn, remove=FALSE)
storm <- read.csv(fn)
dt1 <- data.table(storm)
rm(storm)
```

###Data Munging

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
setnames(dt1, tolower(names(dt1)))
to.keep <- c("state", "evtype", "bgn_date", "bgn_time", "mag", "fatalities", "injuries", "propdmg", "propdmgexp", "cropdmg", "cropdmgexp")
dt1 <- dt1[, to.keep, with=FALSE] #new table with only selected columns

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
i2.max <- count(i2, evtype)
```

```
## Error in group_by_(x, .dots = vars): object 'i2' not found
```

