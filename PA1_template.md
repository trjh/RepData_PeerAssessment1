# Reproducible Research: Peer Assessment 1
### Analysis of an individual's activity monitoring data 

<!-- note to self: http://rmarkdown.rstudio.com/
    If you are not using RStudio then you simply need to call the
    rmarkdown::render function, for example:

    rmarkdown::render("input.Rmd")

    Note that in the case using the "Knit" button in RStudio the basic
    mechanism is the same (RStudio calls the rmarkdown::render function under
    the hood)

    ...but this requires pandoc:
    > rmarkdown::render("PA1_template.Rmd")
    Error: pandoc version 1.12.3 or higher is required and was not found.

    ...which in turn requires either a lot of Haskell, or RStudio:
    http://r.789695.n4.nabble.com/Apply-rmarkdown-render-outside-the-RStudio-don-t-find-pandoc-td4696190.html
    https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md
-->

## Loading and preprocessing the data

> Show any code that is needed to
> 
> 1. Load the data (i.e. `read.csv()`)
> 
> 1. Process/transform the data (if necessary) into a format suitable for your analysis

The following code will unzip the data file (if neccesary), read it in to
memory, and place the data into a dplyr data frame table.

We may not need it, but for now let's also put the date column into datetime
format with lubridate.


```r
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(xtable)
library(ggplot2)

## numbers >= 10^6 (4+2) will be denoted in scientific notation,
## and rounded to 2 digits
## http://yihui.name/knitr/demo/output/
## useful for when we print mean & median inline in the next section
options(scipen = 2, digits = 2)

filename="activity.csv"
if(!file.exists(filename)) {
    unzip("activity.zip")
}
if(!file.exists(filename)) {
    stop("Cannot find ",filename," file")
}

activity <- read.csv("activity.csv")

activity <- tbl_df(activity) %>%
    transmute(steps, date = ymd(date), interval)

summary(activity)
```

```
##      steps           date               interval   
##  Min.   :  0    Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0    1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0    Median :2012-10-31   Median :1178  
##  Mean   : 37    Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12    3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806    Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

&nbsp;  <!-- I need more space here than markdown will give me by default! -->


## What is the mean total number of steps taken per day?

> For this part of the assignment, you can ignore the missing values in the dataset.
>
> 1. Calculate the total number of steps taken per day
> 
> 1. Make a histogram of the total number of steps taken each day
> 
> 1. Calculate and report the mean and median of the total number of steps taken per day
  
Now we'll create a table that sums up the number of steps per day.  dplyr
makes this very easy.

We won't remove the missing values from the dataset at this stage, as it makes
it impossible to distinguish between days when we don't have data, and days
when the subject appears to have stayed completely stationary.


```r
dailysummary <- activity %>%
                group_by(date) %>%
                summarize(steps = sum(steps))

head(dailysummary,3)
```

```
## Source: local data frame [3 x 2]
## 
##         date steps
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
```


We'll now create a histogram[^hist] of the number of steps taken per day.
This shows us that the number of steps per day generally resembles a bell
curve, with the mean lying just above the 10,000 steps mark -- however, there
are at least a couple of days where it would appear that the subject did not
move at all.  (Though perhaps, charitabily, we can assume that the subject
left their step-measuring device behind for a day.)

*TODO better labels*

```r
with(dailysummary, hist(steps,30,main = "Histogram of Steps per Day"))
```

![](PA1_template_files/figure-html/dailysummary_hist-1.png) 

Finally, we can see that the mean and median number of steps are nearly identical:


```r
meandaily_steps   <- mean(dailysummary$steps, na.rm = TRUE)
mediandaily_steps <- median(dailysummary$steps, na.rm = TRUE)
```

The mean of the total steps per day is 10766.19, and the median is
10765.

[^hist]: I tend to prefer ggplot2 graphs, but the base plot system seems to
have the cleanest histograms, at least by default.

&nbsp;  <!-- I need more space here than markdown will give me by default! -->


## What is the average daily activity pattern?

> 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
> 1. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

There may be a way to do this directly via `qplot()` or `ggplot()` arguments, but
let's keep things simple and make ourselves another summary table.  With this
table, creating the time series plot is easy.


```r
intervalsummary <- activity %>%
                    group_by(interval) %>%
                    summarize(meansteps = mean(steps, na.rm = TRUE), medsteps = median(steps, na.rm = TRUE))
# with(intervalsummary, plot(interval, steps, type = "l"))

qplot(interval, meansteps, geom = "line", data = intervalsummary)
```

![](PA1_template_files/figure-html/intervalsummary-1.png) 

The following code shows us that the individual averages the most steps during the interval corresponding to 8:35 in the morning.


```r
filter(intervalsummary, meansteps == max(meansteps))
```

```
## Source: local data frame [1 x 3]
## 
##   interval meansteps medsteps
## 1      835       206       19
```

&nbsp;  <!-- I need more space here than markdown will give me by default! -->


## Imputing missing values
> Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
>
> 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
> 1. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
> 1. Create a new dataset that is equal to the original dataset but with the missing data filled in.
> 1. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#### Number of missing values in the dataset


```r
missing <- nrow(filter(activity, is.na(steps)))
missingpercentage <- format((missing / nrow(activity)) * 100, digits = 4)
```

As calculated from the previous code, there are 2304 rows missing from the table, representing 13.11% of the total rows.  We'll notice later that the dates either have no intervals with `NA` values, or all intervals have `NA` values.  So if we divide the number of missing rows by 288 (24 hours x 60 minutes / 5 minute intervals) we know that we are missing exactly 8 days worth of data.  This might bias our calculations or summaries if the missing days are unduly skewed towards the weekend or weekday.  A quick look at that here:


```r
activity %>% filter(is.na(steps)) %>% select(date) %>% unique %>% as.vector %>% sapply(weekdays) %>% table
```

```
## .
##    Friday    Monday  Saturday    Sunday  Thursday Wednesday 
##         2         2         1         1         1         1
```

We can see that 6/8 of our missing data are in the range Monday to Friday.  This is exactly the proportion of week days we'd expect for 8 missing days -- five-sevenths times eight is 5.71.

#### Divising a strategy for filling in the missing values

The easiest & most reasonable strategy, to my mind, is to fill any missing
interval with the mean for the appropriate 5-minute interval.  However, it
takes a bit of the fun out of divising if that strategy is drectly in the
question.  A second-order approximation would combine the two suggestions,
using any data available for a day with missing values to help compute an
appropriate in-fill value.

Unfortunately, as the following code snippet shows, the missing values in this
data set span entire dates at a time -- there are no dates with only partial
missing values.


```r
missingdata_by_date <- activity %>%
                        transform(na = is.na(steps)) %>%
                        group_by(date) %>%
                        summarize(NAs = sum(na))

missingdata_table <- table(missingdata_by_date$NAs)
# make the results a little clearer
names(missingdata_table) <- c("Days missing 0 values", "Days missing 288 values")
missingdata_table
```

```
##   Days missing 0 values Days missing 288 values 
##                      53                       8
```

So we don't have any indication from the date itself as to the level of
activity.  Any intelligent attempt to fill in the values for the day would
have to base variance from patterns based on day of the week, day of the
month, surrounding days activity, etc.

As I haven't got weeks to spend on this assignment, I'll just fill in any NA
value for a given interval with the median number of steps for that interval
as measured across the entire data set.  Still, let's make sure that adding up
all the median-interval-step-values doesn't result in a daily step total that
varies from the average step value for a day.

*TODO nicer output*

```r
intervalsmean_sum = sum(intervalsummary$meansteps)
intervalsmedian_sum = sum(intervalsummary$medsteps)
mediandaily_steps - intervalsmedian_sum
```

```
## [1] 9624
```

```r
mediandaily_steps - intervalsmean_sum
```

```
## [1] -1.2
```

Clearly I don't want to use the median value of the steps across all values of
interval X.  I'll use the average value, but also `round()` the average, as a
fractional number of steps doesn't make much sense. 

#### Create a new data set with the missing data filled in

So here we create a short
function to replace any NA values -- it requires the steps value and the
interval value as input.  We then use `mapply()` to pass the steps and
interval values to this function in order to build ourselves a new steps
column that imputes missing values.  Finally, we use data.frame and tbl_df to
re-assemble an activity table in dplyr data frame format.


```r
complete_steps <- function(steps, interval) {
    newsteps <- steps
    if (is.na(steps)) {
        newsteps <- round(intervalsummary[intervalsummary$interval==interval,]$meansteps)    
    }
    newsteps
}

newstepscol <- with(activity, mapply(complete_steps, steps, interval))
newactivity <- tbl_df(data.frame(steps = newstepscol, date = activity$date, interval = activity$interval))
head(newactivity)
```

```
## Source: local data frame [6 x 3]
## 
##   steps       date interval
## 1     2 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```

#### Histogram, mean, median, and discussion of new data set

Now we make a histogram, and compute the mean and median of the new set.  I
don't expect the mean & median to change, though perhaps the histogram will
change slightly, increasing the peak at the middle.

We'll put the new and old mean & median values into a table.  Because it's no
fun to learn stuff on the slides and not use ALL of it!

*TODO lattice plot, less squished*

*TODO spacing between xtable and the text*


```r
ndailysummary <- newactivity %>%
		 group_by(date) %>%
		 summarize(steps = sum(steps))

with(ndailysummary, hist(steps,30))
```

![](PA1_template_files/figure-html/interpolate1_effects-1.png) 

```r
nmeandaily_steps   <- mean(ndailysummary$steps, na.rm = TRUE)
nmediandaily_steps <- median(ndailysummary$steps, na.rm = TRUE)

results <- c(meandaily_steps, mediandaily_steps,
	     nmeandaily_steps, nmediandaily_steps)
dim(results) <- c(2,2)
rownames(results) <- c("orig data", "infilled data")
colnames(results) <- c("mean", "median")
print(xtable(results, center=c("r","r")), type = "html", html.table.attributes="border=0")
```

<!-- html table generated in R 3.1.2 by xtable 1.7-4 package -->
<!-- Sun Mar 15 14:54:46 2015 -->
<table border=0>
<tr> <th>  </th> <th> mean </th> <th> median </th>  </tr>
  <tr> <td align="right"> orig data </td> <td align="right"> 10766.19 </td> <td align="right"> 10765.64 </td> </tr>
  <tr> <td align="right"> infilled data </td> <td align="right"> 10765.00 </td> <td align="right"> 10762.00 </td> </tr>
   </table>

We can see that *these values differ* **only slightly** *from the estimates from the first part of the assignment.*  However, we can see from the histogram that my method of imputing missing data greatly skews the histogram, making it appear that the individual walks *exactly* the average number of steps per day much more often than they probably do in relality.

However, this method *does not alter the estimate of the total number of daily steps*.  The original data set contains 61 days worth of data, but the data for 8 days is missing.  Therefore, our early calculations show that the average daily number of steps is 10766.19 over 53 days (for a total of 570608 steps).  Our imputation changes the average only slightly to 10765.64 per day over 61 days (for a total of 656704).  While the totals are different, this is only because there are an additional 8 days worth of data.

One more set of graphs to show us the two histograms, side-by-side

*TODO lattice plot -- or ggplot2*

```r
par(mfrow = c(1,2),
    mar = c(4,4,1,1), oma = c(0,0,2,0))
# mfrow -- 1 row of plots, 2 columns
#  mar  -- margins bottom, left, top, right
#  oma  -- outer margins bottom...

with( dailysummary, hist(steps,30, ylim = c(0,20), main=""))
with(ndailysummary, hist(steps,30, ylim = c(0,20), main=""))
mtext("Comparing histograms of original and imputed data", outer = TRUE)
```

![](PA1_template_files/figure-html/interpolate1_figure2-1.png) 

&nbsp;  <!-- I need more space here than markdown will give me by default! -->


## Are there differences in activity patterns between weekdays and weekends?
> For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.
> 
> 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
> 1. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

As required, we create a new factor variable splitting the weekday and the weekend.  We start with the `weekdays()` function that transforms the date field into a string describing the day of the week (Sunday through Saturday).  We then pass this to an anonymous function, via `sapply()` that returns either "weekday" or "weekend" depending on the output from `weekdays()`. 

I like using `%>%` for this, as we don't have to create a whole string of one-off variables, but the string of processing is clearer than if we nest things like this: third_action(second_action(first_action))


```r
# table(sapply(weekdays(activity$date), function(d) { if ((d == "Saturday") || (d == "Sunday")) { "weekend" } else { "weekday"}}))
daytype <- function(dayname) {
    if (dayname == "Saturday") { "weekend" }
    else if (dayname == "Sunday") { "weekend" }
    else { "weekday" }
}

newactivity <- newactivity %>%
                mutate(weekday = weekdays(date), daytype = vapply(weekday, daytype, ""))

table(newactivity$daytype)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
#newcolumn <- newactivity$date %>%
#                weekdays() %>%
#                sapply(function(d) { if ((d == "Saturday") || (d == "Sunday")) { "weekend" } else { "weekday"}})
#
#newactivity = data.frame(newactivity, newcolumn)
```

Finally we create a table that averages the number of steps taken across each of the 5-minute intervals for daytype="weekday" and daytime="weekend".  We then use this to create the panel plot containing a time series plot of the 5-minute interval vs. average number of steps taken, for weekend days and for weekday days.


```r
interval_dt_summary <- newactivity %>%
                        group_by(interval, daytype) %>%
                        summarize(meansteps = mean(steps))

#        geom_smooth(method = "lm") +
plot <- ggplot(interval_dt_summary, aes(interval, meansteps)) +
        geom_line() +
        facet_grid(daytype ~ .) +
        labs(title = "Activity level over a day, Weekday vs. Weekend",
             y="Average number of Steps",
             x="Interval"
        )
print(plot)
```

![](PA1_template_files/figure-html/splitweekday_plot-1.png) 

## Notes
I'd like to do a better imputation -- something that makes the histogram of the new dataset look more like the histogram of the original data set.  I think I can do this by combining an assumption that the activity of a missing day is relatively proportional to the activity of the days preceeding and following it, and by noticing that the original histogram corresponds to a bell curve.  The latter implies to me that I can use the Normal distribution functions to create random values for the missing intervals using rnorm().

Even more advanced, we could factor in weekday vs. weekend, or even see if there are different trends for each day of the week and use those.

Here's a graph of the average steps per day across the whole data set, with the X axis markers breaking the data up into week-long chunks.


```r
dt <- qplot(as.Date(date), steps, geom = c("line","point"), data = dailysummary)
print(dt + scale_x_date(breaks = "1 week"))
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

```
## Warning: Removed 8 rows containing missing values (geom_point).
```

![](PA1_template_files/figure-html/timeseries_mean_daily_steps-1.png) 
