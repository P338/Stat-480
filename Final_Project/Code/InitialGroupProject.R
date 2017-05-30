### Group Project Airline Data
## This file has the loaded big matrix and does intial exploratory analysis

setwd("~/Stat480/RDataScience/AirlineDelays")
library(biganalytics)
library(foreach)
library(ggplot2)

convertCSVColumns <- function(file, collist){
  fulldata<-read.csv(file)
  for (i in collist) {
    fulldata[,i]<-convertColumn(fulldata[,i])
  }
  write.csv(fulldata, file, row.names=FALSE)
}

# The following function is called by convertCSVColumns. It converts a single column to integer indices.
convertColumn <- function(values){
  allvals<-as.character(values)
  valslist<-sort(unique(allvals))
  xx<-factor(allvals, valslist, labels=1:length(valslist))
  rm(allvals)
  rm(valslist)
  gc()
  as.numeric(levels(xx))[xx]
}

convertCSVColumns("Airlines0305.csv", c(9,11,17,18))

x <- read.big.matrix("Airlines0305.csv", header = TRUE, 
                     backingfile = "Airlines0305.bin",
                     descriptorfile = "Airlines0305.desc",
                     type = "integer", extraCols = "age")

########################################################################################
x <- attach.big.matrix("Airlines0305.desc")

## Number of flights per day in 2003 and 2005
dow = split(1:nrow(x), x[,"DayOfWeek"])
day_flights = foreach(dayInds = dow, .combine=cbind) %do% {
  length(dayInds)
}

## Number of flights per month in 2003 and 2005 combined
month = split(1:nrow(x), x[,"Month"])
month_flights = foreach(monthInds = month, .combine=cbind) %do% {
  length(monthInds)
}

## Number of flights per day in 2003 and 2005 individually
dow2 = split(1:nrow(x), list(x[,"DayOfWeek"], x[,"Year"]))
day_year_flights = foreach(day_yearInds = dow2, .combine=cbind) %do% {
  length(day_yearInds)
}

## Visual of number of flights per day in 2003 and 2005 respectively
dayInds = c(1:7)
plot(dayInds, day_year_flights[8:14], type = "l", col = "green", lty = 1, lwd = 3, ylim = c(700000, 1100000),
     xlab = "Day of Week", ylab = "# Flights", main = "# flights per day in each year")
points(dayInds, day_year_flights[1:7], type="l", col= "blue", lwd = 3)
legend("bottomleft", legend = c("2003", "2005"), col = c("blue", "green"), lwd = 3, cex = 0.7)

## Number of flights per month in 2003 and 2005 individually
month2 = split(1:nrow(x), list(x[,"Month"], x[,"Year"]))
month_year_flights = foreach(month_yearInds = month2, .combine=cbind) %do% {
  length(month_yearInds)
}

## Visual of number of flights per month in 2003 and 2005 respectively
monthInds = c(1:12)
plot(monthInds, month_year_flights[13:24], type = "l", col = "green", lty = 1, lwd = 3, ylim = c(400000, 650000),
     xlab = "Month", ylab = "# Flights", main = "# flights per month in each year")
points(monthInds, month_year_flights[1:12], type="l", col= "blue", lwd = 3)
legend("bottomleft", legend = c("2003", "2005"), col = c("blue", "green"), lwd = 3, cex = 0.7)

## Number of cancelled flights per day in 2003 and 2005 individually
day_year_cancelled = foreach(day_yearInds = dow2, .combine=cbind) %do% {
  sum(x[day_yearInds, "Cancelled"] == 1)
}

## Visual of cancelled flights per day per year
dayInds = c(1:7)
plot(dayInds, day_year_cancelled[1:7], type = "l", col = "blue", lty = 1, lwd = 3, ylim = c(0, 25000),
     xlab = "Day of Week", ylab = "# Cancelled", main = "# Cancelled flights per day in each year")
points(dayInds, day_year_cancelled[8:14], type="l", col= "green", lwd = 3)
legend("bottomleft", legend = c("2003", "2005"), col = c("blue", "green"), lwd = 3, cex = 0.7)

## Number of cancelled flights per month in 2003 and 2005 individually
month_year_cancelled = foreach(month_yearInds = month2, .combine=cbind) %do% {
  sum(x[month_yearInds, "Cancelled"] == 1)
}

## Number of cancelled flights per hour in 2003 and 2005 respectively
x03 = x[x[,"Year"] == 2003]
x05 = x[x[,"Year"] == 2005]
depHours03 = floor(x03[,"CRSDepTime"]/100)
depHours05 = floor(x05[,"CRSDepTime"]/100)
depHours03[depHours03==24] = 0
depHours05[depHours05==24] = 0

hours03 = split(1:length(depHours03), depHours03)
hours05 = split(1:length(depHours05), depHours05)

hours_cancelled_total03 = foreach(hoursInds = hours03, .combine=cbind) %do% {
  sum(x03[hoursInds, "Cancelled"] == 1)
}

hours_cancelled_total05 = foreach(hoursInds = hours05, .combine=cbind) %do% {
  sum(x05[hoursInds, "Cancelled"] == 1)
}

## Visual of cancelled flights per month per year
monthInds = c(1:12)
plot(monthInds, month_year_cancelled[seq(1:12)], type = "l", col = "blue", 
     lty = 1, lwd = 3, ylim = c(0, 30000),
     xlab = "Month", ylab = "# Cancelled", main = "# Cancelled flights per month in each year")
points(monthInds, month_year_cancelled[13:24], type="l", col= "green", lwd = 3)
legend("topright", legend = c("2003", "2005"), col = c("blue", "green"), lwd = 3, cex = 0.7)

## Dep Delay by months
month2 = split(1:nrow(x), list(x[,"Month"], x[,"Year"]))
names(month2) = c("Jan03","Feb03","Mar03","Apr03","May03","Jun03","Jul03","Aug03","Sep03","Oct03","Nov03","Dec03",
                  "Jan05","Feb05","Mar05","Apr05","May05","Jun05","Jul05","Aug05","Sep05","Oct05","Nov05","Dec05")
myProbs <- c(0.1 ,0.2 ,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999, 0.9999)
dep_delay_by_month = foreach(monInds = month2, .combine=cbind) %do% {
  require(bigmemory)
  quantile(x[monInds, "DepDelay"], myProbs, na.rm = TRUE)
}

## Arr Delay by months
month2 = split(1:nrow(x), list(x[,"Month"], x[,"Year"]))
names(month2) = c("Jan03","Feb03","Mar03","Apr03","May03","Jun03","Jul03","Aug03","Sep03","Oct03","Nov03","Dec03",
                  "Jan05","Feb05","Mar05","Apr05","May05","Jun05","Jul05","Aug05","Sep05","Oct05","Nov05","Dec05")
myProbs <- c(0.1 ,0.2 ,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999, 0.9999)
arr_delay_by_month = foreach(monInds = month2, .combine=cbind) %do% {
  require(bigmemory)
  quantile(x[monInds, "ArrDelay"], myProbs, na.rm = TRUE)
}

## Number of early arrivals, late arrivals, exact arrivals per day
arr_day_early = foreach(dayInds = dow, .combine=cbind) %do% {
  sum(x[dayInds, "ArrDelay"] < 0, na.rm = TRUE)
}

arr_day_late = foreach(dayInds = dow, .combine=cbind) %do% {
  sum(x[dayInds, "ArrDelay"] > 0, na.rm = TRUE)
}

arr_day_exact = foreach(dayInds = dow, .combine=cbind) %do% {
  sum(x[dayInds, "ArrDelay"] == 0, na.rm = TRUE)
}

## Number of early departures, late departures, exact departures delays per day
dep_day_early = foreach(dayInds = dow, .combine=cbind) %do% {
  sum(x[dayInds, "DepDelay"] < 0, na.rm = TRUE)
}

dep_day_late = foreach(dayInds = dow, .combine=cbind) %do% {
  sum(x[dayInds, "DepDelay"] > 0, na.rm = TRUE)
}

dep_day_exact = foreach(dayInds = dow, .combine=cbind) %do% {
  sum(x[dayInds, "DepDelay"] == 0, na.rm = TRUE)
}

## Heat map of number % cancelled flights per day per year
percent_day_year_cancelled = foreach(per_day_yearInds = dow2, .combine=cbind) %do% {
  sum(x[per_day_yearInds, "Cancelled"] == 1)/length(per_day_yearInds)
}
percent_day_year_cancelled

df.per_day_cancelled = expand.grid(dow = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"),
                                   year = c("2003", "2005"))

df.per_day_cancelled$per_cancelled = c(percent_day_year_cancelled)

ggplot(data = df.per_day_cancelled, aes(x = dow, y = year)) +
  geom_tile(aes(fill = per_cancelled)) 

## Heat map of number % cancelled flights per month per year
percent_month_year_cancelled = foreach(per_month_yearInds = month2, .combine=cbind) %do% {
  sum(x[per_month_yearInds, "Cancelled"] == 1)/length(per_month_yearInds)
}
percent_month_year_cancelled

df.per_month_cancelled = expand.grid(month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                   year = c("2003", "2005"))

df.per_month_cancelled$per_cancelled = c(percent_month_year_cancelled)

ggplot(data = df.per_month_cancelled, aes(x = month, y = year)) +
  geom_tile(aes(fill = per_cancelled)) 


##############################################################################


## Tree map of # number of cancelled flights per day per year
library(treemap)
total_day_year_cancelled = foreach(total_day_yearInds = dow2, .combine=cbind) %do% {
  sum(x[total_day_yearInds, "Cancelled"] == 1)
}
total_day_2003_cancelled = total_day_year_cancelled[1:7]
total_day_2005_cancelled = total_day_year_cancelled[8:14]

df.total_day_cancelled03 = expand.grid(dow = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
df.total_day_cancelled03$total_cancelled = c(total_day_2003_cancelled)

df.total_day_cancelled05 = expand.grid(dow = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
df.total_day_cancelled05$total_cancelled = c(total_day_2005_cancelled)

treemap(
  df.total_day_cancelled03,
  index=c("dow"),
  vSize="total_cancelled",
  vColor="total_cancelled",
  type="value"
)

treemap(
  df.total_day_cancelled05,
  index=c("dow"),
  vSize="total_cancelled",
  vColor="total_cancelled",
  type="value"
)


df.total_hrs_cancelled03 = expand.grid(hour = c("12am", "1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am",
                                             "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm",
                                             "6pm", "7pm", "8pm", "9pm", "10pm", "11pm"))
df.total_hrs_cancelled03$cancelled = c(hours_cancelled_total03)

df.total_hrs_cancelled05 = expand.grid(hour = c("12am", "1am", "2am", "3am", "4am", "5am", "6am", "7am", "8am",
                                               "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm",
                                               "6pm", "7pm", "8pm", "9pm", "10pm", "11pm"))
df.total_hrs_cancelled05$cancelled = c(hours_cancelled_total05)

df.total_hrs_cancelled = rbind(df.total_hrs_cancelled03, df.total_hrs_cancelled05)
df.total_hrs_cancelled$year = c(rep("2003", 24), rep("2005", 24))

## Ignore plots did not work
# library(ggplot2)
# ggplot(df.total_hrs_cancelled, aes(x=year, y=cancelled, fill=hour)) + 
#  geom_area() + ylim(0, 15000)


# Sector <- rep(c("S01","S02","S03","S04","S05","S06","S07"),times=7)
# Year <- as.numeric(rep(c("1950","1960","1970","1980","1990","2000","2010"),each=7))
# Value <- runif(49, 10, 100)
# data <- data.frame(Sector,Year,Value)

# ggplot(data, aes(x=Year, y=Value, fill=Sector)) + 
#  geom_area()



######################################################################################
# visualizations with gplot
# Ignore not useful
# library(gplots)
# x = attach.big.matrix("Airlines0305.desc")
# x_matrix = as.matrix(x)
# rownames(x_matrix) = x_matrix[, 1]
# x_matrix = x_matrix[, c(-23, -25:-30)]
# x_matrix = x_matrix[, c(14:16, 19:21)]
# x_df = as.data.frame(x)

# heatmap.2(x_matrix)

################################################################################### 
library(readr)
air_data = read_csv("Airlines0305_copy.csv") 
air0305 = air_data
save(air0305, file = 'air0305.RDdata')
air_data2 = read.csv("Airlines0305.csv", header = TRUE) 
head(air_data2)
load()
head(air0305)
origin = split(1:nrow(air0305), list(x[,"Origin"], x[,"Year"]))

