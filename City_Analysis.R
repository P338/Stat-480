### This section contains code that analyzes cities that flights originated from

library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
library(streamgraph)
library(tibble)
setwd("~/Stat480/RDataScience/AirlineDelays")
airports = read.csv("airports.csv")
carriers = read_csv("carriers.csv")
air_data = fread("Airlines0305_copy.csv")
# Joins the 2003, 2005 flights data with the airports data set and joined by origin of the flight
merged_airports = air_data %>% left_join(airports, c("Origin" = "iata"))


## Get delays and cancellation statistics by city and each year and month
by_year_month_city = group_by(merged_airports, city, Year, Month)
city = summarise(by_year_month_city, 
                 count = n(),
                 num_late_dep = sum(DepDelay > 15, na.rm = TRUE),
                 late_dep_rate = sum(DepDelay > 15, na.rm = TRUE)/count,
                 num_early_dep = sum(DepDelay < -15, na.rm = TRUE),
                 early_dep_rate = sum(DepDelay < -15, na.rm = TRUE)/count,
                 num_late_arr = sum(ArrDelay > 15, na.rm = TRUE),
                 late_arr_rate = sum(ArrDelay > 15, na.rm = TRUE)/count,
                 num_early_arr = sum(ArrDelay < -15, na.rm = TRUE),
                 early_arr_rate = sum(ArrDelay < -15, na.rm = TRUE)/count,
                 mean_dep_delay = mean(DepDelay, na.rm = TRUE),
                 mean_arr_delay = mean(ArrDelay, na.rm = TRUE),
                 total_cancelled = sum(Cancelled == 1),
                 cancellation_rates = sum(Cancelled == 1)/count
)
## Get delays and cancellation statistics by city and each year 
by_year_city = group_by(merged_airports, city, Year)
city2 = summarise(by_year_city, 
                 count = n(),
                 num_late_dep = sum(DepDelay > 15, na.rm = TRUE),
                 late_dep_rate = sum(DepDelay > 15, na.rm = TRUE)/count,
                 num_early_dep = sum(DepDelay < -15, na.rm = TRUE),
                 early_dep_rate = sum(DepDelay < -15, na.rm = TRUE)/count,
                 num_late_arr = sum(ArrDelay > 15, na.rm = TRUE),
                 late_arr_rate = sum(ArrDelay > 15, na.rm = TRUE)/count,
                 num_early_arr = sum(ArrDelay < -15, na.rm = TRUE),
                 early_arr_rate = sum(ArrDelay < -15, na.rm = TRUE)/count,
                 mean_dep_delay = mean(DepDelay, na.rm = TRUE),
                 mean_arr_delay = mean(ArrDelay, na.rm = TRUE),
                 total_cancelled = sum(Cancelled == 1),
                 cancellation_rates = sum(Cancelled == 1)/count
)





# 2003 Total Flights Per Month By City
data03 = (city[city[,"Year"] == 2003, c("Year", "Month", "city", "count")])
data03$YearMonth = paste0(data03$Year, sep = "-",  month.abb[data03$Month], "-01")
data03$YearMonth = as.Date(data03$YearMonth, format="%Y-%b-%d")

# Streamgraph of total flights by city and month in 2003
streamgraph(data03, "city", "count", "YearMonth", interactive=TRUE) %>%
  sg_axis_x(20, "Month", "%b") %>%
  sg_fill_brewer("Set1")

# 2003 Total Cancellations Per Month By City
data03 = (city[city[,"Year"] == 2003, c("Year", "Month", "city", "total_cancelled")])
data03$YearMonth = paste0(data03$Year, sep = "-",  month.abb[data03$Month], "-01")
data03$YearMonth = as.Date(data03$YearMonth, format="%Y-%b-%d")

# Streamgraph of total cancelled flights by city and month in 2003
streamgraph(data03, "city", "total_cancelled", "YearMonth", interactive=TRUE) %>%
  sg_axis_x(20, "Month", "%b") %>%
  sg_fill_brewer("PuOr")

# 2005 Total Flights Per Month By City
data05 = (city[city[,"Year"] == 2005, c("Year", "Month", "city", "count")])
data05$YearMonth = paste0(data05$Year, sep = "-",  month.abb[data05$Month], "-01")
data05$YearMonth = as.Date(data05$YearMonth, format="%Y-%b-%d")

# Streamgraph of total flights by city and month in 2005
streamgraph(data05, "city", "count", "YearMonth", interactive=TRUE) %>%
  sg_axis_x(20, "Month", "%b") %>%
  sg_fill_brewer("PiYG")

# 2005 Total Cancellations Per Month By City
data05 = (city[city[,"Year"] == 2005, c("Year", "Month", "city", "total_cancelled")])
data05$YearMonth = paste0(data05$Year, sep = "-",  month.abb[data05$Month], "-01")
data05$YearMonth = as.Date(data05$YearMonth, format="%Y-%b-%d")

# Streamgraph of total cancelled flights by city and month in 2005
streamgraph(data05, "city", "total_cancelled", "YearMonth", interactive=TRUE) %>%
  sg_axis_x(20, "Month", "%b") %>%
  sg_fill_brewer("BrBG")
