library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
setwd("~/Stat480/RDataScience/AirlineDelays")
airports = read.csv("airports.csv")
air_data = fread("Airlines0305_copy.csv")
# Joins the 2003, 2005 flights data with the airports data set and joined by origin of the flight
merged_airports = air_data %>% left_join(airports, c("Origin" = "iata"))
# save(merged_airports, file = "merged_airports.csv")
# merged_airports = fread("merged_airports.csv")


# Delays statistics by state
merged_airports$state_name = tolower(state.name[match(merged_airports$state, state.abb)])
by_year_state = group_by(merged_airports, Year, state_name)
year_state_delays = summarise(by_year_state, 
                         count = n(),
                         mean_dep_delay = mean(DepDelay, na.rm = TRUE),
                         mean_arr_delay = mean(ArrDelay, na.rm = TRUE),
                         total_cancelled = sum(Cancelled == 1),
                         cancellation_rates = sum(Cancelled == 1)/count
)
# state_delays = arrange(state_delays, desc(mean_dep_delay))

## Loading packages
inst_pkgs = load_pkgs = c("streamR","ROAuth",
                           "ggplot2", "grid","scales", "maps")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

# Visualize mean departure and arrival delays by state
library(maps)
states.map = map_data("state")

# Mean Dep Delay By State in 2003
ggplot(year_state_delays[year_state_delays[, "Year"] == 2003, ], aes(map_id = state_name)) + 
  geom_map(aes(fill = mean_dep_delay), map = states.map) +
  scale_fill_gradientn(colours=c("blue","green","yellow","red")) + 
  expand_limits(x = states.map$long, y = states.map$lat)

# Mean Dep Delay By State in 2005
ggplot(year_state_delays[year_state_delays[, "Year"] == 2005, ], aes(map_id = state_name)) + 
  geom_map(aes(fill = mean_dep_delay), map = states.map) +
  scale_fill_gradientn(colours=c("blue","green","yellow","red")) + 
  expand_limits(x = states.map$long, y = states.map$lat)

# Mean Arr Delay By State in 2003
ggplot(year_state_delays[year_state_delays[, "Year"] == 2003, ], aes(map_id = state_name)) + 
  geom_map(aes(fill = mean_arr_delay), map = states.map) +
  scale_fill_gradientn(colours=c("blue","green","yellow","red")) + 
  expand_limits(x = states.map$long, y = states.map$lat)

# Mean Arr Delay By State in 2005
ggplot(year_state_delays[year_state_delays[, "Year"] == 2005, ], aes(map_id = state_name)) + 
  geom_map(aes(fill = mean_arr_delay), map = states.map) +
  scale_fill_gradientn(colours=c("blue","green","yellow","red")) + 
  expand_limits(x = states.map$long, y = states.map$lat)

# Cancellation rates by State in 2003
ggplot(year_state_delays[year_state_delays[, "Year"] == 2003, ], aes(map_id = state_name)) + 
  geom_map(aes(fill = cancellation_rates), map = states.map) +
  scale_fill_gradientn(colours=c("blue","green","yellow","red")) + 
  expand_limits(x = states.map$long, y = states.map$lat)

# Cancellation rates by State in 2005
ggplot(year_state_delays[year_state_delays[, "Year"] == 2005, ], aes(map_id = state_name)) + 
  geom_map(aes(fill = cancellation_rates), map = states.map) +
  scale_fill_gradientn(colours=c("blue","green","yellow","red")) + 
  expand_limits(x = states.map$long, y = states.map$lat)

# Visualize cancellations by state

# Total cancellations by State in 2003
ggplot(year_state_delays[year_state_delays[, "Year"] == 2003, ], aes(map_id = state_name)) + 
  geom_map(aes(fill = total_cancelled), map = states.map) +
  scale_fill_gradientn(colours=c("blue","green","yellow","red")) + 
  expand_limits(x = states.map$long, y = states.map$lat)

# Total cancellations by State in 2005
ggplot(year_state_delays[year_state_delays[, "Year"] == 2003, ], aes(map_id = state_name)) + 
  geom_map(aes(fill = total_cancelled), map = states.map) +
  scale_fill_gradientn(colours=c("blue","green","yellow","red")) + 
  expand_limits(x = states.map$long, y = states.map$lat)


## Aggregate State Analysis 

## Get delays and cancellation statistics by state and each month and year 
by_year_month_state = group_by(merged_airports, state, Year, Month)
state = summarise(by_year_month_state, 
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

## Get delays and cancellation statistics by state and each year 
by_year_state = group_by(merged_airports, state, Year)
state2 = summarise(by_year_state, 
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

# Get the data for 2003 and give it rownames and remove non numeric cols
state03 = state2[state2[, "Year"] == 2003, ]
state03_names = state03$state
state03 = data.matrix(state03)
rownames(state03) = state03_names
state03 = state03[1:51,]
state03 = state03[, -c(1,2)]

# Get the data for 2005 and give it rownames and remove non numeric cols
state05 = state2[state2[, "Year"] == 2005, ]
state05_names = state05$state
state05 = data.matrix(state05)
rownames(state05) = state05_names
state05 = state05[1:51,]
state05 = state05[, -c(1,2)]



# Heat maps of the states and their characteristics in 2003 and 2005
library(gplots)
par(mar=c(50,5,5,10))
# 2003 states heatmap
heatmap.2(state03, scale = "column", dendrogram = "row", density.info = "none", margins = c(9,9),
          keysize = 1.3, main = "03 States Analysis")
# 2005 states heatmap
heatmap.2(state05, scale = "column", dendrogram = "row", density.info = "none", margins = c(8.5,8.5),
          keysize = 1.3, main = "05 States Analysis")


