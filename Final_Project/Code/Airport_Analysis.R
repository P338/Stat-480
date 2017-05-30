### This section contains code that analyzes airports

library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
setwd("~/Stat480/RDataScience/AirlineDelays")
airports = read.csv("airports.csv")
air_data = fread("Airlines0305_copy.csv")
# Joins the 2003, 2005 flights data with the airports data set and joined by origin of the flight
merged_airports = air_data %>% left_join(airports, c("Origin" = "iata"))



#write_csv(merged_airports, file = "merged_airports.csv")
#merged_airports = read_csv("merged_airports.csv")


# Delays & Cancellation rates by airports
by_year_airport = group_by(merged_airports, Year, airport, long, lat)
airport = summarise(by_year_airport, 
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

by_year_airport2 = group_by(merged_airports, Year, airport)
airport2 = summarise(by_year_airport2, 
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

# Top 20 airports in 2003 by flights
airport03 = airport2[airport2[, "Year"] == 2003, ]
airport03 = arrange(airport03, desc(count))
airport03_top20 = airport03[1:20, ]
airport03_top20_names = airport03_top20$airport
airport03_top20 = data.matrix(airport03_top20)
rownames(airport03_top20) = airport03_top20_names
airport03_top20 = airport03_top20[, -c(1,2)]
airport03_top20

# Top 20 airports in 2005 by flights
airport05 = airport2[airport2[, "Year"] == 2005, ]
airport05 = arrange(airport05, desc(count))
airport05_top20 = airport05[1:20, ]
airport05_top20_names = airport05_top20$airport
airport05_top20 = data.matrix(airport05_top20)
rownames(airport05_top20) = airport05_top20_names
airport05_top20 = airport05_top20[, -c(1,2)]
airport05_top20

# Heat maps of airports
library(gplots)
par(mar=c(50,4,4,10))
# 2003 busiest 20 airports heatmap
heatmap.2(airport03_top20, scale = "column", dendrogram = "row", density.info = "none", margins = c(8.5,8),
          keysize = 1.5, main = "Top 20 Airports 03")
# 2005 busiest 20 airports heatmap
heatmap.2(airport05_top20, scale = "column", dendrogram = "row", density.info = "none", margins = c(8.5,8),
          keysize = 1.5, main = "Top 20 Airports 05")


# Keep and filter aiports with more than 1000 flights
airport = filter(airport, count > 1000)
arrange(airport, desc(cancellation_rates))

# Getting airport longitude, latitude, and cancellation rates for the filterd airports in 2003 and 2005
library(maps)
usa.map.data = map_data("usa")

airport03 = airport[airport[, "Year"] == 2003 & airport[, "lat"] < 50 & airport[, "lat"] > 27, ]
data03 = data.frame(long = c(airport03$long), lat = c(airport03$lat),
                    cancellation_rates = airport03$cancellation_rates)
airport05 = airport[airport[, "Year"] == 2005 & airport[, "lat"] < 50 & airport[, "lat"] > 27, ]
data05 = data.frame(long = c(airport05$long), lat = c(airport05$lat), 
                          cancellation_rates = airport05$cancellation_rates)

# Map of cancellation rates by airports in 2003
ggplot(usa.map.data) +
  geom_map(aes(map_id = region), map = usa.map.data, fill = "white", color = "grey20", size = 0.25) + 
  expand_limits(x = usa.map.data$long, y = usa.map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) +
  geom_point(data = data03, aes(x = long, y = lat, colour = cancellation_rates), size = 3, alpha = 1/2) + 
           scale_colour_gradient(low="blue", high="red")

# Map of cancellation rates by airports in 2005
ggplot(usa.map.data) +
  geom_map(aes(map_id = region), map = usa.map.data, fill = "white", color = "grey20", size = 0.25) + 
  expand_limits(x = usa.map.data$long, y = usa.map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) +
  geom_point(data = data05, aes(x = long, y = lat, colour = cancellation_rates), size = 3, alpha = 1/2) + 
  scale_colour_gradient(low="blue", high="red")  



# Delays by airports
by_airport = group_by(merged_airports, airport)
airport_delays = summarise(by_airport, 
                           count = n(),
                           mean_dep_delay = mean(DepDelay, na.rm = TRUE),
                           mean_arr_delay = mean(ArrDelay, na.rm = TRUE)
)
airport_delays = filter(airport_delays, count > 10000)
arrange(airport_delays, desc(mean_dep_delay))



# Count the number of flights between two airports
merged_airports = merged_airports %>% left_join(airports, c("Dest" = "iata"))
merged_airports03 = merged_airports[merged_airports[, "Year"] == 2003, ]
merged_airports05 = merged_airports[merged_airports[, "Year"] == 2005, ]

flight_routes03 = merged_airports03 %>% group_by(airport.x , airport.y) %>%
  summarize(route_count = n())
flight_routes05 = merged_airports05 %>% group_by(airport.x , airport.y) %>%
  summarize(route_count = n())

#flight_routes = aggregate(route_count ~ airport.x + airport.y , merged_airports2, FUN = sum)

# Filter tge top 20 flight routes by count in 2003 and 2005
flight_routes03 = flight_routes03[ order(-flight_routes03[,"route_count"]), ]
flight_routes03 = head(flight_routes03, 20)
merged_routes03 = flight_routes03 %>% left_join(airports, c("airport.x" = "airport"))
merged_routes03 = merged_routes03 %>% left_join(airports, c("airport.y" = "airport"))
merged_routes03

flight_routes05 = flight_routes05[ order(-flight_routes05[,"route_count"]), ]
flight_routes05 = head(flight_routes05, 20)
merged_routes05 = flight_routes05 %>% left_join(airports, c("airport.x" = "airport"))
merged_routes05 = merged_routes05 %>% left_join(airports, c("airport.y" = "airport"))
merged_routes05


# Map of top 20 plane routes in 2003
library(plotly)
geo = list(
  scope = "usa",
  projection = list(type='albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)


p = plot_geo(locationmode = 'usa') %>%
  add_markers(
    data = merged_routes03, x = merged_routes03$long.x , y = merged_routes03$lat.x, text = merged_routes03$airport.x,
    size = 0.01, hoverinfo = "text", alpha = 0.5, color = I("blue")
  ) %>%
  add_markers(
    data = merged_routes03, x = merged_routes03$long.y , y = merged_routes03$lat.y, text = merged_routes03$airport.y,
    size = 0.01, hoverinfo = "text", alpha = 0.5, color = I("blue")
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[1], merged_routes03$long.y[1]),
    y = c(merged_routes03$lat.x[1], merged_routes03$lat.y[1]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[2], merged_routes03$long.y[2]),
    y = c(merged_routes03$lat.x[2], merged_routes03$lat.y[2]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[3], merged_routes03$long.y[3]),
    y = c(merged_routes03$lat.x[3], merged_routes03$lat.y[3]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[4], merged_routes03$long.y[4]),
    y = c(merged_routes03$lat.x[4], merged_routes03$lat.y[4]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[5], merged_routes03$long.y[5]),
    y = c(merged_routes03$lat.x[5], merged_routes03$lat.y[5]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[6], merged_routes03$long.y[6]),
    y = c(merged_routes03$lat.x[6], merged_routes03$lat.y[6]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[7], merged_routes03$long.y[7]),
    y = c(merged_routes03$lat.x[7], merged_routes03$lat.y[7]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[8], merged_routes03$long.y[8]),
    y = c(merged_routes03$lat.x[8], merged_routes03$lat.y[8]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[9], merged_routes03$long.y[9]),
    y = c(merged_routes03$lat.x[9], merged_routes03$lat.y[9]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[10], merged_routes03$long.y[10]),
    y = c(merged_routes03$lat.x[10], merged_routes03$lat.y[10]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[11], merged_routes03$long.y[11]),
    y = c(merged_routes03$lat.x[11], merged_routes03$lat.y[11]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[12], merged_routes03$long.y[12]),
    y = c(merged_routes03$lat.x[12], merged_routes03$lat.y[12]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[13], merged_routes03$long.y[13]),
    y = c(merged_routes03$lat.x[13], merged_routes03$lat.y[13]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[14], merged_routes03$long.y[14]),
    y = c(merged_routes03$lat.x[14], merged_routes03$lat.y[14]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[15], merged_routes03$long.y[15]),
    y = c(merged_routes03$lat.x[15], merged_routes03$lat.y[15]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[16], merged_routes03$long.y[16]),
    y = c(merged_routes03$lat.x[16], merged_routes03$lat.y[16]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[17], merged_routes03$long.y[17]),
    y = c(merged_routes03$lat.x[17], merged_routes03$lat.y[17]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[18], merged_routes03$long.y[18]),
    y = c(merged_routes03$lat.x[18], merged_routes03$lat.y[18]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[19], merged_routes03$long.y[19]),
    y = c(merged_routes03$lat.x[19], merged_routes03$lat.y[19]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes03,
    x = c(merged_routes03$long.x[20], merged_routes03$long.y[20]),
    y = c(merged_routes03$lat.x[20], merged_routes03$lat.y[20]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  layout(
    title = 'Top 20 flight routes in 2003',
    geo = geo, showlegend = FALSE
  )

routes03 = plotly_build(p)
htmlwidgets::saveWidget(routes03, "Top20_Routes_03.html")



# Map of top 20 plane routes in 2005
p2 = plot_geo(locationmode = 'usa') %>%
  add_markers(
    data = merged_routes05, x = merged_routes05$long.x , y = merged_routes05$lat.x, text = merged_routes05$airport.x,
    size = 0.01, hoverinfo = "text", alpha = 0.5, color = I("blue")
  ) %>%
  add_markers(
    data = merged_routes05, x = merged_routes05$long.y , y = merged_routes05$lat.y, text = merged_routes05$airport.y,
    size = 0.01, hoverinfo = "text", alpha = 0.5, color = I("blue")
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[1], merged_routes05$long.y[1]),
    y = c(merged_routes05$lat.x[1], merged_routes05$lat.y[1]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[2], merged_routes05$long.y[2]),
    y = c(merged_routes05$lat.x[2], merged_routes05$lat.y[2]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[3], merged_routes05$long.y[3]),
    y = c(merged_routes05$lat.x[3], merged_routes05$lat.y[3]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[4], merged_routes05$long.y[4]),
    y = c(merged_routes05$lat.x[4], merged_routes05$lat.y[4]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[5], merged_routes05$long.y[5]),
    y = c(merged_routes05$lat.x[5], merged_routes05$lat.y[5]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[6], merged_routes05$long.y[6]),
    y = c(merged_routes05$lat.x[6], merged_routes05$lat.y[6]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[7], merged_routes05$long.y[7]),
    y = c(merged_routes05$lat.x[7], merged_routes05$lat.y[7]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[8], merged_routes05$long.y[8]),
    y = c(merged_routes05$lat.x[8], merged_routes05$lat.y[8]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[9], merged_routes05$long.y[9]),
    y = c(merged_routes05$lat.x[9], merged_routes05$lat.y[9]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[10], merged_routes05$long.y[10]),
    y = c(merged_routes05$lat.x[10], merged_routes05$lat.y[10]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[11], merged_routes05$long.y[11]),
    y = c(merged_routes05$lat.x[11], merged_routes05$lat.y[11]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[12], merged_routes05$long.y[12]),
    y = c(merged_routes05$lat.x[12], merged_routes05$lat.y[12]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[13], merged_routes05$long.y[13]),
    y = c(merged_routes05$lat.x[13], merged_routes05$lat.y[13]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[14], merged_routes05$long.y[14]),
    y = c(merged_routes05$lat.x[14], merged_routes05$lat.y[14]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[15], merged_routes05$long.y[15]),
    y = c(merged_routes05$lat.x[15], merged_routes05$lat.y[15]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[16], merged_routes05$long.y[16]),
    y = c(merged_routes05$lat.x[16], merged_routes05$lat.y[16]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[17], merged_routes05$long.y[17]),
    y = c(merged_routes05$lat.x[17], merged_routes05$lat.y[17]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[18], merged_routes05$long.y[18]),
    y = c(merged_routes05$lat.x[18], merged_routes05$lat.y[18]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[19], merged_routes05$long.y[19]),
    y = c(merged_routes05$lat.x[19], merged_routes05$lat.y[19]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  add_trace(
    data = merged_routes05,
    x = c(merged_routes05$long.x[20], merged_routes05$long.y[20]),
    y = c(merged_routes05$lat.x[20], merged_routes05$lat.y[20]),
    alpha = 0.3, size = I(1.5), color = I("green"), hoverinfo = "text", mode = "lines"
  ) %>%
  layout(
    title = 'Top 20 flight routes in 2005',
    geo = geo, showlegend = FALSE
  )

plotly_build(p2)

routes05 = plotly_build(p2)
htmlwidgets::saveWidget(routes05, "Top20_Routes_05.html")
