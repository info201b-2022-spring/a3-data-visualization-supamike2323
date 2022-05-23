suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(hrbrthemes))
suppressMessages(library(geojsonio))
suppressMessages(library(RColorBrewer))
suppressMessages(library(rgdal))
suppressMessages(library(broom))
suppressMessages(library(rgeos))
incar_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
#incar_df <- read.csv("incarceration_trends.csv")

summary_info <- list()
summary_info$num_observations <- nrow(incar_df)
summary_info$num_types <- ncol(incar_df)

#area with highest jail population

area1 <- incar_df %>%
  group_by(region) %>%
  summarize(total_jail_popupation = sum(total_jail_pop, na.rm = TRUE))

summary_info$highest_jail_population <- max(area1[,'total_jail_popupation'])
area_with_highest_jail_population <- toString(area1[which(area1$total_jail_popupation == summary_info$highest_jail_population), 1])
summary_info$area_with_highest_jail_po <- area_with_highest_jail_population


#unbanicity with highest jail population
urbanicity1 <- incar_df %>%
  group_by(urbanicity) %>%
  summarize(total_jail_popupation = sum(total_jail_pop, na.rm = TRUE))

urbanicity_with_highest_jail_population <- toString(urbanicity1[which(area1$total_jail_popupation == summary_info$highest_jail_population), 1])
summary_info$urbanicity_with_highest_jail_po <- urbanicity_with_highest_jail_population

#total black population in jail over the years 
total_pop_jail <- incar_df %>%
  summarize(total_population_jail = sum(total_jail_pop, na.rm = TRUE)) 

#total black population in jail over the years 
black_pop_jail <- incar_df %>%
  summarize(black_population_jail = sum(black_jail_pop, na.rm = TRUE)) 

#total white population in jail over the years 
white_pop_jail <- incar_df %>%
  summarize(white_population_jail = sum(white_jail_pop, na.rm = TRUE)) 

#total latinx population in jail over the years 
latinx_pop_jail <- incar_df %>%
  summarize(latinx_population_jail = sum(latinx_jail_pop, na.rm = TRUE)) 

#total asian population in jail over the years 
aapi_pop_jail <- incar_df %>%
  summarize(aapi_population_jail = sum(aapi_jail_pop, na.rm = TRUE)) 

#Merge together
joined_chart <- merge(total_pop_jail, white_pop_jail)
joined_chart <- merge(joined_chart, black_pop_jail)
joined_chart <- merge(joined_chart, latinx_pop_jail)
joined_chart <- merge(joined_chart, aapi_pop_jail)

# Put them into summary
summary_info$total_pop_jail <- toString(round(total_pop_jail[1,1]))
summary_info$black_pop_jail <- toString(round(black_pop_jail[1,1]))
summary_info$white_pop_jail <- toString(round(white_pop_jail[1,1]))
summary_info$latinx_pop_jail <- toString(round(latinx_pop_jail[1,1]))
summary_info$aapi_pop_jail <- toString(round(aapi_pop_jail[1,1]))


#Trends over the time chart
black_jail_chart <- incar_df %>%
  group_by(year) %>%
  summarize(black_jail_pop = sum(black_jail_pop, na.rm = TRUE))

white_jail_chart <- incar_df %>%
  group_by(year) %>%
  summarize(white_jail_pop = sum(white_jail_pop, na.rm = TRUE))

latino_jail_chart <- incar_df %>%
  group_by(year) %>%
  summarize(latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE))

aapi_jail_chart <- incar_df %>%
  group_by(year) %>%
  summarize(aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE))

total_jail_chart <- incar_df %>%
  group_by(year) %>%
  summarize(aapi_jail_pop = sum(total_jail_pop, na.rm = TRUE))

total_time_chart <- black_jail_chart[,1]
total_time_chart['black_jail_pop'] <- black_jail_chart[,2]
total_time_chart['white_jail_pop'] <- white_jail_chart[,2]
total_time_chart['latinx_jail_pop'] <- latino_jail_chart[,2]
total_time_chart['aapi_jail_pop'] <- aapi_jail_chart[,2]
total_time_chart['total_jail_pop'] <- total_jail_chart[,2]

#Trends over time chart:
colors <- c("Black Population in Jail" = "blue", "White Population in Jail" = "red", "Latino Population in Jail" = "orange", "Asian Population in Jail" = "black")
time_chart <- suppressWarnings(ggplot(total_time_chart, aes(x = year)) + 
  geom_point(aes(y = black_jail_pop, color = "Black Population in Jail"), size = 1.5)+
  geom_point(aes(y = white_jail_pop, color = "White Population in Jail"), size = 1.5)+
  geom_point(aes(y = latinx_jail_pop, color = "Latino Population in Jail"), size = 1.5)+
  geom_point(aes(y = aapi_jail_pop, color = "Asian Population in Jail"), size = 1.5)+
  labs(title = "Population in jail by race vs. time (1985 - 2018)", x = "Year", y = "Jail population by race") + 
  theme(legend.position = 'right') + suppressWarnings(xlim(1985, 2018)))

#Variable comparison chart:
black_pop_15to64_df <- incar_df %>%
  group_by(year) %>%
  summarize(black_pop_15to64 = sum(black_pop_15to64, na.rm = TRUE))

joined_black_pop_15to64_with_in_jail <- merge(black_pop_15to64_df, black_jail_chart)


colors <- c("Black Population in Jail" = "blue", "Black Population from 15 to 64" = "red")
vari_chart <- suppressWarnings(ggplot(joined_black_pop_15to64_with_in_jail, aes(x = year)) + 
                                 geom_point(aes(y = black_pop_15to64, color = "Black Population from 15 to 64"), size = 1.5) +
                                 geom_line(aes(y = black_pop_15to64, color = "Black Population from 15 to 64"), size = 1.5) +
                                 geom_point(aes(y = black_jail_pop, color = "Black Population in jail"), size = 1.5) +
                                 geom_line(aes(y = black_jail_pop, color = "Black Population in jail"), size = 1.5) +
                                 labs(title = "Black population in jail vs. Black population between 15 to 64 (1990 - 2018)", x = "Year", y = "Population") + 
                                 theme(legend.position = 'right')
                               + suppressWarnings(xlim(1990, 2018))
                               )


#Map of black population in jail in 2017, I have used code from https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html for making the chart below. 
black_jail_in_state <- incar_df %>%
  filter(year == 2017) %>%
  group_by(state) %>%
  summarize(black_jail_pop = sum(round(black_jail_pop), na.rm = TRUE))
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()
states1 <- list("Arkansas", "Alaska", "Arizona", "Alabama", "California", "Colorado", 
                "Connecticut", "Washington D.C", "Delaware", "Florida", "Georgia", "Hawaii", "Iowa", 
                "Indiana", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Massachusetts", 
                "Maryland", "Maine", "Michigan", "Minnesota", "Missouri",
                "Mississippi", "Montana", "North Carolina", "North Dakota", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", 
                "Nevada", "New York", "Ohio", "Oklahoma", "Oregon", 
                "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
                "Texas", "Utah", "Virginia", "Vermont", "Washington", "Wisconsin", 
                "West Virginia", "Wyoming")
for (i in 1:51){
  black_jail_in_state[i, 1] <- states1[[i]]
  
}
data <- black_jail_in_state
data %>% 
  ggplot( aes(x=black_jail_pop)) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white') + 
  scale_x_continuous(breaks = seq(1,30))

spdf_fortified <- spdf_fortified %>%
  left_join(. , data, by=c("id"="state")) 

map_final <- ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill =  black_jail_pop, x = long, y = lat, group = group)) +
  scale_fill_gradient(trans = "log") +
  theme_void() +
  coord_map() + 
  labs(title = "Black Jail population in 2017 (Black_jail_pop)")
