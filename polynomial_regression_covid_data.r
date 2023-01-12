https://mdl.library.utoronto.ca/technology/tutorials/covid-19-data-r

#Loading libraries
pacman::p_load(ggplot2, tidyverse, dplyr, corrplot, naniar, plotly, data.table, plyr)
#data.table package for fread() function

#Importing datasets
covid.data <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

#looking into the dataset
head(covid.data)
str(covid.data)

#cleaning the data
#checking if there are missing values
colSums(is.na(covid.data))

#plotting the missing values using naniar package
gg_miss_var(covid.data)
#lots of missing values

#Replacing the missing values with certain data
##Since I won't be using all the variables in the dataset, I will be only replacing the missing values of certain variables
##creating functions that replace the missing values with 0 and mean
replace.zero <- function(z) +
  replace(z, is.na(z), 0)

replace.mean <- function(x) +
  replace(x, is.na(x), mean(x, na.rm =T))


covid.data <- ddply(covid.data, ~location, transform,
                     new_cases = replace.zero(new_cases),
                     total_cases = replace.zero(total_cases),
                     new_deaths = replace.zero(new_deaths),
                     total_deaths = replace.zero(total_deaths),
                     tests_per_case = replace.zero(tests_per_case),
                     people_fully_vaccinated = replace.zero(people_fully_vaccinated),
                     population_density = replace.mean(population_density),
                     median_age = replace.mean(median_age),
                     gdp_per_capita = replace.mean(gdp_per_capita),
                     extreme_poverty = replace.mean(extreme_poverty),
                     human_development_index = replace.mean(human_development_index),
                     handwashing_facilities = replace.mean(handwashing_facilities),
                     male_smokers = replace.mean(male_smokers),
                     female_smokers = replace.mean(female_smokers),
                     population = replace.mean(population))

#changing date variable into date format
covid.data$date <- as.Date(covid.data$date, format = "%d/%m/%Y")

#Removing continents from country column
continents <- c("Asia", "Africa", "European Union", "Europe", "High income", "Lower middle income", "Low Income", "Upper middle income", 'Oceania', "South America", "North America", "International", "World")
covid.data <- subset(covid.data, !(location %in% continents))

#latest day
day_latest <- max(covid.data$date)

#creating heatmaps
covid.cases <- covid.data %>%
  group_by(location) %>%
  filter(date == max(date))

##creating covid cases heat maps
line <- list(color = toRGB("#d1d1d1"), width = 0.4)
heatmap <- list(
  showframe = F,
  showcoastlines = F,
  projection = list(type = "orthographic"),
  resolution = "100",
  showcountries = T,
  countrycolor = "#d1d1d1",
  showocean = T,
  oceancolor = '#064273',
  showlakes = T,
  lakecolor = '#99c0db',
  showrivers = T,
  rivercolor = '#99c0db',
  bgcolor = '#e8f7fc')

plot_geo() %>%
  layout(geo = heatmap,
         paper_bgcolor = '#e8f7fc',
         title = paste0("World COVID-19 Confirmed Cases till ",  day_latest)) %>%
  add_trace(data = covid.cases,
            z = ~total_cases,
            colors = "Reds",
            text = ~location,
            locations = ~iso_code,
            marker = list(line = line))

##Heatmap for covid deaths
plot_geo() %>%
  layout(geo = heatmap,
         paper_bgcolor = '#e8f7fc',
         title = paste0("World COVID-19 deaths till ",  day_latest)) %>%
  add_trace(data = covid.cases,
            z = ~total_deaths,
            colors = "Reds",
            text = ~location,
            locations = ~iso_code,
            marker = list(line = line))

##Heatmap for COVID vaccination status
covid.vaccination <- covid.data %>%
  group_by(location) %>%
  #filter(date == max(date)) %>%
  mutate(people_fully_vaccinated = max(people_fully_vaccinated, na.rm = T)) %>%
  select(date, location, people_fully_vaccinated) %>%
  filter(date == max(date))

plot_geo() %>%
  layout(geo = heatmap,
         paper_bgcolor = '#e8f7fc',
         title = paste0("Vaccination status till ",  day_latest)) %>%
  add_trace(data = covid.vaccination,
            z = ~people_fully_vaccinated,
            colors = "Reds",
            text = ~location,
            locations = ~iso_code,
            marker = list(line = line))
  

