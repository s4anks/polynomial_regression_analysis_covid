#Loading libraries
pacman::p_load(ggplot2, tidyverse, dplyr, corrplot, naniar, plotly, data.table, plyr)
#data.table package for fread() function

#Importing datasets
covid.data <- fread("https://raw.githubusercontent.com/Roshann-Rai/Polynomial-Regression-on-covid-dataset/main/covid_data.csv")

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
##creating functions that replace the missing values with 0, mean and median
replace.zero <- function(z) +
  replace(z, is.na(z), 0)

replace.mean <- function(x) +
  replace(x, is.na(x), mean(x, na.rm =T))


covid.data1 <- ddply(covid.data, ~location, transform,
                     new_cases = replace.zero(new_cases),
                     total_cases = replace.zero(total_cases),
                     new_deaths = replace.zero(new_deaths),
                     total_deaths = replace.zero(total_deaths),
                     tests_per_case = replace.zero(tests_per_case),
                     new_vaccinations = replace.zero(new_vaccinations),
                     population_density = replace.mean(population_density),
                     median_age = replace.mean(median_age),
                     gdp_per_capita = replace.mean(gdp_per_capita),
                     extreme_poverty = replace.mean(extreme_poverty),
                     human_development_index = replace.mean(human_development_index),
                     handwashing_facilities = replace.mean(handwashing_facilities),
                     male_smokers = replace.mean(male_smokers),
                     female_smokers = replace.mean(female_smokers),
                     population = replace.mean(population))

