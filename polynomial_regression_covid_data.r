#Loading libraries
pacman::p_load(ggplot2, tidyverse, dplyr, corrplot, naniar, plotly, data.table)
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
