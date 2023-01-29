#https://mdl.library.utoronto.ca/technology/tutorials/covid-19-data-r
#https://rpubs.com/khusniank/SMAADWprac2

options(scipen=999)

#Loading libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(corrplot)
library(naniar)
library(plotly)
library(data.table)           #for fread()
library(plyr)
library(scales)
library(cowplot)
library(lubridate)
library(crosstable)
library(kableExtra)
library(DT)
library(caret)                #for createdatapartition()
library(forcats)
library(TTR)                  #for SMA() 
library(tidyr)
library(jtools)               #for effect_plot()

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
covid.data1 <- covid.data %>%
  mutate_at(vars(new_cases, total_cases, new_deaths, total_deaths, tests_per_case, new_tests, reproduction_rate, icu_patients, hosp_patients, people_fully_vaccinated),
            ~replace_na(.,
                        0)) %>%
  group_by(location) %>%
  mutate(human_development_index = replace_na(human_development_index, mean(human_development_index, na.rm = T)),
         population_density = replace_na(population_density, mean(population_density, na.rm = T)),
         gdp_per_capita = replace_na(gdp_per_capita, mean(gdp_per_capita, na.rm = T)),
         extreme_poverty = replace_na(extreme_poverty, mean(extreme_poverty, na.rm = T)),
         male_smokers = replace_na(male_smokers, mean(male_smokers, na.rm = T)),
         female_smokers = replace_na(female_smokers, mean(female_smokers, na.rm = T)),
         icu_patients = replace_na(icu_patients, mean(icu_patients, na.rm = T)),
         hosp_patients = replace_na(hosp_patients, mean(hosp_patients, na.rm = T)),
         handwashing_facilities = replace_na(handwashing_facilities, mean(handwashing_facilities, na.rm = T)),
         median_age = replace_na(median_age, mean(median_age, na.rm = T)),
         stringency_index = replace_na(stringency_index, mean(stringency_index, na.rm = T))) %>%
  ungroup()

colSums(is.na(covid.data1))
#changing date variable into date format
covid.data1$date <- as.Date(covid.data1$date, format = "%Y-%m-%d")

#Removing continents from country column
continents <- c("Asia", "Africa", "European Union", "Europe", "High income", "Lower middle income", "Low Income", "Upper middle income", 'Oceania', "South America", "North America", "International", "World")
covid <- subset(covid.data1, !(location %in% continents))

#Removing the blank spaces from continent column
covid <- subset(covid, !(continent == ""))

#latest day
day_latest <- max(covid$date)

#creating heatmaps
covid.cases <- covid %>%
  group_by(location) %>%
  filter(date == max(date))

#creating covid cases heat maps
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
         title = paste0("World COVID-19 Confirmed Cases till ", day_latest-1)) %>%
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
# covid.vaccination <- covid %>%
#   group_by(location) %>%
#   filter(people_fully_vaccinated == max(people_fully_vaccinated)) %>%
#   select(date, location, people_fully_vaccinated, iso_code)
# 
# plot_geo() %>%
#   layout(geo = heatmap,
#          paper_bgcolor = '#e8f7fc',
#          title = paste0("Vaccination status till ",  day_latest)) %>%
#   add_trace(data = covid.vaccination,
#             z = ~people_fully_vaccinated,
#             colors = "Reds",
#             text = ~location,
#             locations = ~iso_code,
#             marker = list(line = line))

#summary statistics
#creating month column
year.month <- covid %>%
  mutate(Month = as.character(lubridate::month(date)),
         Year = lubridate::year(date)) %>%
  select(continent, location, Year, Month, new_cases, new_deaths)

#recoding the month column
year.month$Month <- recode(year.month$Month, 
                      "1" = "January",
                      "2" = "February",
                      "3" = "March",
                      "4" = "April",
                      "5" = "May",
                      "6" = "June", 
                      "7" = "July",
                      "8" = "August",
                      "9" = "September",
                      "10" = "October",
                      "11" = "November",
                      "12" = "December")
#covid %>%
 # filter(location == "Afghanistan") %>%
  #summarize(total_cases = sum(total_cases))

covid_summary_statistics <- year.month %>%
  group_by(location, Month, Year) %>%
  dplyr::summarise(Total.Deaths = sum(new_deaths, na.rm = T),
         Total.Cases = sum(new_cases, na.rm = T),
         Mean.cases.per.day = round(mean(new_cases, na.rm =T),2),
         Mean.deaths.per.day = round(mean(new_deaths, na.rm =T),2))

covid_summary_statistics %>%
  select(location, Year, Month, Total.Cases, Total.Deaths, Mean.cases.per.day, Mean.deaths.per.day) %>%
  datatable(
    rownames = F,
    class = "cell-border stripe",
    colnames = c("Country", "Year", "Month", "Total cases", "Total Deaths", "Mean cases per day", "Mean Deaths per day"),
    caption = "Country wise COVID-19 Cases and Deaths",
    options = list(columnDefs = list(list(className = "dt-center", targets = 0:1)))
  )

#Trend of world covid cases and deaths
covid %>%
  group_by(date) %>%
  filter(date != day_latest) %>%
  dplyr::summarise(total_deaths = sum(total_deaths, na.rm = T), 
                   total_cases = sum(total_cases, na.rm = T), .groups = "drop") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = total_cases + 1), color = "#2e9449", linewidth = 1.5) +
  geom_line(aes(y = total_deaths + 1), linewidth = 1.5, linetype = 2, color = "#9c2742") +
  scale_y_continuous(trans = "log10", labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(title = "Global COVID infections and deaths",
       subtitle = paste0("Till ", day_latest - 1),
       x = "",
       y = "Log10 transformation") +
  theme_apa() +
  theme(axis.text.x = element_text(angle = 90, color = "black", hjust = 1),
        axis.text = element_text(color = "black")) +
  geom_vline(xintercept = as.Date("2020-03-11"), linetype = "longdash", linewidth = 0.8, col = "black") +
  annotate("text", x = as.Date("2020-03-10"), y = 11100, label = "WHO announces pandemic \n", size = 4.2, angle = 90) +
  geom_vline(xintercept = as.Date("2020-01-30"), linetype = "longdash", linewidth = 0.8, col = "black") +
  annotate("text", x = as.Date("2020-01-20"), y = 16100, label = "Global health emergency declared \n", size = 4.2, angle = 90) +
  annotate("text", x = as.Date("2021-05-05"), y = 1000000, label = "Total Deaths \n", size = 4.2) +
  annotate("text", x = as.Date("2021-05-05"), y = 50000000, label = "Total Cases \n", size = 4.2)

#Trend of new covid cases in different continents
p1 <- covid %>%
  group_by(date, continent) %>%
  dplyr::summarise(new_covid_cases = sum(new_cases, na.rm = T), .groups = "drop") %>%
  ggplot(aes(date)) +
  geom_col(aes(y = new_covid_cases, color = continent)) +
  labs(
    title = "Trend of New COVID-19 cases in different continents",
    subtitle = paste0("Till ", day_latest - 1),
    y = "",
    x = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 0.6)
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  facet_wrap(~continent)
p1

#Trend of new covid deaths in different continents
p2 <- covid %>%
  group_by(date, continent) %>%
  dplyr::summarise(new_covid_deaths = sum(new_deaths, na.rm = T)) %>% 
  ggplot(aes(date)) +
  geom_col(aes(y = new_covid_deaths, color = continent)) +
  labs(
    title = "Trend of New COVID-19 deaths in different continents",
    subtitle = paste0("Till ", day_latest - 1),
    y = "",
    x = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 0.6)
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  facet_wrap(~continent)
p2
                                   

#COVID cases by months
month.df <- year.month %>%
  group_by(Month, continent) %>%
  dplyr::summarise(total.cases = sum(new_cases, na.rm = T),
                   total.deaths = sum(new_deaths, na.rm = T))

month.df$Month <- factor(month.df$Month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

p3<-ggplot(month.df, aes(x = Month, y = total.cases, fill = continent)) +
  geom_col(position = "dodge") +
  # facet_wrap(~continent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.2),
        axis.text = element_text(size = 9, color = "black")) +
  labs(
    x = "",
    y = "Total Cases",
    title = "COVID - 19 cases in different continents in different months",
    subtitle = paste0("Till ", day_latest-0)
  ) +
  scale_y_continuous(label = comma) +
  coord_flip()
ggplotly(p3)

#Top 10 countries with most covid cases
top.10.covid.countries <- covid %>%
  select(location, new_cases, date) %>%
  group_by(location) %>%
  dplyr::summarise(total.cases = sum(new_cases, na.rm = T)) %>%
  top_n(10, total.cases) %>%
  arrange(desc(total.cases)) %>%
  mutate(country.reordered = fct_reorder(location, total.cases))

p4 <- top.10.covid.countries %>%
  ggplot(aes(country.reordered, total.cases)) +
  geom_col() +
  geom_text(aes(label=total.cases), hjust = -0.1, size = 3) +
  scale_y_continuous(label = comma) +
  labs(
    x = "Total Cases",
    y = "",
    title = "Top 10 countries with highest COVID-19 infections",
    subtitle = paste0("Till ", day_latest-1)
  ) +
  theme_bw() +
  coord_flip()
p4

#Top 10 countries with most COVID-19 deaths
top.10.covid.deaths.countries <- covid %>%
  select(date, location, new_deaths) %>%
  group_by(location) %>%
  dplyr::summarise(total.deaths = sum(new_deaths, na.rm = T)) %>%
  top_n(10, total.deaths) %>%
  arrange(desc(location)) %>%
  mutate(country.reordered = fct_reorder(location, total.deaths))
p5 <- top.10.covid.deaths.countries %>%
  ggplot(aes(country.reordered, total.deaths)) +
  geom_col() +
  geom_text(aes(label = total.deaths), hjust = -0.1, size = 3) +
  scale_y_continuous(label = comma) +
  labs(x = "",
       y = "Total Deaths",
       title = "Top 10 countries with most COVID-19 deaths",
       subtitle = paste0("Till ", day_latest-1)) +
  theme_bw() +
  coord_flip()
p5

#Selecting certain columns of covid.data 
covid.data.corr <- covid %>%
  select(new_cases, new_deaths, tests_per_case, new_tests, reproduction_rate, icu_patients, hosp_patients, human_development_index, gdp_per_capita, extreme_poverty, male_smokers, female_smokers, handwashing_facilities)

#Plotting the correlation matrix
cor <- cor(covid.data.corr)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor, method = 'color', 
         type = "upper", #Displays only upper part of the matrix
         order = "hclust",
         col=col(200),
         addCoef.col = "black",  #Add coeffiecient of correlation
         number.cex = 0.8,
         tl.col="black",  #Text label color
         tl.srt=90,  #Text label rotation
         diag = FALSE,
         sig.level = 0.01, insig = "blank")

#Scatter plots
## covid cases vs HDI
p6 <- covid %>%
  filter(date == max(day_latest - 1)) %>%
  select(total_cases, human_development_index, location, continent) %>%
  mutate(location = factor(location)) %>%
  ggplot(aes(human_development_index, total_cases/1000, size = total_cases, color = continent)) +
  geom_point(aes(text = location), alpha = 0.5) +
  scale_size(range=c(1,10), name = "") +
  theme_bw() +
  labs(
    x = "Human Development Index",
    y = "Total cases in thousands"
  )
   
ggplotly(p6, tooltip = c("text", "total_cases", "human_development_index")) %>%
  layout(title = list(text = paste0('Total Covid cases in different countries',
                                    '<br>',
                                    '<sup>',
                                    'Size of scatter plot denotes the covid cases','</sup>')),
         margin = list(t = 70))

## covid deaths vs HDI
p7 <- covid %>%
  filter(date == max(day_latest - 1)) %>%
  select(total_deaths, human_development_index, location, continent) %>%
  mutate(location = factor(location)) %>%
  ggplot(aes(human_development_index, total_deaths/1000, size = total_deaths, color = continent)) +
  geom_point(aes(text = location), alpha = 0.5) +
  scale_size(range=c(1,10), name = "") +
  theme_bw() +
  labs(
    x = "Human Development Index",
    y = "Total deaths in thousands"
  )
 
ggplotly(p7, tooltip = c("text", "total_deaths", "human_development_index")) %>%
  layout(title = list(text = paste0('Total Covid deaths in different countries',
                                    '<br>',
                                    '<sup>',
                                    'Size of scatter plot denotes the covid deaths','</sup>')),
         margin = list(t = 70))

## covid cases vs covid deaths
p8 <- covid %>%
  filter(date == max(day_latest - 1),
         location != "North Korea") %>%
  select(total_deaths, total_cases, location, continent) %>%
  mutate(location = factor(location),
         infection_fatality_rate = (total_deaths/total_cases)*100) %>%
  ggplot(aes(total_cases/1000, total_deaths/1000, size = infection_fatality_rate, color = continent)) +
  geom_point(aes(text = location), alpha = 0.45) +
  scale_size(range=c(1,10), name = "") +
  theme_bw() +
  labs(
    x = "Total cases in thousands",
    y = "Total deaths in thousands"
  )

ggplotly(p8, tooltip = "all") %>%
  layout(title = list(text = paste0('Total Covid cases vs total deaths in different countries',
                                    '<br>',
                                    '<sup>',
                                    'Size of bubble is based on infection fatality rate','<br>',
                                    'Infection mortality rate vary depending on fairness of reported covid cases and deaths', '</sup>')),
         margin = list(t = 100))

#In Nepal?
##total cases and total deaths
covid.nepal <- covid %>%
  filter(location == "Nepal") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = total_cases + 1), color = "#2e9449", linewidth = 1) +
  geom_line(aes(y = total_deaths + 1), linewidth = 1, linetype = 2, color = "#9c2742") +
  scale_y_continuous(trans = "log10", labels = comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(title = "Global COVID infections and deaths",
       subtitle = paste0("Till ", day_latest - 1),
       x = "",
       y = "Log10 transformation") +
  theme_apa() +
  theme(axis.text.x = element_text(angle = 90, color = "black", hjust = 1),
        axis.text = element_text(color = "black")) +
  annotate("text", x = as.Date("2021-05-05"), y = 9000, label = "Total Deaths \n", size = 3.8) +
  annotate("text", x = as.Date("2021-05-05"), y = 450000, label = "Total Cases \n", size = 3.8)  
  
ggplotly(covid.nepal) %>%
  layout(title = list(text = paste0('Global COVID infections and deaths',
                                    '<br>',
                                    '<sup>',
                                    paste0('TIll ',day_latest-1),'</sup>')),
         margin = list(t = 50))

##data manipulation for trend of new cases and new deaths in nepal
covid.nepal1 <- covid %>%
  filter(location == "Nepal") %>%
  mutate(new.cases.smoothed  = as.integer(SMA(new_cases, n = 14)),
         new.deaths.smoothed = as.integer(SMA(new_deaths, n = 14))) %>%     #as.integer is used to convert numbers with decimals into integers
  select(date, new.cases.smoothed, new.deaths.smoothed) %>%
  filter(date > "2020-02-06")     #To remove rows with NAs induced due to smoothing (14 days simple moving average)

covid.nepal1 %>%
  filter(new.deaths.smoothed == 190)

##trend of new covid cases in nepal
p9 <- covid.nepal1 %>%
  ggplot(aes(date, new.cases.smoothed)) +
  geom_line(linewidth = 1, color = "#2C8C33") +
  labs(x = "",
       y = "Number of new cases",
       title = "Trend of New Covid infections in Nepal (14-days smoothed)",
       subtitle = paste0("Till ", day_latest - 1)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  annotate(geom="text", x=as.Date("2022-03-15"), y=8289, 
           label="Per day new covid cases reached\nmaximum i.e. 8,589", size = 3.5) +
  annotate(geom="point", x=as.Date("2021-05-18"), y=8589, size=6, shape=21, fill="transparent")

p9

## trend of new covid deaths in nepal
p10 <- covid.nepal1 %>%
  ggplot(aes(date, new.deaths.smoothed)) +
  geom_line(linewidth = 1, color = "#CF5C5C") +
  labs(x = "",
       y = "Number of new deaths",
       title = "Trend of New Covid related deaths in Nepal (14-days smoothed)",
       subtitle = paste0("Till ", day_latest - 1)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  annotate(geom="text", x=as.Date("2022-03-25"), y=185, 
           label="Per day new covid deaths reached\nmaximum i.e. 190", size = 3.5) +
  annotate(geom="point", x=as.Date("2021-05-24"), y=189, size=6, shape=21, fill="transparent")

p10

#Polynomial Regression model
nepal.df <- covid %>%
  filter(location == "Nepal") %>%
  select(new_tests, new_cases, stringency_index)
plot(nepal.df$new_tests, nepal.df$new_cases)

#Identifying the outliers position in new_tests
out <- boxplot.stats(nepal.df$new_tests)$out
out_ind <- which(nepal.df$new_tests %in% c(out))
out_ind
str(nepal.df)
#Removing the outliers
nepal.df1 <- nepal.df[-out_ind,]
str(nepal.df1)

#Regression model
model <- lm(new_cases ~ poly(new_tests,2) + stringency_index, data = nepal.df1)
summary(model)

residplot <- function(fit, nbreaks=15) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(model)
# pg 200

effect_plot(model, pred = new_tests, interval = T, plot.points = T, line.colors = "#4669E8")
#https://cran.r-project.org/web/packages/jtools/vignettes/effect_plot.html
#https://stats.stackexchange.com/questions/233007/interpreting-effects-plots-in-r
