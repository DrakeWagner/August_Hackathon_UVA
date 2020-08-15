library(tidyverse)
library(libraryscales)

#world_test <- filter(main_data, location == "World") Used this to see if World is a country

#read in the data from downloaded CSV, found at https://github.com/owid/covid-19-data/tree/master/public/data
main_data <- read.csv("owid-covid-data.csv")

#Trim the overall dataset to only the columns needed for both graphs
trim_data <- main_data %>% 
  select("iso_code", "location", "date", "total_cases", "total_cases_per_million")

#reduce the dataset to the countries needed for the line graph
line_graph_countries <- filter(trim_data, location == c("World", "United States", "United Kingdom", "South Korea", "China"))

#change the data type of the data column from factor to date
line_graph_countries$date <- as.Date(line_graph_countries$date)

#filter down to needed dates... this should be the final dataset that we have
line_graph_data <- line_graph_countries %>%
  filter(date > "2020-01-21" & date < "2020-05-08" ) 

line_graph_data <- line_graph_data %>%
  mutate(log_cases_per_million = log10(line_graph_data$total_cases_per_million))

#create line graph using ggplot

ggplot(line_graph_data, mapping = aes(x= date, y =total_cases_per_million, line_type = location, color = location)) + 
  geom_line() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x)) + scale_x_date(breaks = "1 month") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color='gray', linetype=2),
        panel.background = element_blank(),
        legend.text = element_text(),
        legend.key = element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=7)) +
  labs(title='Total confirmed COVID-19 cases per million people',
       subtitle='The number of confirmed cases is lower than the number of total cases. The main reason for this is limited testing.',
       x='Date',
       y='LOG')

# Plot 2
# install mapping packages and data from Natural Earth 
library("rnaturalearth")
library("rnaturalearthdata")
library('rgeos')
library('plotly')

#trim data down to May 8 only (all countries)
may8_data <- filter(trim_data, date == "2020-05-08")

#create 
fig <- plot_ly(may8_data, type='choropleth', 
               locations = may8_data$iso_code, 
               z=may8_data$total_cases_per_million, 
               text=may8_data$location, colorscale="D3")
fig <- fig %>%
  colorbar(title='Total Cases per Million')

fig



