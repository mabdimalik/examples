library(tidyverse)
library(lubridate)
library(scales)

# Location of data files
url_confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
url_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
url_recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

# Read the data from the web using the readr package and base function url 
cases_data <- read_csv(url(url_confirmed))
deaths_data <- read_csv(url(url_deaths))
recovered_data <- read_csv(url(url_recovered))

# Check data dimensions - get the column size
col_n <- dim(cases_data)[2] 

# Wrangle the data in readiness for the visualization
Cases <- colSums(cases_data[, 5:col_n])
Deaths <- colSums(deaths_data[, 5:col_n])
Recovered <- colSums(recovered_data[, 5:col_n])
Date <- mdy(names(cases_data)[5:col_n])

coronavirus <- data.frame(Date, Recovered, Deaths, Cases) %>% 
    mutate(Cases = Cases - (Recovered + Deaths)) %>% # Current cases only
    pivot_longer(2:4, names_to = "covid", values_to = "people")


# Visualize the coronavirus data
coronavirus$covid <- factor(coronavirus$covid, levels = c( "Recovered", "Deaths", "Cases"))
ggplot(data=coronavirus, aes(x=Date, y=people, group=covid, fill=covid)) +
    geom_area() +
    scale_fill_manual(values = c("#8DB87C","#E36A3B", "#f6cb2f"), name = "People") + 
    scale_y_continuous(labels = comma, breaks = c(0, 30000, 60000, 90000, 120000)) +
    ggtitle("Number of people affected by coronavirus, deaths and recoveries.") + 
    theme_minimal() + 
    theme(legend.position = "top", 
          panel.grid.minor = element_blank(),
          title = element_text(face = "bold")) + 
    xlab("") + ylab("")

# Save the plot with the desired dimensions.
ggsave("coronavirus-area-chart.png", width = 20, height = 20, units = "cm")

