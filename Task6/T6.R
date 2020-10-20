library(ggplot2)
library(dplyr)
library(dygraphs)
library(lubridate)
library(tidyverse)
library(gganimate)
library(gifski)
library(av)
library(gapminder)
data<-read.csv("owid-covid-data.csv")
I<-data %>% filter(location=="India")
India_covid<-data %>% filter(location=="India")
India_covid<-data.frame(cases=India_covid$new_cases_smoothed_per_million,deaths=India_covid$new_deaths_smoothed_per_million,gdp=India_covid$gdp_per_capita)
India_covid<-na.omit(India_covid)
cases<-ts(India_covid$cases)
deaths<-ts(India_covid$deaths)
India<-cbind(cases,deaths)

dygraph(India,main="Cases per Million Vs Deaths per Million")%>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE)

total<-data %>% filter(location=="India")
total<-ts(na.omit(total$total_deaths))

dygraph(total,main="Total Deaths") %>%   dySeries("V1", label = "Deaths") %>%
  dyLegend(show = "follow") %>% dyRangeSelector()

new_cases<-ts(na.omit(I$new_cases))
new_deaths<-ts(na.omit(I$new_deaths))

dygraph(new_cases,main="New Cases",group = "a") %>% dySeries("V1", label = "Cases")
dygraph(new_deaths,main="New Deaths",group = "a") %>% dySeries("V1", label = "Deaths")

head(data$date)
new_data<-data %>% filter(location=="India") %>% group_by(date)
new_data<-summarise(new_data,new=replace_na(new_cases,0),cuml=cumsum(new))
head(new_data)
new_data %>% ggplot(aes(x = date, y = cuml)) +geom_point(size=0.5,color="red") +
  geom_line()+
  ggtitle("Daily Cumilative Cases")+transition_reveal(cuml)



