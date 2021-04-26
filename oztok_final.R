#How weather & road properties affect accidents
  #Number of accidents in different types of weather
  #Group by region and weather, graph number of accidents (or severity)

#LOAD THE DATA AND IMPORT
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggrepel)
states <- map_data("state")
accidents <- read.csv("us_accidents_df.csv", stringsAsFactors = FALSE)
View(accidents)

#A frequency table for severity
severity_count <- c(sum(accidents$Severity==1), sum(accidents$Severity==2), sum(accidents$Severity==3), sum(accidents$Severity==4))
freq <- severity_count/sum(severity_count )
barplot(freq, names.arg = 1:4, main = "Relative Frequencies of Severity")

#Analyze temperature
hist(accidents$Temperature.F., breaks=seq(-90, 205, 5), xlab = "Temperature (Fahrenheit)", main = "Histogram of Temperature")


#Analyze wind chill
hist(accidents$Wind_Chill.F., breaks=seq(-90, 190, 5), xlab = "Wind Chill (Fahrenheit)", main = "Histogram of Wind Chill")

#Analyze visibility
temp4 <- accidents$Visibility.mi.
temp4 = temp4[temp4 < 12]
hist(temp4, breaks=seq(0,12,2), xlab = "Visibility (miles)", main = "Histogram of Visibility")

#Analyze the weather condition by severity
weather_cond<-table(accidents$Weather_Condition, accidents$Severity)
weather_cond
weather_cond_df<-data.frame(weather_cond)
colnames(weather_cond_df)<-c("Weather_Condition","Severity","Accidents")
weather_cond_df <- weather_cond_df %>% 
  filter(Severity == 4) %>% 
  arrange(desc(Accidents)) %>% 
  filter(Accidents > 100)



b <- ggplot(data=weather_cond_df, aes(x=Weather_Condition, y=Accidents)) + geom_bar(stat="identity") + coord_flip()
b









#Weather condition does not have a strong impact on accident severity