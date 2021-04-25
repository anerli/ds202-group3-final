library(tidyverse)
library(rmarkdown) # paged_table

census <- read.csv('census2010-2019.csv')
str(census)
names(census)
census <- census %>%
  mutate(across(Census:X2019, function(x){as.numeric(gsub(",", "", x))}))
