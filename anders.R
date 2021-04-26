library(tidyverse)
library(rmarkdown) # paged_table



states <- map_data("state")

# Takes a while
acc_raw <- read.csv('US_Accidents_Dec20.csv')
acc <- acc_raw


str(acc)

nrow(acc) # 4232541

# Sample
smpl <- sample_n(acc, 100000)

states %>% ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group)) +
  geom_point(data=smpl, aes(x=Start_Lng, y=Start_Lat), color='magenta2', size=0.02, alpha=0.2) +
  coord_map()


# install.packages('mapproj')
# First: Map of accidents as dots
states %>% ggplot(aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group)) +
  geom_point(data=acc, aes(x=Start_Lng, y=Start_Lat), color='magenta2', size=0.02, alpha=0.005) +
  coord_map()



# Choropleth map time
# Use State property of acc, but its abbreviated
abbrvs <- read.csv('state_abbrvs.csv', fileEncoding="UTF-8-BOM")
abbrvs

acc <- acc %>% mutate(Code=State) %>% select(-State)

#acc <- acc %>% left_join(abbrvs, by=c('State'='Code'))
acc <- acc %>% left_join(abbrvs, by='Code')

acc <- acc %>% mutate(State=tolower(State))

acc_states <- acc %>% group_by(State) %>%
  summarize(total_accidents=n())

acc_map <- acc_states %>% left_join(states, by=c('State'='region'))

# Total accidents
p2 <- ggplot(acc_map, aes(x=long, y=lat)) + geom_polygon(aes(group=group, fill=total_accidents)) + coord_map() + labs(fill='Total Accidents')
p2 + scale_fill_gradient(low="#222222", high="magenta2")

# Now for accidents per capita
# We choose 2019 for example
library(lubridate)

smpl <- sample_n(acc, 1000)
acc2019 <- smpl %>% 
  mutate(Time=ymd_hms(Start_Time), Year=year(Time)) %>%
  filter(Year==2019)
  #separate(Start_Time, into=c('Date', 'Time'), sep=' ') %>%
  #mutate(Date=ymd(Date))

str(acc2019)


# Clean census
census_raw <- read.csv('census2010-2019.csv')
str(census)
names(census)
census <- census_raw %>%
  mutate(across(Census:X2019, function(x){as.numeric(gsub(",", "", x))})) %>%
  separate(Geographic.Area, sep=', ', into=c('City', 'State'))

str(census)
census_states <- census %>% 
  group_by(State) %>% 
  summarize(
    pop2010=sum(X2010),
    pop2011=sum(X2011),
    pop2012=sum(X2012),
    pop2013=sum(X2013),
    pop2014=sum(X2014),
    pop2015=sum(X2015),
    pop2016=sum(X2016),
    pop2017=sum(X2017),
    pop2018=sum(X2018),
    pop2019=sum(X2019)
  )
census_states

# Now use acc2019 and census_states
smpl <- sample_n(acc, 1000)
acc_yr <- smpl %>% 
  mutate(Time=ymd_hms(Start_Time), Year=year(Time))
acc_yr <- acc %>% 
  mutate(Time=ymd_hms(Start_Time), Year=year(Time))

# FIXME
acc2019 <- acc_yr %>% filter(Year==2019)
str(acc2019)

census_states <- census_states %>% mutate(State=tolower(State))
acc2019 <- acc2019 %>% left_join(census_states, by='State')

str(acc2019)

acc2019 %>% group_by(State) %>% summarize(n=n())

acc2019_states <- acc2019 %>% group_by(State) %>%
  summarize(total_accidents=n(), capita=first(pop2019), total_yearly_accidents_per_capita=n()/first(pop2019))

head(acc2019_states)
acc2019_states %>% filter(State=='iowa')
str(acc2019_states)

acc_map_capita <- acc2019_states %>% left_join(states, by=c('State'='region'))


# Total accidents
ggplot(acc_map_capita, aes(x=long, y=lat)) + geom_polygon(aes(group=group, fill=total_yearly_accidents_per_capita)) + coord_map()

#acc_map_per1000 <- acc_map_capita %>% mutate(per1000=total_yearly_accidents_per_capita*1000)
#ggplot(acc_map_per1000)
acc2019_states %>%
  arrange(total_yearly_accidents_per_capita)

col <- c(rep(c('grey50'), 8), c('red'), rep(c('grey50'), 40))
col <- rep(c('grey50'), 49)
#col <- rep(c('red'), nrow(acc2019_states))

acc2019_states %>% 
  mutate(State=str_to_title(State)) %>%
  mutate(per1000=total_yearly_accidents_per_capita*1000) %>%
  arrange(total_yearly_accidents_per_capita) %>%
  mutate(State=factor(State, levels=State)) %>%
  ggplot(aes(x=State, weight=per1000, fill=State)) + geom_bar() + coord_flip() +
  scale_fill_manual(values=col) +
  ylab('Accidents per 1000 People in 2019')

acc_map_per1000 <- acc_map_capita %>% mutate(`Yearly Accidents per 1000 People`=total_yearly_accidents_per_capita*1000)
p1 <- ggplot(acc_map_per1000, aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group, fill=`Yearly Accidents per 1000 People`)) + 
  coord_map() + ggtitle('Per Capita Accidents in 2019')

p1 + scale_fill_gradient(low="#222222", high="magenta2")
#pivot_wider()
