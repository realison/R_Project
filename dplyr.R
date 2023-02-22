library(tidyverse)
library(purrr)
library(ggplot2)
library(dplyr)
rm(list = ls())
graphics.off()

install.packages('nycflights13')
library(nycflights13)

View(airlines)
View(airports)
View(planes)
View(weather)
View(flights)


flights %>% colnames()

df <- flights %>% 
  inner_join(x=.,
             y=airlines,
             by = c("carrier"="carrier")) %>% 
  rename(carrier_name = name)


df %>% 
  count(carrier_name) %>% 
  arrange(desc(n))

df.planes <- planes %>% 
  rename(year_plan = year)



df.all <- flights %>% 
  left_join(x=.,
            y=airlines,
            by="carrier") %>% 
  rename(carrier_name = name) %>% 
  left_join(x=.,
            y=airports,
            by=c("dest"="faa")) %>% 
  rename(dest_name = name) %>% 
  left_join(x=.,
            y=df.planes,
            by=c("tailnum"="tailnum")) %>% 
  left_join(x=.,
            y=weather,
            by = c("origin","year","month","day","hour"))

df.flight.counts <- flights %>% 
  count(tailnum)

df.planes <- df.flight.counts %>% 
  right_join(x.,
             y = planes,
             by ="tailnum") %>% 
  rename(`number of flights` = n)


airlines1 <- airlines %>% 
  filter(carrier %in% c("AA","VX","DL"))


semi_join(x=airlines1,
           y = flights,
           by = "carrier")
semi_join(x=flights,
          y = airlines1,
          by ="carrier")



df <- flights %>% 
  filter(carrier == "AA") %>% 
  arrange(time_hour)
df <- df %>% 
  mutate(`origin prev flight` = lag(x = origin,n=1)) %>% 
  mutate(`origin test`= case_when(origin == `origin prev flight` ~TRUE,
                                  T~FALSE))

df <-  df %>% 
  mutate(`distance successive flights` = distance + lead(x = distance,n =1 )) %>% 
  mutate(`distance test` = case_when(`distance successive flights` >= 2000 ~ TRUE,
                                      T~ FALSE))


df %>%  filter(`distance test`) %>% count()

df <- df %>% 
  mutate(`distance running tot` = cumsum(distance))

df %>% 
  mutate(`flight id`=row_number()) %>% 
  filter(`distance running tot` >= 1000000) %>% 
  select(`flight id`,everything()) %>% 
  head(1) %>% 
  as.data.frame()

## rank flights

df %>% 
  mutate(`rank flight`=dense_rank(distance)) %>% 
  filter(`rank flight`<=10) %>% 
  head(1) %>% 
  as.data.frame()

flights
airlines

# Exercise 1

flights %>% 
  left_join(x=.,
            y = planes,
            by = "tailnum") %>% 
  left_join(x.,
            y= airlines,
            by = "carrier") %>% 
  mutate(`plane info` = model,
         `carrier name` = name) %>% 
  group_by(manufacturer,`carrier name`) %>% 
  count(`plane info`) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x=manufacturer,
             y = n,
             fill = `carrier name`)) +
  geom_bar(stat= 'identity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
# Exercise 2

weather


ex2 <- flights %>% 
  left_join(x=.,
            y=weather,
            by = c('origin','year','month','day','hour')) %>% 
  select(arr_delay,temp, dewp, humid,
         wind_dir, wind_speed, wind_gust, precip, pressure,
         visib)

model <- lm(arr_delay~temp+dewp+humid+
            wind_dir+ wind_speed+ wind_gust+ precip+pressure+
            visib,data = ex2)
summary(model)

model %>% pluck(coefficients) %>%  enframe() %>% arrange(desc(value)) %>% 
  tail(9) %>% 
  ggplot(aes(x = name,
             y = value)) +
  geom_bar(stat = 'identity')



## scatter plot

ex2 %>% 
  mutate(delay = case_when(arr_delay >30 ~ TRUE,
                           T ~ FALSE)) %>% 
  sample_n(size = 50000) %>% 
  ggplot(aes(x = visib,
             y = precip,
             size = wind_speed,
             color = arr_delay)) +
  geom_jitter()+
  scale_size_area(max_size = 10) +
  scale_color_viridis_c(option = 'magma')+
  facet_wrap(.~delay)
 
model2 <- ex2 %>% 
  mutate(delay = case_when(arr_delay >30 ~ 1,
                           T ~ 0)) 


  
  
  
  
model2.1 <- glm(delay~temp+dewp+humid+
                wind_dir+ wind_speed+ wind_gust+ precip+pressure+
                visib,data=model2,family = binomial )
model2.1
summary(model2.1)


model2.1 %>% 
  pluck(coefficients) %>%  enframe() %>% arrange(desc(value)) %>% 
  tail(9) %>% 
  ggplot(aes(x = name,y = value))+
  geom_bar(stat='identity')


















