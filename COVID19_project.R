rm(list=ls())
graphics.off()
install.packages('lubridate')
install.packages('rio')
install.packages('janitor')
install.packages('tidyverse')
install.packages('cowplot')
install.packages('dplyr')
install.packages('purrr')
install.packages('ggplot2')
library(tidyverse)
library(lubridate)
library(cowplot)
library(rio)
library(janitor)
library(ggplot2)
library(purrr)


# import data

path.origin <- '~/Documents/Rstudio/data/'

data.dir.name.COVID19 <- list.files(path = path.origin) %>% 
  str_subset("^csse_")

df.covid19 <- tibble(directory = paste0(path.origin,data.dir.name.COVID19),
                     file = list.files(path = directory)) %>% 
  mutate(path = str_c(directory,file,sep="/")) %>% 
  mutate(data = purrr::map(.x = path,
                    .f = function(path_){read_csv(path_,
                                                  col_types = cols(.default = "c"))})) %>% 
  mutate(date = str_remove(string = file,pattern = "\\.csv"),
         date = mdy(date)) %>% 
  select(date,data) %>% 
  unnest(cols = "data") %>% 
  clean_names() #clean colnames very practical in janitor


df.covid19 <- df.covid19 %>% 
  mutate (last_update = ymd_hms(last_update)) %>% 
  rename(state = province_state) %>% 
  mutate_at(.tbl = .,
            .vars = setdiff(colnames(.)[5:ncol(.)],"iso3"), #setdiff is to find the subset inthe first element not in the second element
            .funs = as.numeric)


# GDP data


data.dir.name.gdp <- list.files(path = path.origin) %>% 
  str_subset("GDP")


df.gdp <- rio::import(file = paste0(path.origin,data.dir.name.gdp),
                      sheet = "clean data") %>% 
  clean_names()

## columns selection and cleaning

df.gdp <-  df.gdp %>% 
  select(state = state_or_territory,
         gdp_nominal = nominal_gdp_2020,
         gdp_per_capita= gdp_per_capita_2020) %>% 
  mutate_all(.,.funs = str_remove_all,",") %>% 
  mutate_all(.,.funs = str_remove_all,"\\$") %>% 
  mutate_at(.,
            .vars = colnames(.)[2:3],
            .funs = as.numeric)


## population

data.dir.name.pop <- list.files(path = path.origin) %>% 
  str_subset("Population")

df.pop <- rio::import(file = paste0(path.origin,data.dir.name.pop),
                      sheet = "data") %>% 
  clean_names()

## column selection and cleaning 


df.pop <- df.pop %>% 
  select(state = name,
         pop = pop_2019)



data.dir.name.response <- list.files(path=path.origin) %>% str_subset("US_latest")

df.covid19.response <- read_csv(file = paste0(path.origin,data.dir.name.response),
                                col_types = cols(.default = "c")) %>% 
  clean_names()


df.response <-df.covid19.response %>% 
  mutate(date = ymd(date)) %>% 
  select(state = region_name,
         date,
         contains("index")) %>% 
  mutate_at(.tbl = .,
            .vars = colnames(.)[3:ncol(.)],
            .funs = as.numeric)


data.dir.name.vacc <- list.files(path = path.origin) %>% 
  str_subset("vaccine")

df.vacc <- read_csv(file = paste0(path.origin,data.dir.name.vacc),
                    col_types = cols(.default = "c")) %>% 
  clean_names()

df.vacc <- df.vacc %>% 
  mutate(date = ymd(day),
         daily_vaccinations = as.numeric(daily_vaccinations)) %>% 
  select(state = entity,
         date,
         vaccinations = daily_vaccinations)


# initial data inspection

## function for NAs detection 

count_NA <- function(df){
  require(tidyverse)
  require(cowplot)
  
  df.Na.count <- purrr::map(df,~sum(is.na(.))) %>% 
    simplify() %>% 
    tibble(col = names(.),
           NAs = .) %>% 
    mutate(`NA %` = round(NAs/nrow(df)*100,2))
  
  print(df.Na.count %>% as.data.frame())
  
  # absolute na count
  
  p1 <-  ggplot(df.Na.count,aes(x = col,
                                y = NAs))+
    geom_col()+
    theme(axis.text.x = element_text(angle = 90))
  
  # relative Nas count
  
  p2 <-  ggplot(df.Na.count,aes(x = col,
                                y = `NA %`))+
    geom_col()+
    scale_y_continuous(limits = c(0,100))+
    theme(axis.text.x = element_text(angle = 90))
    
 plot_grid(p1,p2,nrow = 2) 
  
}


count_NA(df.covid19)
count_NA(df.gdp)
count_NA(df.vacc)
count_NA(df.pop)
count_NA(df.response)




df.covid19 %>% 
  select(state,date,confirmed,active,recovered,deaths) %>% 
  #filter (state == "California") %>% 
  pivot_longer(cols = c("confirmed","deaths","recovered","active"),
               names_to = "variable",
               values_to = "Value") %>% 
  ggplot(aes(x = date,
             y = Value,
             color = variable))+
  geom_point()+
  facet_grid(variable ~ .,
             scale = "free")
#install.packages('tidystringdist')
library(tidystringdist)

## state name matching

state_matching <-  function(states_base = states.list,
                            data,
                            col_name){
  require(tidystringdist)
  
  col_name <- rlang::ensym(col_name)
  
  states.data <- data %>% distinct(state)
  ## create table of all combinations states pairs
  states.combs <- expand.grid(state_base = states.list %>% pull(state_base),
                              state = states.data %>% pull(state))  
  
  states.distance <- tidy_stringdist(df = states.combs,
                                       v1 = state_base,
                                      v2 = state,
                                      method = "osa") %>% 
    arrange(state_base,osa) %>% 
    group_by (state_base) %>% 
    mutate(rank = row_number()) %>% 
    ungroup() %>% 
    filter(rank == 1) %>% 
    select(state_base,!!col_name := state)
  
  return(states.distance)
}


states.list <- tibble(state_base = datasets::state.name)
states.list
states.list.covid19 <- state_matching(data = df.covid19,col_name = 'state.covid_19')
states.list.gdp <- state_matching(data = df.gdp,col_name = 'state.gdp')
states.list.pop <- state_matching(data = df.pop,col_name = 'state.pop')
states.list.response <- state_matching(data = df.response,col_name = 'state.response')
states.list.vacc <- state_matching(data = df.vacc,col_name = 'state.vacc')


states.list <- states.list.covid19 %>% 
  inner_join(x.,
             y = states.list.gdp,
             by = "state_base") %>% 
  inner_join(x.,
             y = states.list.pop,
             by = "state_base") %>% 
  inner_join(x.,
             y = states.list.response,
             by = "state_base") %>% 
  inner_join(x.,
             y = states.list.vacc,
             by = "state_base") %>% 
  arrange(state_base) %>% 
  mutate(state_id = row_number()) %>% 
  select(state_id, everything())

## name fix

states.list.vacc %>% 
  filter(state_base %in% c("New Jersey","New York"))
states.list[states.list$state_base  == "New York","state.vacc"] <- "New York State"
  
## add state region


rm(state.region)
states.region <- tibble(state_base = state.name,
                        region = state.region)

## states table

df.states <- states.list %>% 
  left_join(x.,
            y = states.region,
            by="state_base")


## relevant table
df.dates <- tibble(date = seq.Date(from = df.covid19 %>% pull(date) %>% min(),
                                   to = df.covid19 %>% pull(date) %>% max(),
                                   by = "day"))
                     

install.packages('maps')
library(maps)
library(purrr)


df.main <- df.states %>% 
  cross_join(x=.,
            y=df.dates)

## check
df.main %>% 
  count(state_base) %>% 
  as.data.frame()

df.main <- df.main %>% 
  left_join(x=.,
            y= df.covid19 %>% select(state,date,confirmed,deaths),
            by = c("state.covid_19" = "state","date" = "date"))%>% 
  left_join(x=.,
            y=df.gdp, 
            by = c("state.gdp" = "state")) %>% 
  
  left_join(x=.,
            y=df.pop, 
            by = c("state.pop" = "state")) %>% 
  left_join(x=.,
            y=df.vacc %>% select(state,date,vaccinations),
            by = c("state.vacc" = "state","date" = "date")) %>% 
  left_join(x=.,
            y=df.response,
            by = c("state.response" = "state","date" = "date")) %>% 
  # remove redundant columns
  select(-c("state.covid_19","state.gdp","state.vacc","state.pop","state.response")) %>% 
  select(state_id,
         state = state_base,
         region = region,
         date,
         `confirmed total` =  confirmed ,
         `deaths total` = deaths,
         `daily vaccine doses` = vaccinations,
         everything() 
  ) %>% 
  rename(pouplation = pop) %>% 
  arrange(state,date)


###  check the missing values

df.main %>% filter(is.na(`confirmed total`)) %>% nrow()
df.main %>% filter(is.na(`deaths total`)) %>% nrow()
df.main %>% filter(is.na(`daily vaccine doses`)) %>% nrow()

### the vacc has many missing values, which means the starting date of data collection could be different

df.vacc.date.min <- df.main %>% 
  filter(!is.na(`daily vaccine doses`)) %>% 
  group_by(state) %>% 
  summarise(min_date = min(date)) %>% 
  ungroup()

df.main <- df.main %>% 
  rename(population = pouplation) %>% 
  mutate(`population in million` = round(population/10**6,2))
df.main <- df.main %>% 
  mutate(`daily vaccine doses` = replace_na(`daily vaccine doses`,0)) %>% 
  group_by(state) %>% 
  # daily counts
  mutate(`confirmed daily cases` = `confirmed total`-lag(`confirmed total`,1),
         `deaths daily cases` = `deaths total`-lag(`deaths total`,1)) %>% 
  #total counts for the vaccine
  mutate(`vaccine doses total` = cumsum(`daily vaccine doses`)) %>% 
  ungroup() %>% 
  #rearrange columns
  select(state_id:date,
         `confirmed total`,`deaths total`,`vaccine doses total`,
         `confirmed daily cases`, `deaths daily cases`,`daily vaccine doses`,everything())
  
### check negative values

df.main %>% filter(`confirmed daily cases` <0)
df.main %>% filter(`deaths daily cases` <0)
df.main %>% filter(`daily vaccine doses` <0)
df.main %>% filter(`vaccine doses total` <0)

### replace the negative numbers with 0

df.main %>% 
  mutate(`confirmed daily cases`=case_when(`confirmed daily cases`>=0 ~ `confirmed daily cases`,
                                           T ~ 0),
         `deaths daily cases`=case_when(`deaths daily cases`>=0 ~ `deaths daily cases`,
                                           T ~ 0),
         `daily vaccine doses`=case_when(`daily vaccine doses`>=0 ~ `daily vaccine doses`,
                                        T ~ 0))



# 4) EDA

## states per region

df.main %>% 
  group_by(region) %>% 
  summarise(states = n_distinct(state)) %>% 
  ungroup()


## show states on map

max.date <- df.main %>% pull(date) %>% max(.)


## states to lower case
df.main <- df.main %>% 
  mutate(state_ = str_to_lower(state))

map.data <- df.main %>% 
  filter(date == max.date) %>% 
  left_join(x=.,
            y = map_data("state"),
            by = c("state_" = "region"),
            multiple = "all") 


map_data("state")

ggplot(data = map.data,aes(x = long,y = lat,group = group)) +
  geom_polygon(aes(fill = region),
               color = "black")

df.main <- df.main %>% 
  mutate(`confirmed total %` = `confirmed total`/population,
         `deaths total %` = `deaths total`/population)
df.region.group <- df.main %>% 
  group_by(region) %>% 
  count(state) %>% 
  ungroup() %>% 
  arrange(region,state) %>% 
  group_by(region) %>% 
  mutate(states = n(),
         id = row_number()) %>% 
  ungroup() %>% 
  mutate(group = case_when(id <=round(states/2,2) ~ 1,
                           T ~2)) %>% 
  mutate(`region - group`=paste0(region," - group ",group)) %>% 
  select(region,state,`region - group`)



## plot functions

plot_confirmed_cases_total <- function(region.group){
 #data
   plot.data <- df.main %>% 
    filter(`region - group` == region.group)
  #confirmed cases absolute count
   plt11 <- plot.data %>% 
     ggplot(aes(x = date,
            y = `confirmed total`,
            group = state,
            color = state)) +
     geom_line()+
     geom_point(size = 0.5) +
     scale_color_viridis_d() +
     xlab("Date") +
     ylab("Nr of confirmed cases total")+
     ggtitle(paste0("Infected cases/",region.group))+
     theme_minimal() +
     theme_bw()
  #confirmed relative count
   plt12 <- plot.data %>% 
     ggplot(aes(x = date,
                y = `confirmed total %`,
                group = state,
                color = state)) +
     geom_line()+
     geom_point(size = 0.5) +
     scale_color_viridis_d() +
     xlab("Date") +
     ylab("% of confirmed cases total")+
     theme_minimal() +
     theme_bw()
   
   #deaths total absolute count
   plt21 <- plot.data %>% 
     ggplot(aes(x = date,
                y = `deaths total`,
                group = state,
                color = state)) +
     geom_line()+
     geom_point(size = 0.5) +
     scale_color_viridis_d() +
     xlab("Date") +
     ylab("Nr of deaths cases total")+
     ggtitle(paste0("Infected cases/",region.group))+
     theme_minimal()+
     theme_bw()
   
   #deaths total relative count
   plt22 <- plot.data %>% 
     ggplot(aes(x = date,
                y = `deaths total %`,
                group = state,
                color = state)) +
     geom_line()+
     geom_point(size = 0.5) +
     scale_color_viridis_d() +
     xlab("Date") +
     ylab("% of deaths cases total")+
     theme_minimal()+
     theme_bw()
   
   plot <- plot_grid(plt11,plt12,plt21,plt22,nrow=2,ncol=2)
   
   ggsave(filename=paste0("~/Documents/Rstudio/data/",region.group,".png"),
          plot = plot,
          width = 30,height = 20,units = "cm")
  
}


df.main <- df.main %>% 
  left_join(x=.,
            y = df.region.group %>% select(-region),
            by = "state"
  )

## plot for each region and group

region.groups <- df.region.group %>% distinct(`region - group`) %>% pull(`region - group`)


plot_confirmed_cases_total("Northeast - group 1")

purrr::map(.x = region.groups,.f=plot_confirmed_cases_total)


## which states pay the highest price considering the confirmed deaths cases?

df.main %>% 
  filter(date == max.date) %>% 
  select(region,state,`confirmed total %`,`deaths total %`) %>% 
  pivot_longer(cols = c(`confirmed total %`,`deaths total %`),
               names_to = "count",
               values_to = "value") %>% 
  group_by(state) %>% 
  mutate(tot_percentage = sum(value)) %>% 
  ungroup() %>% 
  arrange(tot_percentage,state) %>% 
  mutate(state = as.factor(state),
         state = fct_inorder(state)) %>% 
  ggplot(aes(y = state,
             x = value,
             fill = region))+
  geom_col(color = 'black') +
  facet_wrap(~count,
             scales = 'free') +
  xlab('Percentage % of state population')+
  ylab('state')+
  ggtitle('confirmed cases and deaths cases relative count') +
  scale_fill_viridis_d()+
  theme_minimal() +
  theme_bw()

ggsave(filename=paste0("~/Documents/Rstudio/data/relative_count_confirmed_deaths_cases_last_date.png"),
       plot = last_plot(),
       width = 30,height = 20,units = "cm")

## create a map data

p1 <- df.main %>% 
  filter(date == max.date) %>% 
  select(region,state_,`confirmed total %`,`deaths total %`) %>% 
  left_join(x = .,
            y = map_data("state"),
            by=c("state_"="region")) %>% 
  ggplot(.,aes(x = long,y = lat,group = group)) +
  geom_polygon(aes(fill = `deaths total %` ),
               color = "black") +
  scale_fill_gradient(low = 'white',high = 'black')+
  ggtitle("Percentage of deaths VS state popluation")+
  xlab("")+
  ylab("")

p2 <- df.main %>% 
  filter(date == max.date) %>% 
  select(region,state_,`confirmed total %`,`deaths total %`) %>% 
  left_join(x = .,
            y = map_data("state"),
            by=c("state_"="region")) %>% 
  ggplot(.,aes(x = long,y = lat,group = group)) +
  geom_polygon(aes(fill = `confirmed total %` ),
               color = "black") +
  scale_fill_gradient(low = 'white',high = 'darkred')+
  ggtitle("Percentage of confirmed cases VS state popluation")+
  xlab("")+
  ylab("")

plot_grid(p1,p2,nrow = 2)

ggsave(filename=paste0("~/Documents/Rstudio/data/map_relative_count_confirmed_deaths_cases_last_date.png"),
       plot = last_plot(),
       width = 30,height = 40,units = "cm")


## Are daily dynamics change over time?
## daily confirmed cases
## daily deaths 

# calculate 7d average 

install.packages('zoo')
library(zoo)

df.main <- df.main %>% 
  arrange(state,date) %>% 
  mutate(`confirmed cases 7d avg` = rollapply(`confirmed daily cases`,FUN = mean,width = 7, align = "right", fill = NA)) %>% 
  mutate(`deaths 7d avg` = rollapply(`deaths daily cases`,FUN = mean,width = 7, align = "right", fill = NA)) %>% 
  ungroup()


plot_7d_avg <- function(region.group){
  #data
  plot.data <- df.main %>% 
    filter(`region - group` == region.group)
  #confirmed cases absolute count
  plt3 <- plot.data %>% 
    ggplot(aes(x = date,
               y = `confirmed cases 7d avg`,
               group = state,
               color = state)) +
    geom_line()+
    geom_point(size = 0.5) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Nr of 7d avg confirmed cases")+
    ggtitle(paste0("Infected cases/",region.group))+
    theme_minimal() +
    theme_bw()
  
  #deaths total absolute count
  plt4 <- plot.data %>% 
    ggplot(aes(x = date,
               y = `deaths 7d avg`,
               group = state,
               color = state)) +
    geom_line()+
    geom_point(size = 0.5) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Nr of 7d avg deaths cases")+
    ggtitle(paste0("Infected cases/",region.group))+
    theme_minimal()+
    theme_bw()
  
  plot <- plot_grid(plt3,plt4,nrow=2)
  
  ggsave(filename=paste0("~/Documents/Rstudio/data/7d_avg_confirmed_deaths_cases",region.group,".png"),
         plot = plot,
         width = 30,height = 20,units = "cm")
  
}

library(purrr)


purrr::map(.x = region.groups,
    .f=plot_7d_avg)

plot_7d_avg(region.group="Northeast - group 1")


## Do state economics have effect on tot percentage of confirmed cases and deaths?

df.main %>% 
  filter(date == max.date) %>% 
  ggplot(aes(x = `confirmed total %`,
             y = `deaths total %`,
             size = `population in million`,
             color = `gdp_per_capita`))+
  geom_point(alpha = 0.75,show.legend = T)+
  facet_wrap(~ region) +
  scale_color_gradient(low = 'brown1',high = 'green')+
  scale_size_area(max_size = 40)+
  xlab('confirmed cases %')+
  ylab('deaths cases %') +
  ggtitle('Total confirmed cases % and total deaths cases % VS GDP per capita and population')
  
ggsave(filename=paste0("~/Documents/Rstudio/data/confirmed_deaths_vs_gdp_and_population.png"),
       plot = last_plot(),
       width = 30,height = 20,units = "cm")

## Does vacciantion help increase COVID 19 confirmed cases and deaths?

vac_start_date <- df.main %>% filter(`vaccine doses total` !=0) %>% pull(date) %>% min()


plot_vac_effect<- function(region.group){
  #data
  plot.data <- df.main %>% 
    filter(`region - group` == region.group)
  #confirmed cases absolute count
  plt5 <- plot.data %>% 
    ggplot(aes(x = date,
               y = `confirmed cases 7d avg`,
               group = state,
               color = state)) +
    geom_line()+
    geom_point(size = 0.5) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Nr of 7d avg confirmed cases")+
    ggtitle(paste0("Infected cases/",region.group))+
    theme_minimal() +
    theme_bw()
  
  #deaths total absolute count
  plt6 <- plot.data %>% 
    ggplot(aes(x = date,
               y = `deaths 7d avg`,
               group = state,
               color = state)) +
    geom_line()+
    geom_point(size = 0.5) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Nr of 7d avg deaths cases")+
    ggtitle(paste0("Infected cases/",region.group))+
    theme_minimal()+
    theme_bw()
  #vaccine count
  plt7 <- plot.data %>% 
    ggplot(aes(x = date,
               y = `vaccine doses total`,
               group = state,
               color = state)) +
    geom_line()+
    geom_point(size = 0.5) +
    scale_color_viridis_d() +
    xlab("Date") +
    ylab("Nr of Vaccine doses total")+
    ggtitle(paste0("Vaccine doese total",region.group))+
    theme_minimal()+
    theme_bw()
  
  plot <- plot_grid(plt5,plt6,plt7,nrow=3)
  
  ggsave(filename=paste0("~/Documents/Rstudio/data/7d_avg_confirmed_deaths_vacc_cases",region.group,".png"),
         plot = plot,
         width = 30,height = 20,units = "cm",dpi = 600)
  
}

purrr::map(.x = region.groups,
           .f=plot_vac_effect)


#show on map how number of covid 19 cases change over time?
# add date id and snapshot flag

df.main <- df.main %>% 
  arrange(state,date) %>% 
  group_by(state) %>% 
  mutate(date_id = row_number()) %>% 
  ungroup() %>% 
  mutate(`date snapshop flag` = case_when(date_id == 1 ~ TRUE,
                                          date == max.date ~ TRUE,
                                          date_id %%30 == 0 ~ TRUE,
                                          T~FALSE))

df.main %>% 
  filter(`date snapshop flag` == TRUE) %>% 
  select(state_, region,date, `confirmed total`) %>% 
  left_join(x = .,
            y = map_data("state"),
            by = c("state_"="region"))  %>% 
  ggplot(.,aes(x = long,y = lat,group = group)) +
  geom_polygon(aes(fill = `confirmed total` ),
               color = "black") +
  facet_wrap(~date)+
  scale_fill_gradient(low = 'white',high = 'darkred')+
  ggtitle("Percentage of confirmed cases every 30 days")+
  xlab("") +
  ylab("")

plot_selected_state <- function(selected.state){
  
  df.main.state <- df.main %>% 
    filter(state == selected.state)
  
  p1.select.state <- df.main.state %>% 
    select(date,`confirmed total`,`deaths total`,`vaccine doses total`) %>% 
    pivot_longer(cols = c("confirmed total","deaths total","vaccine doses total"),
                 names_to="indicator",
                 values_to = "value") %>% 
    mutate(indicator = factor(indicator,
                              levels = c("confirmed total","deaths total","vaccine doses total"))) %>% 
    ggplot(aes(x = date,
               y = value,
               fill = indicator))+
    geom_area(color = "black")+
    scale_fill_viridis_d('magma')
  
  ggsave(filename=paste0("~/Documents/Rstudio/data/state_action_take_",selected.state,".png"),
         plot = p1.select.state,
         width = 30,height = 20,units = "cm")
  
  
}


plot_selected_state("Texas")





























