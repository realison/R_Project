rm(list = ls())
graphics.off()

library(lubridate)
library(hms)
library(nycflights13)
library(tidyverse)

as_date(18992)
yq("2012:Q3")

q <- seq(2021,2022,0.25)
date_decimal(q)


fast_strptime(x = "2021-12-31 12:00:00",format= "%Y-%m-%d %H:%M:%S") %>% 
  class()


flights %>% 
  select(year,month,day,hour,minute) %>% 
  mutate(datetime = make_datetime(year,month,day,hour,minute),
         date = make_datetime(year,month,day))


#extract components

flights2 <- flights %>% 
  select(year,month,day,hour,minute) %>% 
  mutate(datetime = make_datetime(year,month,day,hour,minute),
         date = make_datetime(year,month,day)) %>% 
  #extract week day
  mutate(wday = wday(datetime,week_start = 1),
         week = week(datetime),
         Q = quarter(datetime)) %>% 
  arrange(desc(datetime))

now()
# seconds
now()-3600

age <- today()-ymd("1994-10-02")

#convert age to duration
as.duration(age)

# ggplot2

tibble(x = seq(-3,3,1),
       probs = pnorm(q = x, mean = 0, sd=1,lower.tail = T))
groups <- paste("group",1:4,ep = " ")
probs <- c(.2,.3,.4,.1)
sum(probs)

set.seed(123)
df.data <- sample(groups,
                  size = 1000,
                  replace = T,
                  prob = probs) %>% 
  tibble(group= .)
df.data %>% 
  ggplot(aes(x = group)) +
  geom_bar(stat = "count")


df.data %>% 
  group_by(group) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x= group,y=freq)) +
  geom_bar(stat= 'identity')



df.data %>% 
  group_by(group) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>%
  ggplot(aes(x= group,y=freq)) +
  geom_bar(stat= 'identity') +
  scale_fill_brewer(palette = 3) +
  geom_text(aes(label=freq,
                y = freq+10),
            size = 10)




figure <- mpg %>% 
  group_by(manufacturer) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(percentage = round(n/sum(n)*100,1),
         label = paste(n," | ",percentage,"%", sep = "")
  ) %>% 
  arrange(desc(n)) %>% 
  mutate(manufacturer= as.factor(manufacturer),
         manufacturer=fct_inorder(manufacturer) )%>% 
  ggplot(aes(x = manufacturer,
             y = n,
             fill = manufacturer)) +
    geom_bar(stat= "identity",
             show.legend = F,
             color = "black") +
    geom_text(aes(label= label,
                  y = n+1),size = 3) +
  scale_fill_viridis_d(option = "inferno",direction = -1) +
  xlab("car manufacturer") +
  ylab("count") +
  ggtitle("number of cars per each manu.")
figure    
ggsave(filename = "~/Desktop/manufacturer.png",
       plot = figure,
       units="cm",
       width = 40,
       height = 21,
       dpi = 300)


mpg %>% 
  ggplot(aes(x=cty,
             y= hwy))+
  geom_point(shape=17,
                 size = 2,
                 color="red")+
  geom_smooth(method = "lm",
              se =T)


economics %>% 
  ggplot(aes(date)) +
  geom_line(aes(y = unemploy),color = "red",linewidth= 1.2,linetype = "dashed") +
  geom_line(aes(y = pce),color = "blue") +
  geom_line(aes(y = psavert),color = "green") +
  scale_y_log10()


economics_long %>%  
  filter(variable != "pop") %>% 
  ggplot(aes(date,value,group = variable,color=variable)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years",date_labels = "%Y-%m")

library(ggwordcloud)


df.cars <- mpg %>%
  count(model,manufacturer)
set.seed(135)
df.cars %>% 
  ggplot(aes(label = model,size= n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()


# word rotation

df.cars <- df.cars %>% 
  mutate(angle = 90*sample(c(0,1),size = n(),replace = T,prob=c(0.7,0.3))) %>% 
  mutate(angle1 = 45*sample(c(-2:2),size = n(),replace = T,prob=c(1,1,4,1,1)))





df.cars %>% 
  ggplot(aes(label = model,size= n,
             angle = angle1,
             color = manufacturer)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  scale_color_viridis_d(option = "magma")
  theme_minimal() 

  
  
#map
  
df.crime <- USArrests %>% 
  mutate(region = str_to_lower(rownames(.))) %>% 
  left_join(x = .,
            y = map_data("state"),
            by = "region" )

  
  
df.crime %>% 
  ggplot(aes(long,
             lat,
             group = group)) +
  geom_polygon(aes(fill = Assault),
               color = "black"
               ) +
  scale_fill_viridis_c(option="magma") +
  theme()




install.packages('cowplot')
library(cowplot)


p1 <- ggplot(mpg,aes(x= cty,y = hwy)) + geom_jitter()
p2 <- ggplot(mpg,aes(x= displ,y = hwy)) + geom_jitter()
p3 <- ggplot(mpg,aes(x= cyl,y = hwy)) + geom_jitter()
p4 <- ggplot(mpg,aes(x= drv,y = cty)) + geom_jitter()
p5 <- ggplot(mpg,aes(x= trans,y = hwy)) + geom_jitter()
p6 <- ggplot(mpg,aes(x= class,y = hwy)) + geom_jitter()
#create subplots

plot_grid(p1,p2,p3,p4,labels = c("p1","p2","p3","p4"))
plot_grid(p1,p2,p3,p4,p5,p6,nrow=3,ncol=2)





flights <- flights %>% 
  group_by(carrier) %>% 
  mutate(`nu flights`=n()) %>% 
  ungroup() %>% 
  mutate(rank = dense_rank(1/`nu flights`)) %>% #rank from desc
  filter(rank <=4) %>%
  select(carrier, rank, distance)  %>% 
  mutate(carrier = as.factor(carrier),
         carrier= fct_infreq(carrier)) 

flights %>% 
  ggplot(aes(distance,
             fill = carrier)) +
  geom_density(alpha = 0.5,color = "black") +
  scale_x_continuous(breaks=seq(0,2000,250),limits = c(0,2000)) +
  scale_y_continuous(breaks=seq(0,0.005,0.0001)) 




















