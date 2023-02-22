rm(list = ls())
graphics.off()

library(tidyverse)
library(purrr)
library(hflights)


#map function

df <- hflights %>% 
  select(ActualElapsedTime, AirTime, Distance,TaxiIn,TaxiOut) %>% 
  as_tibble()
df

df %>% map(.x=.,.f=mean,na.rm=TRUE ) # with some na could lead to no mean values
df %>% map(min,na.rm=TRUE)
df %>% map(max,na.rm=TRUE)
df %>% map(sd,na.rm=TRUE)

str(mpg)

mpg %>% 
  select(displ,year,cyl,cty,hwy)

numeric.cols <- map(.x=mpg,.f=is.numeric) %>% 
  unlist() %>% 
  tibble(column = names(.),
         numeric=.) %>% 
  filter(numeric == "TRUE") %>% 
  pull(column)



# control the output


df %>% map_dbl(.x=.,.f=mean,na.rm=TRUE ) %>% class()# with some na could lead to no mean values
df %>% map_chr(min,na.rm=TRUE)
df %>% map(max,na.rm=TRUE)
df %>% map(sd,na.rm=TRUE)


numeric.cols2 <- map_lgl(.x=mpg,.f=is.numeric)


# create a summary table



df %>% 
  colnames() %>% 
  tibble(variable = .,
         mean = df %>% map_dbl(.x=.,.f=mean,na.rm=TRUE ),
         sd = df %>% map_dbl(sd,na.rm=TRUE),
         max = df %>% map_dbl(max,na.rm=TRUE)
  )


list <- list(
  a = 1,
  b = "word",
  v = 1:10,
  df= mpg
)
list
#return the length of each objects

list %>% map_int(.,.f=length)


models <- mpg %>% 
  split(.$cyl) %>% 
  map(~lm(hwy~displ,
          data = .))

models %>% 
  map(summary) %>% 
  map_dbl(~.$"r.squared")




mu <- c(0,-4,5,8)
sig <- c(1,2,1,3)

data <- map2(.x = mu, .y=sig, .f=rnorm,n = 1000) %>% 
  enframe () %>% 
  mutate(name = paste0("norm",1:4)) %>% 
  unnest(cols = c(value))
data %>% 
  ggplot(aes(x = value,
             colour = name,
             fill = name)) +
  geom_density(linewidth = 0.3,
               alpha = 0.5)
#exporting plots
plots <- mpg %>% 
  split(.$manufacturer) %>% 
  map(~ggplot(.,aes(displ,hwy))+geom_point()+ggtitle(paste0(.$manufacturer)))

#create a path for directory

path <- "~/Desktop/plots"

if(!dir.exists(path)){
  dir.create(path)
}

#export plots

list(str_c(path,"/",names(plots),".pdf"),plots) %>% 
  pwalk(.,ggsave)


























































































