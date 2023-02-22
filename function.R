library(tidyverse)
library(purrr)
library(ggplot2)
install.packages('hflights')
library(hflights)
cross2(letters[1:3],1:2)
list.n <- list(runif(n=10),
               runif(n=10),
               runif(n=10),
               runif(n=10))
list.c <- list(sample(letters,17),
               sample(letters,17),
               sample(letters,17),
               sample(letters,17))

#intersection of letters
reduce(list.c,intersect)

## calculation a cumulative sum numeric vectors
reduce(list.n,sum)

accumulate(list.c,intersect)
accumulate(list.n,sum)


#nested data

df <- mpg  %>% 
  filter(manufacturer %in% c("jeep","land rover","lincoln")) %>% 
  select(manufacturer,model,displ,cyl,hwy)
df.n <- df %>% 
  group_by(manufacturer) %>% 
  nest()


df1 <- df.n %>% 
  unnest(col=c("data"))

df1

df.n$data %>% map(.x=.,.f=~length(.$model))
df.n$data %>% map(.x=.,.f=~mean(.$hwy))


## nesting a larder data frame
mpg %>% 
  group_by(manufacturer) %>% 
  nest()

diamonds %>% 
  group_by(cut,color) %>% 
  nest() %>% 
  mutate(`avg price`=map(data,~mean(. $price)),
         `nr diamonds`=map(data,~length(.$price))) %>% 
  mutate(`avg price`= unlist(`avg price`),
         `nr diamonds`= unlist(`nr diamonds`))

df.models <- mpg %>% 
  group_by(manufacturer) %>% 
  nest() %>% 
  mutate(model = map(.x = data,
                     .f=~lm(hwy~displ+cyl,data = .)))
# model's coefficients

model <- df.models %>% filter(manufacturer == "audi") %>% pull(model)

model %>% flatten() %>% pluck(coefficients) %>% enframe() %>% .[[2]]

model %>% map(summary) %>% map_dbl("r.squared")

extract_coef <- function(model){
  coefficients(model)[[1]]
}
extract_coef <- function(model,id_coef){
  coefficients(model)[[id_coef]]
  
  }

# extract r square or coefficients

df.models1 <- df.models %>% 
  mutate(summary = map(.x=model,.f=summary),
         `r squared`=map_dbl(.x=summary,.f='r.squared'),
         `coef a0`=map_dbl(.x=summary,.f=extract_coef,1),
         `coef a1`=map_dbl(.x=summary,.f=extract_coef,2),
         `coef a2`=map_dbl(.x=summary,.f=extract_coef,3))

## closer look to models with r squared = 0

df.models1 %>% 
  filter(`r squared` == 0) %>% 
  select(manufacturer,data) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  ggplot(aes(x = displ,y = hwy,color=as.factor(cyl)))+
  geom_point()+
  facet_wrap(.~manufacturer)

## take a look at the highest r squarer

df.models1 %>% 
  arrange(desc(`r squared`)) %>% 
  head(1) %>% 
  select(manufacturer,data) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  ggplot(aes(x = displ,y = hwy,color=as.factor(cyl)))+
  geom_point() +
  facet_wrap(.~manufacturer)

library(dplyr)
# practice purrr export files

mpg %>% 
  map_df(.x = .,
         .f = ~(data.frame(missing_values = sum(is.na(.x)),
                distinct_values=n_distinct(.x),
                class=class(.x))),
         .id = "variable")
# import multifiles into r

## import csv files from single directory

path <- "D:/RStudio/mpg_single_level/"


df1 <- tibble(directory = path,
              files = list.files(path)) %>% 
  mutate(path=str_c(directory,files,sep="/")) %>% 
  mutate(data = map(.x=path,
                    .f = function(path_){
                      read_csv(path_,
                               col_types=cols(
                                 "manufacturer" =col_character(),
                                   "model" =col_character(),
                                   "displ"  =col_double(),
                                    "year" =col_integer(),
                                   "cyl"  = col_integer(),
                                   "trans"  = col_character(),
                                   "drv"    = col_character(),
                                   "cty"    =col_double(),
                                   "hwy"     =col_double(),    
                                 "fl" = col_character(),
                                   "class" =  col_character()
                               ))
                    })) %>% 
  pull(data) %>% 
  bind_rows()

df1


## export multiple files into the same directory

directory <- "D:/RStudio/mpg_single_level/export/"

if(!dir.exists(direcotry)){
  dir.create(directory)
}

df.export <- mpg %>% 
  group_by(manufacturer,model) %>% 
  mutate(car_id = row_number()) %>% 
  ungroup() %>% 
  mutate(path = paste0(directory,manufacturer,"_",str_remove_all(model,pattern = " "),
                       car_id,".csv")) %>% 
  select(-car_id) %>% 
  group_by(path) %>% 
  nest() %>% 
  ungroup()

df.export
# create a list of data and file path for pmap to export files
list(x = df.export$data,
     file= df.export$path) %>% 
  pmap(.l=.,.f=write_csv) %>% 
  quietly()
  
install.packages('cowplot')  
library(cowplot)


# Exercise 1

blueprint_raw <- read_csv('D:/RStudio/assignment_06/assignment_06/simulations_blue_print.txt',col_names=F) %>% 
  rename(type = X1)

## create vector
type <- blueprint_raw %>% pull(type)


blueprint_raw <- blueprint_raw %>% 
  separate(col=1, into = c("f",'arg1','arg2',"arg3"),sep = ";")

## create vector functions

f <- blueprint_raw %>% pull(f)
args <- list()


for(r in 1:nrow(blueprint_raw)){
  args.row.text <- ""
  
  for(c in 2:ncol(blueprint_raw)){
    if(blueprint_raw[r,c] !=""){
      args.row.text <- str_c(args.row.text,blueprint_raw[r,c],sep=",")
     }
  }
  
  args.row.text <- str_remove(args.row.text,pattern="^,|,$")
  args.row.text <- paste0("list(",args.row.text,")")
  eval(parse(text=paste0("args.row.list=",args.row.text)))
  args <- c(args,list(args.row.list))
  
}

args


# do the simulation

set.seed(123)

data.sim <- invoke_map(.f=f,.x=args)%>% 
  enframe() %>% 
  mutate(type = type,
         f = f) %>% 
  unnest(cols=c(value)) %>% 
  select(f,type,value)
data.sim



## create subplots inside tibbles

plots <- data.sim %>% 
  group_by(f) %>% 
  nest() %>% 
  mutate(plot= map(.x=data,
                   .f = ~ggplot(.,aes(x = value,fill = type))+
                     geom_density(alpha = 0.3)+
                     scale_fill_viridis_d()+
                     ggtitle(paste0(f))+
                     xlab("")+
                     theme_minimal()))
## draw subplots

plot_grid(plotlist = plots %>% pull(plot),nrow=3)











