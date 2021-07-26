#libs
library(tidyverse)
library(janitor)

#import data
read_csv("data/wine_quality/winequality-red.csv")
read_csv2("data/wine_quality/winequality-red.csv")


cars <- read_csv('data/cars/cars.csv')

#explore DF

ncol(cars)
nrow(cars)
names(cars)


#explore name cases

names(clean_names(cars, case = 'snake'))
names(clean_names(cars, case = 'lower_camel'))
names(clean_names(cars, case = 'upper_camel'))
names(clean_names(cars, case = 'all_caps'))
names(clean_names(cars, case = 'lower_upper'))
names(clean_names(cars, case = 'upper_lower'))

#set names
cars <- clean_names(cars)

#explore DF

sample_n(cars, 6)
str(cars)
glimpse(cars)
summary(cars)

#explore wrong class vars

cars$number_of_doors
cars$cylinders
cars$price

#Old School parsing
# puertas
## exploramos valores posibles
table(cars$number_of_doors,useNA = 'always')

car_doors <- c()

for(i in cars$number_of_doors){
  if(is.na(i)){
    val <- NA
  }
  else if(i == 'two'){
    val <- 2
  }
  else if(i == 'four'){
    val <- 4
  }
  else{
    rm(car_doors)
    stop('input no contemplado')
  }
  car_doors <- c(car_doors,val)
}
cars$number_of_doors <- car_doors

# cilindros
# tidyverse parsing

table(cars$cylinders,useNA = 'always')

case_when(cars$cylinders == 'eight' ~ 8,
          cars$cylinders == 'five' ~ 5,
          cars$cylinders == 'four' ~ 4,
          cars$cylinders == 'six' ~ 6,
          cars$cylinders == 'three' ~ 3,
          cars$cylinders == 'twelve' ~ 12,
          cars$cylinders == 'two' ~ 2,
          TRUE ~ NA_real_)


# DS way parsing

library(words2number)
cars$cylinders <- to_number(cars$cylinders)


#parse price
as.numeric(cars$price)
cars$price[is.na(as.numeric(cars$price))]
cars$price <- as.numeric(str_replace_all(cars$price,'\\?',''))

cars$hp <-as.numeric(cars$hp)
cars$max_rpm <-as.numeric(cars$max_rpm)


#character to factor

cars$make <- as.factor(cars$make)
cars$fuel <- as.factor(cars$fuel)
cars$aspiration <- as.factor(cars$aspiration)
cars$body_style <- as.factor(cars$body_style)
cars$drive_wheels <- as.factor(cars$drive_wheels)
cars$engine_location <- as.factor(cars$engine_location)



#new summary
summary(cars)

#correlation matrix

cars %>% select_if(is.numeric) %>% cor(use = 'complete.obs') %>% View()
