#libs
library(tidyverse)
library(janitor)

#import data

redWine <- read_csv2("data/wine_quality/winequality-red.csv")
whiteWine <- read_csv2("data/wine_quality/winequality-white.csv")

# Ex 0
redWine <- clean_names(redWine)
whiteWine <- clean_names(whiteWine)

nrow(redWine)
ncol(redWine)

nrow(whiteWine)
ncol(whiteWine)

names(redWine) == names(whiteWine)

sample_n(redWine,5)
sample_n(whiteWine,5)

# Ex 1

redWine$class <- 'red'
whiteWine$class <- 'white'

wineDf <- rbind(redWine, whiteWine)


# EX 2


str(wineDf)
summary(wineDf)

wineDf$volatile_acidity <- as.numeric(wineDf$volatile_acidity)
wineDf$citric_acid <- as.numeric(wineDf$citric_acid)
wineDf$residual_sugar <- as.numeric(wineDf$residual_sugar)
wineDf$chlorides <- as.numeric(wineDf$chlorides)
wineDf$density <- as.numeric(wineDf$density)
wineDf$sulphates <- as.numeric(wineDf$sulphates)

wineDf$class <- as.factor(wineDf$class)

summary(wineDf)


# EX 3





cor(select(wineDf, -class),use = 'complete.obs')

cor(select(wineDf[wineDf$class == 'red',], -class),use = 'complete.obs')
cor(select(wineDf[wineDf$class == 'white',], -class),use = 'complete.obs')

# EX 4

get_outliers <- function(x){
  quantiles <- quantile(x,c(.25,.75))
  IQR <- quantiles[2] - quantiles[1]
  boundaries <- c(quantiles[1] - 1.5* IQR, quantiles[2] + 1.5 * IQR)
  print(boundaries)
  
  return(x[x<boundaries[1] | x > boundaries[2]])
}

get_outliers(wineDf$alcohol)


redWine <- read_delim("data/wine_quality/winequality-red.csv",delim = ';')
whiteWine <- read_delim("data/wine_quality/winequality-white.csv",delim = ';') 


redWine <- clean_names(redWine)
whiteWine <- clean_names(whiteWine)

redWine$class <- 'red'
whiteWine$class <- 'white'


wineDf <- rbind(redWine, whiteWine)
wineDf$class <- factor(wineDf$class)

summary(wineDf)
