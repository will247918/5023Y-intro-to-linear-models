# Library packages -------------------
library(tidyverse)
library(janitor)
library(lubridate)
#-------------------------------------

# Read in data -----------------------
darwin <- read_csv('data/darwin.csv')
#-------------------------------------

# Clean data
colnames(darwin)
head(darwin)

#Change the coloumn type to lower case.
darwin[[2]]<-tolower(darwin[[2]])

#Make data wider
darwin_wider <- darwin %>%
  pivot_wider(names_from = type, values_from = height)
darwin_wider

darwin_lm <- lm(height~type, darwin)
summary(darwin_lm)

darwin_lm2 <- lm(height~ type + factor(pair), data = darwin)
summary(darwin_lm2)

performance::check_model(darwin_lm, check = c('normality','qq'))
performance::check_model(darwin_lm, check="homogeneity")
performance::check_model(darwin_lm, check="outliers")