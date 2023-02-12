library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

darwin <- read_csv(here("data", "darwin.csv"))

lsmodel0 <- lm(formula=height ~ 1, data = darwin)
broom::tidy(lsmodel0)
broom::glance(lsmodel0)
broom::augment(lsmodel0)
summary(lsmodel0)

lsmodel1 <- lm(height~type, data=darwin)
summary(lsmodel1)

ggplot(darwin, aes(x=type, y= height)) +
  geom_jitter(alpha=0.4,width=0.1)+
  stat_summary(fun=mean, color='blue')

confint(lsmodel1)
