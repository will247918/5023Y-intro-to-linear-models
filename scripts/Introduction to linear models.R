library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)

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

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)
broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

means <- emmeans::emmeans(lsmodel1, specs = ~ type)
means

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

#plot(lsmodel1)
performance::check_model(lsmodel1, check=c("normality","qq"))

performance::check_model(lsmodel1, check="homogeneity")

plot(lsmodel1, which=c(1,3))

performance::check_model(lsmodel1, check="outliers")

plot(lsmodel1, which=c(4,4))