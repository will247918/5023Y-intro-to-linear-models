library(tidyverse)
library(rstatix)
library(performance)
library(here)
library(rstatix)
janka <- read_csv(here('data','wood_density.csv'))
janka <- janitor::clean_names(janka)


janka_plot <- ggplot(janka, aes(x=dens,y=hardness))+
  geom_point()+
  geom_smooth(methos='lm')
janka_plot

#Use cor_test
cor_test(janka,dens,hardness)

# Or use with package
with(janka, cor(dens,hardness))

# Fit the data to a linear model
janka_lm <- lm(hardness~dens, janka)
summary(janka_lm)

dens_mean <- janka %>%
  summarise(mean_dens = mean(dens))
  
janka_centered <- janka %>%
  group_by(dens)%>%
  mutate(centered_dens = dens-pull(dens_mean))

janka_centeredlm <- lm(hardness~dens,janka_centered )
summary(janka_centeredlm)

ggplot(janka_centeredlm, aes(x=dens,y=hardness)) +
  geom_point()+
  geom_smooth(method='lm')

#Calculate confidence intervals
confint(janka_lm)

predict(janka_lm)

resid(janka_lm)

#Creae the y residual values etc.
augmented_lm<- janka_lm %>% 
  broom::augment()

augmented_lm %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")


# A line connecting all the data points in order 
p1 <- augmented_lm %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_lm %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_lm %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")


library(patchwork)
p1+p2+p3

model_plot <- function(data, x, y, title ){
  
 ggplot(aes(x=.data[[x]],y=.data[[y]]),data=data)+
    geom_line()+
    ggtitle(title)
}

p1<-model_plot(augmented_lm, 'dens', 'hardness', 'full data')
p1
p2<-model_plot(augmented_lm,'dens','.fitted','linear model')
p2
p3<-model_plot(augmented_lm,'dens','.resid','Remaining pattern')
p3 
p1+p2+p3

performance::check_model(janka_lm, check=c('normality','qq'))
performance::check_model(janka_lm, check='homogeneity')
performance::check_model(janka_lm, check='outliers')

# Work out the hardness of wood samples with density 65
coef(janka_lm)
coef(janka_lm)[1]+coef(janka_lm)[2]*65

broom::augment(janka,newdata = tibble(dens=c(22,35,65)), se=T)

predict(janka_lm,list(dens=c(22,35,65)))

# gitcreds::gitcreds_set()
# Enter password or token: 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