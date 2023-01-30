library(tidyverse)
library(here)

darwin <- read_csv(here("data", "darwin.csv"))


glimpse(darwin)
head(darwin)
colnames(darwin)

darwin <- janitor::clean_names(darwin)

darwin %>%
  duplicated() %>%
  sum()

darwin %>%
  summarise(min=min(height,na.rm = T),
            max=max(height, na.rm = T))
darwin %>%
  distinct(pair)

darwin %>%
  distinct(type)

darwin %>%
  is.na() %>%
  sum()

summary(darwin)

darwin %>%
  ggplot(aes(x=type,
             y=height))+
  geom_point()


darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))


darwin_summary <- darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))

darwin_summary %>%
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

# put this at top of script
library(kableExtra)

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

darwin_wide <- darwin %>%
 pivot_wider(names_from = type, values_from = height) %>%
 mutate(difference = Cross - Self)

difference_summary <- darwin_wide %>%
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n(),
            SE = sd/sqrt(n))
difference_summary


x <- seq(-4,4, length = 100)
y<-dnorm(x)
plot(x,y,type="l", lwd=2, axes=F, xlab="", ylab = "")
axis(1,at=-3:3, labels=c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI