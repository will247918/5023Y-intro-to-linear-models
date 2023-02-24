library(tidyverse)
library(rstatix)
library(performance)
library(here)
library(rstatix)
janka <- read_csv(here('data','wood_density.csv'))
jnaka <- janitor::clean_names(janka)


janka_plot <- ggplot(janka, aes(x=dens,y=hardness))+
  geom_point()
wood_density_janka

#Use cor_test
cor_test(janka,dens,hardness)

# Or use with package
with(wood_density, cor(dens,hardness))

# 










# gitcreds::gitcreds_set()
# Enter password or token: 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