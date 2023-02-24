source("scripts/Introduction to linear models.R")

x<- seq(-4,4,length=100)
hx <- dnorm(x)

degf <- c(1,3,8,30)
colors <- c('red', 'blue', 'darkgreen', 'gold', 'black')
labels <- c('df=1', 'df=3', 'df=8', 'df=30', 'normal')

plot(x,hx, type = 'l',lty=2, xlab='x value', ylab = 'Density', main = 'comparison of t Distributions')

for(i in 1:4){
  lines(x,dt(x,degf[i]), lwd=2, col=colors[i])
}

legend('topright', inset=.05, title='Distributions',
       labels, lwd=2, lty=c(1,1,1,1,2), col=colors)

df<- c(1:30)
critical_t <- map_dbl(df,~qt(p=0.05/2, df=.x, lower.tail=F))

tibble(df,critical_t) %>%
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.6), linetype='dashed', color='red')+
  labs(x='Degrees of freedom',
       y=expression(paste('Critical value of ', italic('t'))))


lsmodel1 <- lm(height~ type, data=darwin)
summary(lsmodel1)

t.test(height~type, data=darwin, paired=T)
lsmodel_darwin <- lm(height~type + factor(pair), data = darwin)
summary(lsmodel_darwin)

tidy_model1<- broom::tidy(lsmodel1)
tidy_model1[[2,2]]/tidy_model1[[2,3]]

lsmodel_darwin <- lm(height~type + factor(pair), data = darwin)
summary(lsmodel_darwin)

lm(height ~ type + factor(pair), data = darwin) %>%
  broom::tidy(., conf.int=T)%>%
  slice(1:2)

m1 <- lm(height~type, data=darwin) %>%
  broom::tidy(.,conf.int=T) %>%
  slice(2,2) %>%
  mutate(model='unpaired')

m2 <- lm(height~type, factor(pair), data=darwin) %>%
  broom::tidy(.,conf.int=T) %>%
  slice(2,2) %>%
  mutate(model='paired')

rbind(m1,m2) %>%
  ggplot(aes(model,estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype='dashed')+
  theme_minimal()+
  coord_flip()

set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

z = y[,c(5,8)]
p=z$p.value
list_pvalue = list()
for (i in p) {
  
  list_pvalue[[length(list_pvalue)+1]] = i
}


gt6_values = list_pvalue[list_pvalue <= 0.05]
length(gt6_values)

z %>%
  mutate('significance'= if_else(p<=0.05,'significant','notsignificant')) %>%
  group_by(significance) %>%
  summarise('number of experiments'=n())

broom::tidy(y, conf.int=T, conf.level=0.95)
  