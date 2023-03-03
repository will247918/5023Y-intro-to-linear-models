darwin <- read_csv(here("data", "darwin.csv"))

lsmodel0 <- lm(formula=height ~ 1, data = darwin)
broom::tidy(lsmodel0)
broom::glance(lsmodel0)
broom::augment(lsmodel0)

lsmodel1 <- lm(height~type, data = darwin)
summary(lsmodel1)

anova(lsmodel1)

lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin)
anova(lsmodel2)
