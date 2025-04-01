######################################
#lab 1
######################################
library(Sleuth3)
attach(case1701)

# Looking at the data
names(case1701)
head(case1701)
case1701[1 : 6, ]
summary(case1701)
str(case1701)

#---------------------------------------------
# Correlation Among Multivariate Responses
#---------------------------------------------
round(cor(case1701[, 1 : 11]), 2)
pairs(case1701[, 1 : 11])

#---------------------------------------------
# ANOVA Type I, II Tests
#---------------------------------------------

avg <- apply(case1701[, 1 : 11], 1, mean)

# convert the explanatory variables into factors
Current <- as.factor(Current)
Config <- as.factor(Config)
Material <- as.factor(Material)

# extra SS F-test
anova(lm(avg ~ Current * Config * Material), lm(avg ~ Current + Config + Material))

#
# SS type I: result from anova()
#
anova(lm(avg ~ Current + Config + Material))

# order of model terms matters
anova(lm(avg ~ Config + Current + Material))


#
# SS type II: result from Anova() from package car
#
library(car)

Anova(lm(avg ~ Current + Config + Material), type='II')

# check for SS of Current
anova(lm(avg ~ Config + Material), lm(avg ~ Current + Config + Material))

# check for SS of Config
anova(lm(avg ~ Current + Material), lm(avg ~ Current + Config + Material))

# order of model terms does not matter
Anova(lm(avg ~ Config + Current + Material), type='II')


detach(case1701)
