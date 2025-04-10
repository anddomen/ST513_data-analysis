###########################################
#Lab 2
###########################################
library(Sleuth3)
attach(case1701)
#scaling one measurement
Z1 <- (L1 - mean(L1))/sd(L1)

#scaling automatically with prcomp
out.scaled1 <- prcomp(case1701[, 1 : 11], scale = T)

#scaling all of the columns in the data before applying PCA
forces.scaled <- scale(case1701[, 1 : 11])
out.scaled2 <- prcomp(forces.scaled)

#compare PCA outputs to make sure they match
out.scaled1$rotation
out.scaled2$rotation
#the outputs match!

#continue analysis with unstandardized magnetic force measurements
out <- prcomp(case1701[, 1 : 11])
summary(out)
names(out)
loadings <- out$rotation
pcs <- out$x
avg <- apply(case1701[, 1 : 11], 1, mean)
ends <- apply(case1701[, 1 : 6], 1, mean) - apply(case1701[, 9 : 11], 1, mean)
mid <- apply(case1701[,1 : 4], 1, mean) - case1701[, 6]

plot(pcs[, 1], avg, xlab = "First Principal Component", ylab = "Average", pch = 16)

cor(pcs[, 1], avg)

cor(pcs[, 1], pcs[, 2])
cor(avg, ends)

lmod <- lm(ends ~ avg)
plot(pcs[, 2], resid(lmod), xlab = "Second Principal Component", ylab = "Residuals", pch = 16)

lmod <- lm(mid ~ avg + ends)
plot(pcs[, 3], resid(lmod), xlab = "Third Principal Component", ylab = "Residuals", pch = 16)


reducedmod <- lm(avg ~ Current + Config + Material)
fullmod <- lm(avg ~ Current*Config*Material)
anova(reducedmod, fullmod) #test for interactions

anova(reducedmod)

detach(case1701)
###########################################
#Exercise 17.15
###########################################
attach(ex1715)
names(ex1715)
head(ex1715)
range(Attend)
range(AnnInc)

round(cor(ex1715[, 3 : 6]), 3)
pairs(ex1715[, 3 : 6])

denom <- prcomp(ex1715[, 3 : 6], scale = T)
denom.usd <- prcomp(ex1715[, 3 : 6], scale = F)
denom$rotation
denom.usd$rotation

loadings <- denom$rotation
pcs <- denom$x

summary(denom)

detach(ex1715)
