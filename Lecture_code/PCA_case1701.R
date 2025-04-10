library(Sleuth3)

### Data
data(case1701)
names(case1701)

case1701

# 11 magnetic force measurements (taking a subset of the columns)
case1701[, 1:11]

# check correlation between magnetic force measurements
cor(case1701[, 1:11])
pairs(case1701[, 1:11])


attach(case1701)
### PCA
fit.pca <- prcomp(case1701[, 1:11])
# if we want to standardize the response:
# fit.pca <- prcomp(case1701[, 1:11], scale = TRUE)

names(fit.pca)
summary(fit.pca)
fit.pca$x
fit.pca$rotation

#We can check the standard deviations of the PCs and find that the results match summary(fit.pca)
apply(fit.pca$x, 2, sd)

#we can see that the vector of loadings for each PC has length 1
apply(fit.pca$rotation, 2, function(x) sum(x^2))

### Choosing surrogates for the PCs
# check loadings
fit.pca$rotation

#define surrogates
avg <- apply(case1701[, 1: 11], 1, mean)
ends <- apply(case1701[,8:11], 1, mean) - apply(case1701[,1:6], 1, mean) 
mid <- apply(case1701[,1:4], 1, mean) - L6

### Compare surrogates with PCs
# PC1 vs avg
PC1 <- fit.pca$x[,1]
plot(PC1, avg)
cor(PC1, avg)

#PC2 vs ends~avg
PC2 <- fit.pca$x[,2]
endslm <- lm(ends~avg)
ends_resid <- endslm$residuals
plot(PC2, ends_resid)
cor(PC2, ends_resid)

#PC3 vs mid~avg+end
PC3 <- fit.pca$x[,3]
midlm <- lm(mid ~ avg + ends)
mid_resid <- midlm$residuals
plot(PC3, mid_resid)
cor(PC3, mid_resid)

#comparing PCS
pairs(data.frame(PC1, PC2, PC3))

### Univariate analyses of surrogates
library(car) #loading Anova function for type II anova
Anova(lm(avg ~ factor(Current) + factor(Material) + factor(Config) ), type = "II")
Anova(lm(ends ~ factor(Current) + factor(Material) + factor(Config) ), type = "II")
Anova(lm(mid ~ factor(Current) + factor(Material) + factor(Config) ), type = "II")

detach(case1701)
