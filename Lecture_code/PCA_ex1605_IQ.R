library(Sleuth3)

attach(ex1605)

iq <- ex1605[,3:6]

cor(iq)

iq.pca <- prcomp(iq)

summary(iq.pca)
iq.pca$x
iq.pca$rotation
