#----Install and/or load needed libraries----#
install.packages("nFactors")
library(nFactors)
library(tidyverse)


#-----Find principal components-----#
PCs <- prcomp(~ ., data = okDat, scale = TRUE)
summary(PCs)

par(mfrow = c(1, 2))

plot(PCs$sdev^2/sum(PCs$sdev^2), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = 'b')
plot(cumsum(PCs$sdev^2/sum(PCs$sdev^2)), xlab = "Principal Component",
     ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')


#-----Estimate number of factors to retain with scree test-----#
#15, it seems
okScr <- nScree(okDat)
okScr

#Examine the eigenvalues
#The first 15 are > 1.0, so this suggests about 15 factors; the first 5 >> 1.0
round(eigen(cor(okDat))$values, 4)


#-----Estimate EFA model with k = 15 factors-----#
factanal(okDat, factors = 15)


