#----Install and/or load needed libraries----#
install.packages("nFactors")
install.packages("semPlot")
install.packages("GPArotation")
install.packages("parameters")
install.packages("performance")

library(nFactors)
library(semPlot)
library(GPArotation)
library(tidyverse)
library(psych)
library(parameters)
library(performance)
library(lavaan)

#-----Checking that dataset is suitable for factor analysis-----#
#Yes, very much so!
performance::check_factorstructure(okDat)


#-----Find principal components-----#
#Response(s) must be removed
pcDat <- okDat[,-c(34,67)]

PCs <- prcomp(~ ., data = pcDat, scale = TRUE)
summary(PCs)


#Contribution of variance levels out at around 4 to 5 PCs; may consider no more than that
plot(PCs, type = 'l')

par(mfrow = c(1, 2))
plot(PCs$sdev^2/sum(PCs$sdev^2), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = 'b')
abline(v=15)
plot(cumsum(PCs$sdev^2/sum(PCs$sdev^2)), xlab = "Principal Component",
     ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')
abline(v = c(5,10,15), h = c(0.4,0.5,0.6))


#-----Estimate number of factors to retain with scree test-----#
#15 seems to be the majority
okScr <- nScree(pcDat)
okScr

#Examine the eigenvalues

#The first 15 are > 1.0, so this suggests about 15 factors; the first 5 >> 1.0,
#which is consistent with the PCA
round(eigen(cor(pcDat))$values, 4)


#-----Estimate and Compare EFA models with k = 5 vs 15 factors-----#

okEFA15 <- factanal(pcDat, factors = 15, scores = "regression", rotation = "oblimin")
#EFAPromax <- factanal(pcDat, factors = 15, scores = "regression", rotation = "promax")

print(okEFA15$loadings, sort = TRUE, cutoff = 0.3)


#-----Fit EFA using psych package-----#
faOK <- fa(pcDat, nfactors = 15) %>%
  model_parameters(sort = TRUE, threshold = "max")

#Determining number of factors to retain based on various methods/metrics
n <- n_factors(pcDat)

plot(n) + see::theme_modern()
