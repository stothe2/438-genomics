library(rafalib)

e <- read.delim("data/counts.txt", row.names=1)
tab <- read.delim("data/phen.txt")
c <- read.delim("data/cov.txt", row.names=1)

## Pre-processing
e_prime <- t(e) # Re-order data
L <- log2(1 + e_prime) # Log-transform data
Z <- scale(L) # Z-score

## Computing PCs
pca <- prcomp(L)
pca.var <- pca$sdev^2

## Plots of first two PC loadings
par(mfrow = c(2, 2))
plot(pc$rotation[, 1], ylim = c(-0.7, 0.7))
