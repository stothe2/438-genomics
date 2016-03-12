library(rafalib)

e <- read.delim("data/counts.txt", row.names=1)
tab <- read.delim("data/phen.txt")
c <- read.delim("data/cov.txt", row.names=1)

## Pre-processing
e_prime <- t(e) # Re-order data
L <- log2(1 + e_prime) # Log-transform data
Z <- scale(L) # Z-score

## Computing PCs and percent variance
pca <- prcomp(Z)
pca.var <- pca$sdev^2
pca.percent_var <- pca.var/sum(pca.var)

## Scatter plot of first two PC loadings
cols <- c('pink', 'red')
par(mfrow = c(1, 1))
plot(pca$x[,1], pca$x[,2], col=cols[tab$disease+1], pch=16, xlab="PC1", ylab="PC2")
legend("bottomleft", legend=c("Normal","Parkinson's"), col=cols, pch=16)
