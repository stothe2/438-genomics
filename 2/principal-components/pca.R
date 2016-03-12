library(rafalib)

e <- read.delim("data/counts.txt", row.names=1)
tab <- read.delim("data/phen.txt", row.names=1)
c <- read.delim("data/cov.txt", row.names=1)

## Pre-processing
e_prime <- t(e) # Re-order data
L <- log2(1 + e_prime) # Log-transform data
Z <- scale(L) # Z-score

## Computing PCs and percent variance
pca <- prcomp(Z)
pca.var <- pca$sdev^2
pca.percent_var <- pca.var/sum(pca.var)
print(pca.percent_var[1:10])

## Scatter plot of first two PC loadings
cols <- c('pink', 'red')
par(mfrow = c(1, 1))
plot(pca$x[,1], pca$x[,2], col=cols[tab$disease+1], pch=16, xlab="PC1", ylab="PC2")
legend("bottomleft", legend=c("Normal","Parkinson's"), col=cols, pch=16)

## Pairwise Pearson correlation between PCs and covariates
c_prime <- data.frame(t(c))
for (i in 1:10) { # For top 10 PCs
  for (j in 1:3) { # For all covariates
    c_test <- cor.test(pca$x[,i], c_prime[,j], use="pairwise.complete.obs", method="pearson")
    if (abs(c_test$estimate) > 0.2 & c_test$p.value < 0.05) {
      cat("PC", i, sep="")
      cat(" estimate =", c_test$estimate, "p-value =", c_test$p.value, "covariate =", colnames(c_prime)[j], "\n", sep=" ")
    }
  }
}

### Pairwise Pearson correlation between disease status and covariates
#cor_matrix <- matrix(nrow=2, ncol=2, dimnames= list(c("Normal", "Parkinson's"), c("estimate", "p-value")))
#c_test <- cor.test(pca$x[1:44,1], c_prime[1:44,1], use="pairwise.complete.obs", method="pearson")
#cor_matrix[1,1] = c_test$estimate
#cor_matrix[1,2] = c_test$p.value
