library(rafalib)

e <- read.delim("data/expr.txt", row.names=1)
tab <- read.table("data/class_labels.txt", header=T)
tissue <- tab[["tissue"]]

## K-means
set.seed(1)
km <- kmeans(t(e), centers=t(e)[1:5,], iter.max=10)
cbind(cluster=c(1,2,3,4,5), samples=km$size)
table(tissue=tissue, cluster=km$cluster)

## Bayesian Information Criterion
bic <- function(fit) {
  k <- nrow(fit$centers) # number of clusters
  m <- ncol(fit$centers) * k # number of free parameters
  n <- length(fit$cluster) # number of samples
  D <- fit$tot.withinss
  return (D + m*log(n))
}

bic.values <- c()
for (k in 2:10) {
  bic.values[k-1] <- bic(kmeans(t(e), centers=t(e)[1:k,], iter.max=10))
}
par(mfrow = c(1, 1))
plot(c(2:10), bic.values, xlab="Number of clusters (k)", ylab="BIC scores", pch=1)

## Visualizing
d <- dist(t(e)) # distance between sample points
km <- kmeans(t(e), centers=5)
mds <- cmdscale(d)

mypar(1,1)
plot(mds[,1], mds[,2], col=km$cluster, pch=16)
legend(-16.2, -2.5, legend=c(1:5), col=c(1:5), pch=16)
