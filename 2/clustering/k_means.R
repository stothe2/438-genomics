library(rafalib)

e <- read.delim("data/expr.txt", row.names=1)
tab <- read.table("data/class_labels.txt", header=T)
tissue <- tab[["tissue"]]

km <- kmeans(t(e[1:5,]), centers=5, iter.max=10)

set.seed(1)
cbind(cluster=c(1,2,3,4,5), samples=km$size)
table(tissue=tissue, cluster=km$cluster)

## Visualizing
d <- dist(t(e)) # distance between sample points
km <- kmeans(t(e), centers=5)
mds <- cmdscale(d)

mypar(1,2)
plot(mds[,1], mds[,2], col=km$cluster, pch=16)
