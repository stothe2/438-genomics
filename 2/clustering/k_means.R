library(rafalib)

e <- read.delim("data/expr.txt", row.names=1)
tab <- read.table("data/class_labels.txt", header=T)
tissue <- tab[["tissue"]]

# t(e) re-orders the matrix so that row=sampleids and column=geneids
km <- kmeans(t(e[1:5,]), centers = 5, iter.max = 10)

table(tissue = tissue, cluster = km$cluster)
