library(dplyr) # For manipulating tabular data
library(rafalib)

dat <- read.csv("data/genotype.csv")

counter0 <- 0 # Counter for SNPs with MAF > 0.1
counter1 <- 0 # Counter for SNPs with MAF > 0.05
counter2 <- 0 # Counter for SNPs with MAF > 0.03

sample_size <- dim(dat)[1]
total_columns <- dim(dat)[2]

for (i in 2:total_columns) { # For each SNP
	snp_col <- select(dat, i)
	aa <- sum(snp_col==0)
	ab <- sum(snp_col==1)
	bb <- sum(snp_col==2)
	f_a <- (2 * aa + ab) / (2 * sample_size)
	f_b <- (2 * bb + ab) / (2 * sample_size)

	#cat(colnames(col), min(f_A, f_B), "\n", sep="\t")

	maf <- min(f_a, f_b)
	if (maf > 0.1)
		counter0 <- counter0 + 1
	if (maf > 0.05)
		counter1 <- counter1 + 1
	if (maf > 0.03)
		counter2 <- counter2 + 1
}

cat("# of SNPs with MAF greater than 0.03 is", counter2, "\n", sep=" ")
cat("# of SNPs with MAF greater than 0.05 is", counter1, "\n", sep=" ")
cat("# of SNPs with MAF greater than 0.1 is", counter0, "\n", sep=" ")
