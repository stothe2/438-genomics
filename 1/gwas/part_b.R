library(dplyr) # For manipulating tabular data
library(rafalib)

geno_dat <- read.csv("data/genotype.csv")
pheno_dat <- read.csv("data/phenotype.csv")
dat <- merge(pheno_dat, geno_dat)

sample_size <- dim(dat)[1]
total_columns <- dim(dat)[2]
p_vec <- numeric(9088)

for (i in 3:total_columns) { # For each SNP
	snp_col <- dat[,i]
	aa <- sum(snp_col==0)
	ab <- sum(snp_col==1)
	bb <- sum(snp_col==2)
	f_a <- (2 * aa + ab) / (2 * sample_size)
	f_b <- (2 * bb + ab) / (2 * sample_size)

	if (min(f_a, f_b) >= 0.05) {
		logitfit <- glm(V1 ~ factor(snp_col), data = dat, family = binomial) # Factor to indicate that SNP column is to be used as a categorical variable

		try(p_value_0 <- coef(summary(logitfit))[2,4] * 9088, silent = TRUE) # Bonferroni correction to maintain FWER of 0.05
		try(p_value_1 <- coef(summary(logitfit))[3,4] * 9088, silent = TRUE)
		if (p_value_0 < 0.05) {
			cat(colnames(dat)[i], p_value_0, "phenotype=0", "\n", sep=" ")
		} else if (p_value_1 < 0.05) {
			cat(colnames(dat)[i], p_value_1, "phenotype=1", "\n", sep=" ")
		}
	}
}
