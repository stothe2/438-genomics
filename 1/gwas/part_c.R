library(dplyr) #  for manipulating tabular data
library(rafalib)

geno_dat <- read.csv("data/genotype.csv")
pheno_dat <- read.csv("data/phenotype.csv")
dat <- merge(pheno_dat, geno_dat)

sample_size <- dim(dat)[1]

# SNP 10 is assummed to be rs623043.
index <- which(colnames(dat) == "rs623043")
x <- dat[,index] # snp10
u <- mean(x)

logitfit <- glm(V1 ~ factor(x), data = dat, family = binomial)
#summary(logitfit)

# Logistic coefficients give change in log of odds of outcome for
# one unit change in predictor.
b <- coef(logitfit)
#prob_y_is_1 <- 1 / (1 + exp(u + exp(b * x)))
print(b)
print(u)
newdata1 <- data.frame(x)
predict.prob <- predict(logitfit, newdata = newdata1, type = "response")
predicted.Y <- ifelse(predict.prob > 0.5, 1, -1)
LL <- sum(log(1 + exp(-predicted.Y * (u + b * x))))
print(LL)
