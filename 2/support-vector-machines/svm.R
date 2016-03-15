# Each DNA sequence is of length 60.
# We'll use phenotype labels "S":spliced and "N":not-spliced.

library(dplyr) # for manipulating tabular data

train_sequence <- read.table("data/train_sequence.txt", header=T)
test_sequence <- read.table("data/test_sequence.txt", header=T)
#head(train_dat)

base <- c("A", "T", "C", "G")

combns <- expand.grid(base, base)
combns <- within(combns, two_mers<-paste(Var1, Var2, sep=""))
two_mers <- combns$two_mers

combns <- expand.grid(base, base, base)
combns <- within(combns, three_mers<-paste(Var1, Var2, Var3, sep=""))
three_mers <- combns$three_mers

combns <- expand.grid(base, base, base, base)
combns <- within(combns, four_mers<-paste(Var1, Var2, Var3, Var4, sep=""))
four_mers <- combns$four_mers


count.three_mers <- 0
count.four_mers <- 0

for (i in 1:dim(train_sequence)[1]) {
  count.two_mers <- 0
  for (j in 1:length(two_mers)) {
    count.two_mers <- count.two_mers + length(gregexpr(two_mers[j], train_sequence[i,1])[[1]])
  }
  cat(i, count.two_mers, "\n", sep=" ")
}
