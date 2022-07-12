#Q1)
Pr(disease) = 0.02  #has disease
Pr(healthy) = 0.98  #healthy
P(+ | disease) = 0.85
P(- | healthy) = 0.90

P(+ | healthy ) = 1 - P(- | healthy) = 1 - 0.9 = 0.1

Pr (diseae | + ) = Pr( + | disease) * Pr(disease) / Pr(+)
 = P(+ | disease) * P(disease) / (Pr(+|disease) * P(disease)) + (P(+|healthy) * P(healthy))
 = (0.85 * 0.02) / ( (0.85 * 0.02) + (0.1 * 0.98) )
 = 0.1478261  

#Q2) 
N <- 10^6
P(+|disease) = 0.85
P(-|healthy) = 0.90
p_disease <- 0.02

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), N, replace=TRUE, prob=c(1 - p_disease, p_disease))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#answer
mean(test == 1)
mean(test == 0)

#Q3)
P(disease|-) = Pr(-|disease) * P(disease) / P(-)
 = P(-|disease) * P(disease) / P(-|disease) * P(disease) + P(-|healthy) * P(healthy)
 = (0.15 * 0.02) / ( (0.15 * 0.02) + (0.90 * 0.88) )

or 

mean(disease[test==0])

#Q4)
mean(disease[test==1])

#Q5)
mean(disease[test==1]) / mean(disease==1)

