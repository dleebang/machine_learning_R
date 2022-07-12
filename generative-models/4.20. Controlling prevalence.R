# The naive bayes approach includes a parameter to account for differences in
# prevalence pi = Pr(Y=1). If we use hats to denote the estimates, we can write
# p_hat(x) as:
# 
#   p_hat(x) = Pr(Y=1|X=x) = f_hat_X|Y=1(x) * pi_hat / f_hat_X|Y=0(x) * (1 - pi_hat) + f_hat_X|Y=1(x) * pi_hat
#   
# The naive bayes approach gives us a direct way to correct the imbalance
# between sensitivity and specificity by simply forcing pi_hat to be whatever
# value we want it to be in order to better balance sensitiviy and specificity.
# 
# 
## Code
#code in previous scripts
# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Changing the cutoff of the decision rule with pi = 0.5 to balance
# sensitivity and specificity
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# Draw plot
# this shows that a best cutoff is between 66 and 67, which is
# about the middle of female and male average heights
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)
