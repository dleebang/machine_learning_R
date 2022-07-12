# the approaches described so far estimate the conditional probability directly
# and do not consider the distribution of the predictors: these are called
# as the discrimnative approaches.
# 
# Baye's theorem tells us that knowing the distribution of the predictors X
# may be useful.
# 
# Generative models are methods that model the JOINT DISTRIBUTION of outcomes
# and predictors (we model hor the entire data, X and Y, are generated).
# 
# The most general generative model is the NAIVE BAYES.
# 
# Look at the formula for NAIVE BAYES at https://rafalab.github.io/dsbook/examples-of-algorithms.html#generative-models
# The formula implies that if we can estimate these conditional distributions
# of the predictors, we can develop a powerful decision rule. This is a bif if,
# because we will encounter examples in which X has many dimensions and we 
# do not have much information about the distribution. In these cases,
# Naive Bayes will be practically impossible to implement, but when we have
# a small number of predictors (not much more than 2), the generative models
# can be quite powerful.