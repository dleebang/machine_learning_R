# We use the notation (X1 = x1,..., Xp = xp) to represent observed values x1,...,xp
# for covariates X1,...,Xp. 
# 
# This does not imply that the outcome Y will take a specific value, but instead
# a specific probability. In particular, we denote the CONDITIONAL PROBABILITIES
# for each class k as:
# 
#     p_k(x) = Pr(Y = k | X = x), for k = 1,...,K
# 
# These probabilities guide an algorithm that makes the best prediction:
# for any given x, we will predict class k with the largest probability
# among p1(x), p2(x), ... pk(x). In mathematical notation this is written like:
# 
#     Y_hat = max_k * p_k(x)
# 
# 
# In machine learning, this is referred to as Bayes' Rule. This is a theoretical
# rule because in practice we don't know p_k(x). The better our probability estimates
# p_hat_k(x), the better our predictor:
# 
#     Y_hat = max_k * p_hat_k(x)
# 
# The prediction will depend on two things:
#     
#     1) how close are the max_k * p_k(x) to 1 or 0 (perfect certainty)
#     2) how close estimates p_hat_k(x) are to p_k(x)
# 
# Having a good estimate of the p_k(x) will suffice for us to build optmial prediction models, 
# since we can control the balance between specificity and sensitivity however we wish. In fact,
# estimating these conditional probabilities can be thought as the main challenge
# of machine learning.
# 
# 