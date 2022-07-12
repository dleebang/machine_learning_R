# Due to the connection between conditional probabilities and conditional expectations:
#     
#     p_k(x) = Pr(Y = k | X = x), for k = 1,...,K
#     
# We often only use the expectation to denote both the conditional probability and
# conditional expectation
# 
# 
# For continous outcomes, we define a loss function to evaluate the model. 
# The most commonly used one is MSE (Mean Squared Error), which is expected value
# of all the differences between Y_hat and Y, squared. The reason why we care
# about the conditional expectation in machine learning is that because the
# EXPECTED VALUE OF THE SQUARED ERROR (Y_hat - Y)^2 minimizes the MSE:
# 
#     Y_hat = E(Y | X = x) minimizes E{(Y_hat - Y)^2 | X = x}
#     
# Due to this property, a succinct description of the main task of machine learning
# is that we use data to estimate f(x) = E(Y | X = x) for any set of features (predictors)
# 
# THE MAIN WAY IN WHICH COMPETING MACHINE LEARNING ALGORITHMS DIFFER IS
# IN THEIR APPROACH TO ESTIMATING THIS EXPECTATION
# 