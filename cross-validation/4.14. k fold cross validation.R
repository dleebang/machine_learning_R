# For k-fold cross validation, we divide the dataset into a training set
# and a test set. We train our algorithm exclusively on the training set and
# use the test only for evaluation purposes.
# 
# For each set of algorithm parameters being considered, we want an ESTIMATE
# OF THE MSE AND THEN WE WILL CHOOSE THE PARAMETERS THA PROVIDES THE SMALLEST 
# MSE. In k-fold cross validation, we randomly split the training set into
# k non-overlapping sets, and repeat the calculation for MSE for each of these sets.
# Then, we compute the average MSE and obtain an estimate of our loss. 
# Finally, we can select the optimal parameter that minimized the MSE and 
# use the optimal parameters to use on the test set.
# 
# After selecting optimal parameters for the training set, you can gather again
# the entire dataset and repeat the procedure by dividing the set into 
# randomly split test sets and pick parameters again for the entire set.
# 
# In terms of how to select k for cross validation, larger values of k
# are preferable but they will also take much more computational time.
# For this reason, choises of k = 5 and k = 10 are common.