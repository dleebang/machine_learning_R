# A very common approach to evaluating accuracy and F1-score is to compare
# them graphically by plotting both. A widely used plot that does this is the 
# RECEIVING OPERATING CHARACTERISTIC (ROC) curve. The ROC curve plots sensitivity
# (True positive rate - TPR) versus 1 - specificity or the false positive rate (FPR)
# 
# However, ROC curves have one weakness and it is that neither of the measures
# plotted depend on prevalence. In cases in which prevalence matters, we may instead
# make a PRECISION-RECALL plot, which has a similar idea with ROC curve.
# 
# packages pROC or plotROC

## Code
p <- 0.9
n <- length(test_index)

y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex) #accuracy

# ROC curve
probs <- seq(0, 1, length.out = 10)

# calculate sensitivity and specificity for guessing algorithm
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot ROC curve
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

# calculate sensitivity and specificity for height cutoffs
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

#plot both curves together
#all values of sensitivity are higher than for specificity, implying
#that it is in fact a better method than just guessing with probs.
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() + 
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)

# plot ROC curve with each cutoff value plotted
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)


#plot precision vs recall

#calculate recall (sensitivity) and precision (positive predictive values) for
#guessing
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

#calculate recall (sensitivity) and precision (positive predictive values) for
#height cutoffs
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

#plot precision-recall curves
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

#change positives to mean male instead of females
#guessing
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

#height_cutoffs
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

#plot inverted positives
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

# we see that for both Y = 1 Female or Y = 1 Male, precision of guessing
# is not high and for height_cutoffs is always heigher. This is because
# the prevalence is low
