library(tidyverse)

options(digits=7)
set.seed(1986, sample.kind="Rounding")
#simulate 1000 schools with random number of students
n <- round(2^rnorm(1000, 8, 1))

#assign a true quaility for each school independent from size. This is the
#parameter we want to estimate in our analysis.
set.seed(1, sample.kind="Rounding") 
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

#We can see the top 10 schools
schools %>% top_n(10, quality) %>% arrange(desc(quality))

#Simulate students take a school test. There is random variability in test taking
#so we will simulate the test scores as normally distributed with the average
#determined by school quality and standard deviation of 30 percentage pts. 
set.seed(1, sample.kind="Rounding")
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})

#calculate mean scores for each school
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1)
#what are the top schools based on the avg score? Show just the ID, size, and
#the avg score
top_10_scores <- schools %>% select(id, size, score) %>% top_n(10) %>% arrange(desc(score))

#Q2)
median(schools$size)
schools %>% top_n(10) %>% summarize(median(size))

#Q3)
schools %>% 
  select(id, size, score) %>% 
  arrange(score) %>% 
  slice(1:10) %>% 
  .$size %>% 
  median()

#Q4)
schools %>% ggplot(aes(score, size)) +
  geom_point(alpha = 0.3) +
  geom_point(data = filter(schools, rank <= 10), col = "red", size = 3)

#Q5)
#use regularization to shrink deviation from the average towards 0.
#first define overal average for all schools
overall <- mean(sapply(scores, mean))

#now define how each school deviates from the overall average
a <- 25
head(schools)
reg_scores <- sapply(scores, function(l) overall + (sum(l-overall) / (length(l) + a)) )

schools %>% mutate(reg_score = reg_scores) %>% 
  top_n(10, reg_score) %>% arrange(desc(reg_score))


#Q6)
RMSE <- function(quality, estimate) {
  sqrt(mean((quality - estimate)^2))
}

alfas <- seq(10, 250)

rmses <- sapply(alfas, function(a) {
  estimate <- sapply(scores, function(l) overall + (sum(l-overall) / (length(l) + a)))
  sqrt(mean((estimate - schools$quality)^2))
})

qplot(alfas, rmses)
alfas[which.min(rmses)]


#Q7)
optimized_alfa <- alfas[which.min(rmses)]

reg_scores <- sapply(scores, function(l) overall + (sum(l-overall) / (length(l) + optimized_alfa)) )

schools %>% mutate(reg_score = reg_scores) %>% 
  top_n(10, reg_score) %>% arrange(desc(reg_score))


#Q8)
alfas <- seq(10, 250)

rmses <- sapply(alfas, function(a) {
  estimate <- sapply(scores, function(l) (sum(l) / (length(l) + a)))
  sqrt(mean((estimate - schools$quality)^2))
})

qplot(alfas, rmses)
alfas[which.min(rmses)]
