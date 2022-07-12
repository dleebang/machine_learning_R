# Taylor's theorem also tells us that if you look at any mathematical
# function closely enough, it looks like a parabola. It also states that you
# dont have to look as closely when approximating with parabolas as you do when
# approximating with lines. 
# 
# This means we can make the window size even larger and fit parabolas instead
# of lines. This is actually the default for loess function, which sets the
# degree argument to 2. Degree argument = 1 fit polynomials of degree 1 (line)
#  
# 
## Code
#compare plots of loess fitting parabolas and lines
total_days <- diff(range(polls_2008$day))
span <- 28/total_days

#line
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

#parabolas
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)

#plot
polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1) 


