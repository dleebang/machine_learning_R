# ggplot can use loess in its geom_smooth function
# 

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth()

# but be careful with default parameters as THEY ARE RARELY OPTIMAL. 
# these can be changed accordingly:
# 

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(method = "loess", span = 0.15, method.args = list(degree=1))
