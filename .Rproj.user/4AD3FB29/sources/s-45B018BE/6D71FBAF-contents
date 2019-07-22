library(gapminder)
glimpse(gapminder)

opar = par(mfrow=c(2,2))
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap, nclass=10)
hist(log(gapminder$gdpPercap), nclass=10)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex=.5)
par(opar)

cor(gapminder$lifeExp, log10(gapminder$gdpPercap))

library(gridExtra)
p1 <- gapminder %>% ggplot(aes(x = lifeExp)) + geom_histogram()
p2 <- gapminder %>% ggplot(aes(x = gdpPercap)) + geom_histogram()
p3 <- gapminder %>% ggplot(aes(x = gdpPercap)) + geom_histogram() + scale_x_log10()
p4 <- gapminder %>% ggplot(aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  scale_x_log10() + geom_smooth()
grid.arrange(p1, p2, p3, p4, ncol=2)


df <- data.frame(gp = factor(rep(letters[1:3], each = 10)), y = rnorm(30))
ds <- df %>% group_by(gp) %>% summarise(mean = mean(y), sd = sd(y))

ggplot(df, aes(x = gp, y = y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)

ggplot() +
  geom_point(data = df,aes(x = gp, y = y)) +
  geom_point(data = ds, aes(x = gp, y = mean), colour = 'red', size = 3) +
  geom_errorbar(data = ds, aes(x = gp, y = mean, 
                               ymin = mean - sd, 
                               ymax = mean + sd),
                
                colour = 'red', width = .4)

data("diamonds", "mpg")
diamonds
mpg

# One numeric var...

p1 <- gapminder %>% ggplot(aes(gdpPercap)) + geom_histogram()
p2 <- gapminder %>% ggplot(aes(gdpPercap)) + geom_histogram() + scale_x_log10()
p3 <- gapminder %>% ggplot(aes(gdpPercap)) + geom_freqpoly() + scale_x_log10()
p4 <- gapminder %>% ggplot(aes(gdpPercap)) + geom_density() + scale_x_log10()
grid.arrange(p1, p2, p3, p4, ncol=2)

# One factor var...

diamonds %>% ggplot(aes(cut)) + geom_bar()
diamonds %>%
  group_by(cut) %>%
  tally() %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# Two numeric var...

diamonds %>% ggplot()














