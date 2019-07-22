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

diamonds %>% ggplot(aes(carat, price)) + geom_point()
diamonds %>% ggplot(aes(carat, price)) + geom_point(alpha = .01)
mpg %>% ggplot(aes(cyl, hwy)) + geom_point()
mpg %>% ggplot(aes(cyl, hwy)) + geom_jitter()

pairs(diamonds %>% sample_n(1000))

# Numeric and factor var...

mpg %>% ggplot(aes(class, hwy)) + geom_boxplot()

library(gridExtra)

mpg %>% ggplot(aes(class, hwy)) + geom_jitter(col = 'gray') + 
  geom_boxplot(alpha = .5) -> p1

mpg %>% mutate(class = reorder(class, hwy, median)) %>%
  ggplot(aes(class, hwy)) + geom_jitter(col = 'gray') +
  geom_boxplot(alpha = .5) -> p2

mpg %>% mutate(class = factor(class, levels = 
                                c('2seater', 'subcompact', 'compact', 'midsize',
                                  'minivan', 'suv', 'pickup'))) %>%
  ggplot(aes(class, hwy)) + geom_jitter(col = 'grey') +
  geom_boxplot(alpha = .5) -> p3


mpg %>% mutate(class = factor(class, levels = 
                                c('2seater', 'subcompact', 'compact', 'midsize',
                                  'minivan', 'suv', 'pickup'))) %>%
  ggplot(aes(class, hwy)) + geom_jitter(col = 'grey') +
  geom_boxplot(alpha = .5) + coord_flip() -> p4

grid.arrange(p1, p2, p3, p4, ncol = 2)

par(mfrow = c(1, 1))

# Two factor vars....

glimpse(data.frame(Titanic))

xtabs(Freq ~ Class + Sex + Age + Survived, data.frame(Titanic))

mosaicplot(Titanic, main = 'Survival on the Titanic')

age_survive <- apply(Titanic, c(3,4), sum)
prop.table(age_survive, margin = 1) %>% round(., 3)


sex_survived <- apply(Titanic, c(2,4), sum)
prop.table(sex_survived, margin = 1) %>% round(., 3)

t2 = data.frame(Titanic)
t2 %>% group_by(Sex) %>%
  summarise(n = sum(Freq),
            survivors = sum(if_else(Survived == 'Yes', Freq, 0))) %>%
  mutate(rate_survived = survivors/n)

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(gdpPercap,lifeExp)) +
  geom_point() +
  scale_x_log10() +
  ggtitle("Gapminder data for 2007") -> p1

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(gdpPercap,lifeExp)) +
  geom_point(aes(size = pop, col = continent)) +
  scale_x_log10() +
  ggtitle("Gapminder data for 2007") -> p2

grid.arrange(p1, p2, ncol=2)

# facet

gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line()

gapminder %>%
  ggplot(aes(year, lifeExp, group = country, col = continent)) +
  geom_line()

windows()
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line() %>%
  facet_wrap(~ continent)

# Error in sanitise_dim(nrow) : 'pairlist' object cannot be coerced to type 'integer'













