#' ---
#' title: "ch04(ggplot2)"
#' author: "jakinpilla"
#' date : "`r format(Sys.time(), '%Y-%m-%d')`"
#' output: 
#'    github_document : 
#'        pandoc_args: --webtex
#'        toc : true
#' ---

#+ message = FALSE, warning = FALSE
library(ez)
library(ggplot2)
library(nlme)
library(pastecs)
library(reshape2)
library(WRS)
library(clinfun)
library(pgirmess)
library(car)
library(tidyverse)
library(sqldf)
library(gapminder)
library(gridExtra)

#' Read data downloaded by shell script 

boston <- read.table("./housing.data")
glimpse(gapminder)

opar = par(mfrow=c(2,2))
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap, nclass=10)
hist(log(gapminder$gdpPercap), nclass=10)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex=.5)
par(opar)

cor(gapminder$lifeExp, log10(gapminder$gdpPercap))

#' 동일하게 ggplot()을 이용하여 그릴 수 있다.
p1 <- gapminder %>% ggplot(aes(x = lifeExp)) + geom_histogram()
p2 <- gapminder %>% ggplot(aes(x = gdpPercap)) + geom_histogram()
p3 <- gapminder %>% ggplot(aes(x = gdpPercap)) + geom_histogram() + scale_x_log10()
# scale_x_log10()은 x축의 scale을 log10으로 변환시켜 준다.
p4 <- gapminder %>% ggplot(aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  scale_x_log10() + geom_smooth()
grid.arrange(p1, p2, p3, p4, ncol=2)

gl(3, 10, labels = c('a', 'b', 'c'))
df <- data.frame(gp = factor(rep(letters[1:3], each = 10)), y = rnorm(30))
df <- data.frame(gp = gl(3, 10, labels = c('a', 'b', 'c')), y = rnorm(30))
ds <- df %>% group_by(gp) %>% summarise(mean = mean(y), sd = sd(y))
ds

#' geom_point()와 geom_errorbar() 함수를 활용해보자.
ggplot() +
  geom_point(data = df,aes(x = gp, y = y)) +
  geom_point(data = ds, aes(x = gp, y = mean), colour = 'red', size = 3) +
  geom_errorbar(data = ds, aes(x = gp, y = mean, 
                               ymin = mean - sd, 
                               ymax = mean + sd),
                
                colour = 'red', width = .4)

#' geom_errorbar() 함수의 옵션을 조정하여 보자.
ggplot() +
  geom_point(data = df,aes(x = gp, y = y)) +
  geom_point(data = ds, aes(x = gp, y = mean), colour = 'red', size = 3) +
  geom_errorbar(data = ds, aes(x = gp, y = mean, 
                               ymin = mean - sd, 
                               ymax = mean + sd),
                
                colour = 'blue', width = .2)


data("diamonds", "mpg")
diamonds
mpg

#' One numeric var...

p1 <- gapminder %>% ggplot(aes(gdpPercap)) + geom_histogram()
p2 <- gapminder %>% ggplot(aes(gdpPercap)) + geom_histogram() + scale_x_log10()
p3 <- gapminder %>% ggplot(aes(gdpPercap)) + geom_freqpoly() + scale_x_log10()
p4 <- gapminder %>% ggplot(aes(gdpPercap)) + geom_density() + scale_x_log10()
grid.arrange(p1, p2, p3, p4, ncol=2)

#' One factor var...
#' 
#' 막대그래프를 그려본다.
diamonds %>% ggplot(aes(cut)) + geom_bar()
diamonds %>%
  group_by(cut) %>%
  tally() %>%
  mutate(pct = round(n / sum(n) * 100, 1))

#' Two numeric var...
#' - Scatter plot...

diamonds %>% ggplot(aes(carat, price)) + geom_point()
diamonds %>% ggplot(aes(carat, price)) + geom_point(alpha = .01)
mpg %>% ggplot(aes(cyl, hwy)) + geom_point()
mpg %>% ggplot(aes(cyl, hwy)) + geom_jitter()

pairs(diamonds %>% sample_n(1000))

#' #### Numeric and factor var...
#' - Boxplot...

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

#' #### Two factor vars....

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
  geom_point() + # geom_point()에 대한 옵션이 없음.
  scale_x_log10() +
  ggtitle("Gapminder data for 2007") -> p1

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(gdpPercap,lifeExp)) +
  geom_point(aes(size = pop, col = continent)) + # size pop 옵션을 통해 pop 크기에 대한 ponit 사이즈를 조절함.
  scale_x_log10() +
  ggtitle("Gapminder data for 2007") -> p2

grid.arrange(p1, p2, ncol=2)

#' ### Faceting

gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line()

gapminder %>%
  ggplot(aes(year, lifeExp, group = country, col = continent)) +
  geom_line()

#' 대륙별 년도의 변화에 따른 기대수명의 변화를 `facet_wrap()` 함수를 통해 알아보자.
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line() +
  facet_wrap(~ continent)















