library(tidyverse)


# read data downloaded by shell script ------------------------------------

boston <- read.table("./ch03_data_preprocessing/housing.data")
names(boston) <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad",
                   "tax", "ptratio", "black", "lstat", "medv")
glimpse(boston)

plot(boston)
summary(boston)

library("sqldf")

# Sql Practice in R dataframe...----------------------------------------------------

sql_1 <- "select Species, count(*), avg(`Sepal.Length`) from iris group by `Species`"
sqldf(sql_1)


sql_2 <- "select Species, `Sepal.Length`, `Sepal.Width` from iris 
          where `Sepal.Length` < 4.5 order by `Sepal.Width`"
sqldf(sql_2)

df1 <- tibble(x =c(1, 2), y = 2:1)
df2 <- tibble(x = c(1, 3), a=10, b = "a")

# inner join...
sql_3 <- "select * from df1 inner join df2 on df1.x = df2.x"
sqldf(sql_3)

df1 %>%
  inner_join(df2, by = "x")

# left_join...
sql_4 <- "select * from df1 left join df2 on df1.x = df2.x"
sqldf(sql_4)

df1 %>%
  left_join(df2, by = "x")


# R basic funcs... --------------------------------------------------------

# rev()...
c(1,2,3) %>% rev()
# ?reorder

require(graphics)
InsectSprays %>% head()
InsectSprays %>% tail()

# reorder()...
bymedian <- with(InsectSprays, reorder(spray, count, median))
bymedian

boxplot(count ~ bymedian, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE,
        col = "lightgray")

# relevel()...
data("warpbreaks")
warpbreaks %>% str()

warpbreaks$tension <- relevel(warpbreaks$tension, ref = "M")
str(warpbreaks)

summary(lm(breaks ~ wool + tension, data = warpbreaks))

# cut()...
Z <- stats::rnorm(10000)
table(cut(Z, breaks = -6:6))
sum(table(cut(Z, breaks = -6:6, labels = FALSE)))
graphics::hist(Z, breaks = -6:6)


tx0 <- c(9, 4, 6, 5, 3, 10, 5, 3, 5)
x <- rep(0:8, tx0)
stopifnot(table(x) == tx0)
table( cut(x, b = 8))
table( cut(x, breaks = 3*(-2:5)))
table( cut(x, breaks = 3*(-2:5), right = FALSE))

## label construnction::dig.lab...
y <- stats::rnorm(100)
table(cut(y, breaks = pi/3*(-3:3)))
table(cut(y, breaks = pi/3*(-3:3), dig.lab = 4))

## findInterval()....
x <- 2:18
v <- c(5, 10, 15) # create two bins [5,10) and [10,15)
cbind(x, findInterval(x, v))

N <- 100
X <- sort(round(stats::rt(N, df = 2), 2))
tt <- c(-100, seq(-2, 2, len = 201), +100)
it <- findInterval(tt, X)
tt[it < 1 | it >= N] # only first and last are outside range(X)

## aperm()...
x  <- array(1:24, 2:4)
xt <- aperm(x, c(2,1,3))


library(gapminder)
gapminder[gapminder$country == 'Korea, Rep.', c('pop', 'gdpPercap')]
gapminder[gapminder$country == 'Korea, Rep.' & gapminder$year == 2007, ]
f2 <- gapminder
names(f2)

# change var name...
names(f2)[6] = 'gap_per_cap'

# mutate or tansform vars...
f2$total_gap = f2$pop + f2$gap_per_cap

# compute summaries...
median(gapminder$gdpPercap)
apply(gapminder[, 4:6], 2, mean)
summary(gapminder)


# dplyr...
gapminder %>%
  summarise(
    n_obs = n_distinct(country),
    n_years = n_distinct(year),
    med_gdpc = median(gdpPercap),
    max_gdppc = max(gdpPercap)
  )

gapminder %>% select(country) %>% distinct()

gapminder %>%
  group_by(continent) %>%
  summarise(median(lifeExp))

df1
df2
df1 %>%
  full_join(df2, by = 'x')

## union and union_all...
mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ]
second <- mtcars[10:32, ]

union(first, second)
union_all(first, second)