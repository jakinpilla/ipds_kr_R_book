#' ---
#' title: "ch03(SQL and dplyr)"
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


#' read data downloaded by shell script ------------------------------------

boston <- read.table("./housing.data")

names(boston) <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad",
                   "tax", "ptratio", "black", "lstat", "medv")
glimpse(boston)

boston %>% dim() 
1:nrow(boston) %>% sample(100) -> idx

#'  그래프 그려지는 속도를 원활하게 하기 위해 데이터를 임의로 100개만 선택하여 산점도를 그린

plot(boston[idx, ])
summary(boston)

#' Sql Practice in R dataframe...----------------------------------------------------

sql_1 <- "select Species, count(*), avg(`Sepal.Length`) from iris group by `Species`"
sqldf(sql_1)

#' '.'이 있는 변수명들이 있어 back tic으로 변수명을 감싼다.

sql_2 <- "select Species, `Sepal.Length`, `Sepal.Width` from iris 
          where `Sepal.Length` < 4.5 order by `Sepal.Width`"

sqldf(sql_2)

df1 <- tibble(x =c(1, 2), y = 2:1); df1
df2 <- tibble(x = c(1, 3), a=10, b = "a"); df2

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


#' R basic funcs... --------------------------------------------------------
#' 
#' rev()...
c(1,2,3) %>% rev()
# ?reorder

require(graphics)
InsectSprays %>% head()
InsectSprays %>% tail()

#' reorder()...
bymedian <- with(InsectSprays, reorder(spray, count, median))
bymedian

with(iris, reorder(Species, Sepal.Length, median))

boxplot(count ~ bymedian, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE,
        col = "lightgray")

#' relevel()...

data("warpbreaks")
warpbreaks %>% str()

# ' tension 내 'M'을 reference level로 하여 tension열에 팩터 순위를 변경한다.

warpbreaks$tension <- relevel(warpbreaks$tension, ref = "M")
warpbreaks %>% str()

summary(lm(breaks ~ wool + tension, data = warpbreaks))

#' iris 데이터를 이용해 relevel과 reorder 함수에 활용 연습을 해보자.

iris -> iris_1
iris_1 %>% str()

#' versicolor를 reference level로 설정한다.
#'
iris_1$Species <- relevel(iris_1$Species, ref = 'versicolor')
iris_1 %>% str()

#' factor의 level 순위에 의해 x축이 정렬된다.
ggplot(iris_1, aes(Species, Sepal.Length)) +
  geom_boxplot()

#' Sepal.Length의 median값 순으로 Species 열의 level을 정렬하여 보자.
iris_1$Species <- with(iris_1, reorder(Species, Sepal.Length, median))

ggplot(iris_1, aes(Species, Sepal.Length)) +
  geom_boxplot()


#' cut()...
Z <- stats::rnorm(10000)
table(cut(Z, breaks = -6:6))
sum(table(cut(Z, breaks = -6:6, labels = FALSE)))
graphics::hist(Z, breaks = -6:6)

#' cut() 함수로 만들어진 데이터로 bar plot을 그려보자.
table(cut(Z, breaks = -6:6)) -> a
a %>% data.frame() -> df_a
colnames(df_a) <- c('range', 'freq')
df_a %>% 
  ggplot(aes(range, freq)) + geom_bar(stat = 'identity')


tx0 <- c(9, 4, 6, 5, 3, 10, 5, 3, 5)
x <- rep(0:8, tx0)
stopifnot(table(x) == tx0)

cut(x, b = 8) # b는 breaks의 줄임말이다.
cut(x, breaks = 8)

table( cut(x, b = 8))
table( cut(x, breaks = 3*(-2:5)))
table( cut(x, breaks = 3*(-2:5), right = FALSE))

#' label construnction::dig.lab...
y <- stats::rnorm(100)
table(cut(y, breaks = pi/3*(-3:3)))
table(cut(y, breaks = pi/3*(-3:3), dig.lab = 4)) # dig.lab 옵션은  표시되는 숫자의 자리수를 결정한다.

#' findInterval()....
x <- 2:18
v <- c(5, 10, 15) # create two bins [5,10) and [10,15)
u <- c(5, 7, 10, 13, 15) # create two bins [5,10) and [10,15)

#' 두 개의 구간이면 0, 1, 2, 3 등 4개의 인덱스를 반환하고 
findInterval(x, v)

#' 네 개의 구간이면 0, 1, 2, 3, 4, 5 등 6개의 인덱스를 반환한다.
findInterval(x, u)

cbind(x, findInterval(x, v))
cbind(x, findInterval(x, u))

N <- 100
X <- sort(round(stats::rt(N, df = 2), 2))
tt <- c(-100, seq(-2, 2, len = 201), +100)
it <- findInterval(tt, X)
tt[it < 1 | it >= N] # only first and last are outside range(X)

#' aperm()...
x  <- array(1:24, 2:4)
xt <- aperm(x, c(2,1,3))


myarray <- array( 1:24, dim=c(2,3,4),
                  dimnames=list(One=c('a','b'), Two=c('A','B','C'), Three=1:4) )

myarray
aperm(myarray, c(3,1,2))

myarray[2,3,4]

mynewarray <- aperm(myarray, c(3,1,2) )
mynewarray[4,2,3]


library(gapminder)
gapminder[gapminder$country == 'Korea, Rep.', c('pop', 'gdpPercap')]
gapminder[gapminder$country == 'Korea, Rep.' & gapminder$year == 2007, ]
f2 <- gapminder
names(f2)

#' change var name...
names(f2)[6] = 'gap_per_cap'

#' mutate or tansform vars...
f2$total_gap = f2$pop + f2$gap_per_cap

#' compute summaries...
median(gapminder$gdpPercap)
apply(gapminder[, 4:6], 2, mean)
summary(gapminder)


#' Using dplyr...
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

#' union and union_all...
mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ]
second <- mtcars[10:32, ]

union(first, second)
union_all(first, second)
