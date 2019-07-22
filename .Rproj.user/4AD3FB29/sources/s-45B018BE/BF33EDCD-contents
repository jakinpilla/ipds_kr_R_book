y <- sleep$extra[sleep$group == 1]

summary(y)
sd(y)
par(mfrow=c(2, 2))
hist(y)
boxplot(y)
qqnorm(y); qqline(y)
hist(y, prob = T); lines(density(y), lty = 2)
par(mfrow=c(1, 1))

t.test(y)

# One Sample t-test
# 
# data:  y
# t = 1.3257, df = 9, p-value = 0.2176
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.5297804  2.0297804
# sample estimates:
#   mean of x 
# 0.75

t.test(y, alternative = 'greater')

# One Sample t-test
# 
# data:  y
# t = 1.3257, df = 9, p-value = 0.1088
# alternative hypothesis: true mean is greater than 0
# 95 percent confidence interval:
#   -0.2870553        Inf
# sample estimates:
#   mean of x 
# 0.75 

curve(dnorm(x, 0, 1.8), -4, 4)

options(digits = 3)
set.seed(2019)

y_star <- rnorm(10, 0, 1.8)
mean(y_star - 0); sd(y_star)

t_star <- mean(y_star - 0) / (sd(y_star)/sqrt(length(y_star)))
t_star

y_star <- rnorm(10, 0, 1.8)
mean(y_star - 0); sd(y_star)

t_star <- mean(y_star - 0) / (sd(y_star) / sqrt(length(y_star)))
t_star

y_star <- rnorm(10, 0, 1.8)
mean(y_star - 0); sd(y_star)

t_star <- mean(y_star - 0) / (sd(y_star) / sqrt(length(y_star)))
t_star

set.seed(2019)
B <- 1e4
n <- 10
xbars_star <- rep(NA, B)
sds_star <- rep(NA, B)
ts_star <- rep(NA, B)

for(b in 1:B) {
  y_star <- rnorm(n, 0, 1.789)
  m <- mean(y_star)
  s <- sd(y_star)
  xbars_star[b] <- m
  sds_star[b] <- s
  ts_star[b] <- m / (s/sqrt(n))
}

opar <- par(mfrow = c(2,2))
hist(xbars_star, nclass = 100)
abline(v = 0.75, col = 'red')

hist(sds_star, nclass = 100)
abline(v = 1.789, col = 'red')

hist(ts_star, nclass = 100)
abline(v = 1.3257, col = 'red')

qqnorm(ts_star); qqline(ts_star)

length(which(ts_star > 1.3257)) / B

# 신뢰구간의 의미...

set.seed(2019)

y_star <- rnorm(10, 1, 1.8)
t.test(y_star)$conf.int

y_star <- rnorm(10, 1, 1.8)
t.test(y_star)$conf.int

y_star <- rnorm(10, 1, 1.8)
t.test(y_star)$conf.int

set.seed(2019)
B <- 1e2
conf_intervals <- 
  data.frame(b = rep(NA, B),
             lower = rep(NA, B),
             xbar = rep(NA, B),
             upper = rep(NA, B))

true_mu <- 1.0

for(b in 1:B){
  (y_star <- rnorm(10, true_mu, 1.8))
  conf_intervals[b, ] = c(b=b,
                          lower=t.test(y_star)$conf.int[1],
                          xbar=mean(y_star),
                          upper=t.test(y_star)$conf.int[2])
}


conf_intervals <- conf_intervals %>%
  mutate(lucky = (lower <= true_mu & true_mu <= upper))

glimpse(conf_intervals)

table(conf_intervals$lucky)

conf_intervals %>%
  ggplot(aes(b, xbar, col = lucky)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax = upper)) +
  geom_hline(yintercept = true_mu, col = 'red')

par(mfrow = c(1,1))
hist(c(0,1), nclass=100, prob = TRUE, main = 'Individual sleep time increase')
set.seed(2019)

B <- 1e4
n <- 10

xbars_star < rep(NA, B)

for(b in 1:B) {
  xbars_star[b] <- mean(sample(c(0,1), size = n, replace = T))
}

hist(xbars_star, nclass= 100, main = 'Sample mean of 10 obs')











