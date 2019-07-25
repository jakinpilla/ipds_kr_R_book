library(tidyverse)

# loading packages -------------------------------------------------------

setwd('C:/Users/Daniel/Documents/ipds_kr_R_book/ch08_classification/adult')

Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally',
              'ROCR', 'ISLR','glmnet', 'gbm', 'boot', 'randomForest', 'dummies', 
              'curl', 'gridExtra')
lapply(Packages, library, character.only=T)


# loading data ------------------------------------------------------------

adult <- read.csv("adult.data", header = F, strip.white = T)

names(adult) <- c("age", "workclass", "fnlwgt", "education", "education_num",
                  "marital_status", "occupation", "relationship", "race", "sex",
                  "capital_gain", "capital_loss", "hours_per_week", "native_country",
                  "wage")
adult %>% head()


# Define binomial_deviance function ---------------------------------------

binomial_deviance <- function(y_obs, yhat) {
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1- epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs == 0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return((2*sum(a+b)))
}

set.seed(2019)
n <- nrow(adult)
idx <- 1:n
training_idx <- sample(idx, n * .6)

idx <- setdiff(idx, training_idx) # reflesh idx...
validate_idx <- sample(idx, n * .2)
test_idx <- setdiff(idx, validate_idx)

length(training_idx)
length(validate_idx)
length(test_idx)

training <- adult[training_idx, ]
validation <- adult[validate_idx, ]
test <- adult[test_idx, ]


# -------------------------------------------------------------------------
xx <- model.matrix(wage ~.-1, adult)
x <- xx[training_idx, ]
x
y <- ifelse(training$wage == ">50K", 1, 0)
dim(x)


# glmnet() ----------------------------------------------------------------

ad_glmnet_fit <- glmnet(x, y) # default is lasso model... alpha = 1

plot(ad_glmnet_fit) # coeficient profile plot....

ad_glmnet_fit

coef(ad_glmnet_fit, s = c(.1713, .1295))


# cv.glmnet() -------------------------------------------------------------

ad.cvfit <- cv.glmnet(x, y, family = "binomial")
plot(ad.cvfit)

log(ad.cvfit$lambda.min)
log(ad.cvfit$lambda.1se)

length(which(coef(ad.cvfit, s = "lambda.min") > 0))
length(which(coef(ad.cvfit, s = "lambda.1se") > 0))


# case when alpha 0, .5, 1 ------------------------------------------------

set.seed(2019)
foldid <- sample(1:10, size = length(y), replace = T)
cv1 <- cv.glmnet(x, y, foldid = foldid, alpha = 1, family = 'binomial')
cv.5 <- cv.glmnet(x, y, foldid = foldid, alpha = .5, family = 'binomial')
cv0 <- cv.glmnet(x, y, foldid = foldid, alpha = 0, family = 'binomial')

par(mfrow=c(2,2))
plot(cv1, main = "Alpha=1.0")
plot(cv.5, main = "Alpha=0.5")
plot(cv0, main = "Alpha=0.0")
plot(log(cv1$lambda), 
     cv1$cvm,
     pch=19,
     col = 'red',
     xlab = 'log(Lambda)',
     ylab = cv1$name,
     main = "alpha=1.0")
points(log(cv.5$lambda), cv.5$cvm, pch = 19, col = "blue")
points(log(cv0$lambda), cv0$cvm, pch = 19, col = "grey")
legend("topleft", legend=c("alpha = 1", "alpha = .5", "alpha = 0"),
       pch = 19, col =c('red', 'grey', 'blue'))
par(mfrow=c(1,1))

predict(ad.cvfit, s = "lambda.1se", newx = x[1:5, ], type = 'response')


# validation --------------------------------------------------------------

# with validation, to find out predict values...
y_obs <- ifelse(validation$wage == ">50K", 1, 0)
yhat_glmnet <- predict(ad.cvfit, s = "lambda.1se", new = xx[validate_idx, ], 
                       type = 'response')

# yhat_glmnet <- predict(ad.cvfit, s = "lambda.min", new = xx[validate_idx, ], 
#                        type = 'response')

yhat_glmnet %>% as_tibble() %>% pull() -> yhat_glmnet

binomial_deviance(y_obs, yhat_glmnet)

pred_glmnet <- prediction(yhat_glmnet, y_obs)

perf_glmnet <- performance(pred_glmnet, measure = "tpr", x.measure = "fpr")

plot(perf_lm, col = 'black', main = 'ROC Curve')
plot(perf_glmnet, col = 'blue', add = T)
abline(0, 1)
legend('bottomright', inset=.1,
       legend = c("GLM", "glmnet"),
       col = c('black', 'blue'), 
       lty = 1, lwd = 2)
performance(pred_glmnet, "auc")@y.values[[1]]


# Tree Model --------------------------------------------------------------

cvr_tr <- rpart(wage ~ ., data = training)
cvr_tr

printcp(cvr_tr)
summary(cvr_tr)

opar <- par(mfrow = c(1, 1), xpd = NA)
plot(cvr_tr)
text(cvr_tr, use.n = T)
par(opar)

yhat_tr <- predict(cvr_tr, validation)
yhat_tr <- yhat_tr[, ">50K"]

binomial_deviance(y_obs, yhat_tr)

pred_tr <- prediction(yhat_tr, y_obs)
perf_tr <- performance(pred_tr, measure = 'tpr', x.measure = 'fpr')
plot(perf_lm, col = 'black', main = "ROC Curve")
plot(perf_tr, col = 'blue', add = T)
abline(0, 1)
legend('bottomright', inset = .1, 
       legend = c("GLM", "Tree"),
       col = c("black", "blue"),
       lty = 1, lwd = 2)

performance(pred_tr, "auc")@y.values[[1]]


# RandomForest ------------------------------------------------------------

set.seed(2019)
ad_rf <- randomForest(wage ~ ., training)
ad_rf

plot(ad_rf)

tmp <- importance(ad_rf)
tmp %>% class()
tmp %>% 
  as.data.frame() %>%
  mutate(var = rownames(.)) %>%
  as_tibble() -> tmp.1

colnames(tmp.1) <- c("mean_gini_desc", "var")

tmp.1 %>%
  select(var, mean_gini_desc) %>% 
  arrange(desc(mean_gini_desc))

varImpPlot(ad_rf)



# RandomForest Predict ----------------------------------------------------

predict(ad_rf, newdata = adult[1:5, ])

predict(ad_rf, newdata = adult[1:5, ], type = "prob")


# -------------------------------------------------------------------------

yhat_rf <- predict(ad_rf, newdata = validation, type = 'prob')[, '>50K']
y_obs <- ifelse(validation$wage == ">50K", 1, 0)

binomial_deviance(y_obs, yhat_rf)

pred_rf <- prediction(yhat_rf, y_obs)
perf_rf <- performance(pred_rf, measure = "tpr", x.measure = 'fpr')


# Plottting with three models:: glmnet, tr, rf ---------------------------

perf_glmnet %>% plot(col = 'black', main = 'ROC Curve')

perf_tr %>% plot(col = 'blue', add = T)

perf_rf %>% plot(col = 'red', add = T)

abline(0, 1)

legend('bottomright', inset=.1, 
       legend = c("GLM", "glmnet", "RF"),
       col = c('black', 'blue', 'red'), lty = 1, lwd = 2)

library(gridExtra)

performance(pred_glmnet, "auc")@y.values[[1]]
performance(pred_tr, "auc")@y.values[[1]]
performance(pred_rf, "auc")@y.values[[1]]

data.frame(yhat_glmnet, yhat_rf) %>%
  ggplot(aes(yhat_glmnet, yhat_rf)) +
  geom_point(alpha=.5) +
  geom_smooth(method = 'gam') -> p1

# reshape2::melt(data.frame(yhat_glmnet, yhat_rf)) -> df.1
# 
# data.frame(yhat_glmnet, yhat_rf) %>%
#   gather(key = "variable", value = "value") -> df.2
# 
# data.frame(yhat_glmnet, yhat_rf) %>%
#   gather(`yhat_glmnet`, `yhat_rf`, key = "variable", value = "value") -> df.3
#   
# df.1$value[1:10]
# df.2$value[1:10]
# df.3$value[1:10]
# 
# identical(df.1$value, df.2$value)
# identical(df.1$value, df.3$value)

data.frame(yhat_glmnet, yhat_rf) %>%
  gather(`yhat_glmnet`, `yhat_rf`, key = "variable", value = "value") %>%
  ggplot(aes(value, fill = variable)) +
  geom_density(alpha = .5)  -> p2

grid.arrange(p1, p2, ncol = 2)


# Boosting ----------------------------------------------------------------

set.seed(2019)

adult_gbm <- training %>% mutate(wage = ifelse(wage == ">50K", 1, 0))

ad_gbm <- gbm(wage ~ ., data = adult_gbm, 
              distribution = "bernoulli", 
              n.trees = 50000, cv.folds = 3, 
              verbose = T)

















