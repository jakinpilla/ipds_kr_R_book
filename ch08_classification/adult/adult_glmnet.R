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
y
dim(x)


# glmnet() ----------------------------------------------------------------

ad_glmnet_fit <- glmnet(x, y) # default is lasso model... alpha = 1

plot(ad_glmnet_fit) # coeficient profile plot....

ad_glmnet_fit

coef(ad_glmnet_fit, s = c(.1713, .1295))


# cv.glmnet() -------------------------------------------------------------

ad.cvfit <- cv.glmnet(x, y, family = "binomial") # glmnet 함수는 디폴트로 alpha = 1 즉 라쏘를 사용한다.
plot(ad.cvfit)

# 그림 상단에는 0이 아닌 선택된 변수의 숫자가 써있다.
# 왼쪽으로 갈수록 복잡한(모든 X 변수를 포함한) 모형이다.
# 오른쪽으로 갈수록 간단한(상수 평균만 적합한) 모형이다.
# 모형의 적합도는 이항편차(Binomial Deviance)로 주어진다.(y축에 해당)
# 각 lambda 값에서 k-fold 교차검증은 k-개의 테스트 오차값을 산출하고 그값들의 표준편차 값 오차범위(error bar)로 나타나 있다.
# 빨간점은 주어진 lambda 값에서의 k개의 교차검증의 평균값이다.


log(ad.cvfit$lambda.min) # 최적의 예측
log(ad.cvfit$lambda.1se) # 해석 가능한 모형

length(which(coef(ad.cvfit, s = "lambda.min") > 0))
length(which(coef(ad.cvfit, s = "lambda.1se") > 0))

ad.cvfit %>% coef(s = 'lambda.min') %>% as.matrix() %>%
  as.data.frame() %>%
  rename(value = 1) %>%
  filter(value > 0) %>%
  nrow()

ad.cvfit %>%
  coef(s = 'lambda.1se') %>%
  as.matrix() %>%
  as.data.frame() %>%
  rename(value = 1) %>%
  filter(value > 0) %>%
  nrow()

# case when alpha 0, .5, 1 ------------------------------------------------
# alpha = 1: Lasso
# alpha = 0: Ridge
# alpha =.5: ElasticNet

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

predict(ad.cvfit, s = "lambda.1se", newx = x[1:5, ], type = 'response') # 확률 예측값을 반환
predict(ad.cvfit, s = "lambda.1se", newx = x[1:5, ], type = 'link') # 링크함수값을 반환


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
training %>% nrow()
test %>% nrow()

training %>% head()
test %>% head()

cvr_tr <- rpart(wage ~ ., data = training)
cvr_tr

# Post-pruning
# 먼저 tree 를 완전히 자라게 한 다음 Bottom-up 방식으로 tree 를 가지 치기하는 방식이다.
# Reduced Error Pruning:  가지치기를 시도해 보고  만일 generalization error가 개선 된다면 subtree를 단말 node 로 대치한다.
# rpart 에서는 error 계산을 위하여 training data set으로 X(cross) validation한다.
# 단말 node 의 class label은 node에서 다수를 차지하는 record의 class label로 정함. 

# rpart 패키지에서는 cp값을 증가시켜가며 tree 크기를 감소시켜 x validation error(xerror)을 계산한다.
# 이때 xerror이 최소로 하는 cp가 최적이다.

# Validation of decision tree using the ‘Complexity Parameter’ and cross validated error :
printcp(cvr_tr)
plotcp(cvr_tr)

summary(cvr_tr)

opar <- par(mfrow = c(1, 1), xpd = NA)
plot(cvr_tr)
text(cvr_tr, use.n = T)
# par(opar)

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
# Bagging : Bootstrap Aggregation...
# b = 1, 2, 3, ..., B 부트스트랩 샘플을 얻은 후 각각의 샘플에 비교적 간단한 모형을 적합하여
# 최종적으로 각 모형의 다수결 투표 혹은 예측값의 평균을 최종 예측값으로 하는 것을 Bagging이라 한다.

# p개의 변수 중 랜덤하게 선택한 m개의 변수만을 고려한다.
# 분류문제: m = sqrt(p), 회귀 예측문제 : m = p/3 을 보통 이용한다.
# 각 부트스르랩 모형이 서로 다른 변수를 포함하도록 강제하고, 소수의 강력한 예측변수가 모든 나무 모형에
# 나타나서 모든 나무 모형이 유사하게 되는 것을 방지한다.

set.seed(2019)
ad_rf <- randomForest(wage ~ ., training)
ad_rf

plot(ad_rf)

tmp <- importance(ad_rf)
tmp

tmp %>% class()

tmp %>% 
  as.data.frame() %>%
  mutate(var = rownames(.)) %>%
  as_tibble() -> tmp.1

tmp.1 

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

# false positive rate가 작은 영역을 살펴보면 이 영역에서 RF는 다른 모형보다 true positive rate 가 더 높다.
# false positive rate가 작은 영역은 예측이 상대적으로 쉬운 관측치라고 할 수 있는데 만약 예측의 목적이
# 아주 작은 관측치에 대해 높은 true positive rate를 내는 것이 목적이라면 rf모델을 사용하는 것이 더 좋을 수 있다.

library(gridExtra)

performance(pred_glmnet, "auc")@y.values[[1]]
performance(pred_tr, "auc")@y.values[[1]]
performance(pred_rf, "auc")@y.values[[1]]

data.frame(yhat_glmnet, yhat_rf) %>%
  ggplot(aes(yhat_glmnet, yhat_rf)) +
  geom_point(alpha=.5) +
  geom_smooth() -> p1

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

# RF는 예측 확률값이 대부분 0과 1에 위치하는 극단적은 극단적인 분포를 보이는 반면에,
# glmnet은 예측 확률의 분포가 좀 더 균등하다는 것이다...
 

# Boosting ----------------------------------------------------------------

set.seed(2019)

adult_gbm <- training %>% mutate(wage = ifelse(wage == ">50K", 1, 0))

ad_gbm <- gbm(wage ~ ., data = adult_gbm, 
              distribution = "bernoulli", 
              n.trees = 50000, cv.folds = 3, 
              verbose = T)



# install.packages("caret", dependencies = c("Depends", "Suggest"))













