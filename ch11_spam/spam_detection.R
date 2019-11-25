library(tidyverse)
library(RMySQL)

Packages <- c("MASS", "glmnet", "randomForest", "gbm", "rpart",
              "boot", "data.table", "ROCR", "gridExtra")
lapply(Packages, library, character.only = T)


setwd("C:/Users/Daniel/Documents/ipds_kr_R_book/ch11_spam/")
read.table("spambase.data", strip.white = T, sep = ",", header = F) %>% 
  as_tibble() -> data
names(data) <- c('word_freq_make', 'word_freq_address', 'word_freq_all', 'word_freq_3d',
                 'word_freq_our', 'word_freq_over', 'word_freq_remove', 'word_freq_internet', 'word_freq_order',
                 'word_freq_mail', 'word_freq_receive', 'word_freq_will', 'word_freq_people',
                 'word_freq_report', 'word_freq_addresses', 'word_freq_free', 'word_freq_business',
                 'word_freq_email', 'word_freq_you', 'word_freq_credit', 'word_freq_your', 
                 'word_freq_font', 'word_freq_000', 'word_freq_money', 'word_freq_hp',
                 'word_freq_hpl', 'word_freq_george', 'word_freq_650', 'word_freq_lab',
                 'word_freq_labs', 'word_freq_telnet', 'word_freq_857', 'word_freq_data',
                 'word_freq_415', 'word_freq_85', 'word_freq_technology', 'word_freq_1999',
                 'word_freq_parts', 'word_freq_pm', 'word_freq_direct', 'word_freq_cs', 
                 'word_freq_meeting', 'word_freq_original', 'word_freq_project', 'word_freq_re',
                 'word_freq_edu', 'word_freq_table', 'word_freq_conference', 'word_freq_;',
                 'word_freq_(', 'word_freq_[', 'word_freq_!', 'word_freq_$', 'word_freq_#',
                 'capital_run_length_average', 'capital_run_length_longest', 'capital_run_length_total',
                 'class')

data # %>% View()
# 
# con <- dbConnect(
#   MySQL(),
#   user = "root", 
#   password = "chr0n3!7!"
# )
# 
# dbSendQuery(con, "CREATE DATABASE spambase;")
# dbSendQuery(con, "USE spambase;")
# 
# dbWriteTable(con, "spambase", data, overwrite = T)
# dbDisconnect(con)

con_spam <- dbConnect(
  MySQL(),
  user = "root", 
  password = "chr0n3!7!",
  dbname = "spambase"
)

data_from_db <- dbGetQuery(con_spam, "select * from spambase;")
data_from_db %>% colnames()

data_from_db %>% dplyr::select(-row_names) -> data_spam
data_spam


data_spam %>% dplyr::select(1:10, 58) %>% sample_n(500) -> data_tmp_1

## ggpairs & pairs.panels...
library(GGally)
ggpairs(data_tmp_1)

library(psych)
pairs.panels(data_tmp_1)

library(corrgram)
corrgram(data_tmp_1, order = T, upper.panel = panel.cor)

?corrgram

data_spam %>% dplyr::select(48:57, 58) %>% sample_n(500) -> data_tmp_2
ggpairs(data_tmp_2)

cor(data_spam[, -58], as.numeric(data_spam$class)) %>% as.data.frame() %>%
  rename(cor = V1) -> tmp

tmp$var <- rownames(tmp)

?reorder

tmp %>%
  ggplot(aes(reorder(var, cor), cor)) + 
  geom_point() +
  coord_flip()

library(gridExtra)

data_spam$class <- as.factor(data_spam$class)

data_spam %>% ggplot(aes(class)) + geom_bar() -> p1; p1

data_spam %>% ggplot(aes(class, `word_freq_$`)) + 
  geom_jitter(col = 'gray') + geom_boxplot(alpha = .5) + scale_y_sqrt() -> p2; p2

data_spam %>% ggplot(aes(`word_freq_$`, group=class, fill = class)) + 
  geom_density(alpha = .5) + scale_x_sqrt() + scale_y_sqrt() -> p3; p3

data_spam %>% ggplot(aes(class, capital_run_length_longest)) +
  geom_jitter(col ='gray') +
  geom_boxplot(alpha = .5) + 
  scale_y_log10() -> p4; p4

grid.arrange(p1, p2, p3, p4, ncol = 2)

# `$` 즉 돈을 의미하는 표현이 많은 것인 spam 메일일 가능성이 크다...

# 특수문자 변수명 처리 -------------------------------------------------------------

library(randomForest)
# data_rf <- randomForest(class ~. , data = data_spam)

old_names <- names(data_spam)
new_names <- make.names(old_names, unique = T) # make syntactically valid names...

changed_nm_tbl <- cbind(old_names, new_names)[old_names != new_names, ] # beautiful coding...
changed_nm_tbl

names(data_spam) <- new_names

# Data Spliting -----------------------------------------------------------

nrow(data_spam) -> n
1:n %>% sample(., n*.6) -> train_idx
setdiff(1:n, train_idx) -> idx_without_train
idx_without_train %>% sample(., n*.2) -> validation_idx
setdiff(idx_without_train, validation_idx) -> test_idx

train_idx %>% length()
validation_idx %>% length()
test_idx %>% length()


train_data <- data_spam[train_idx, ]
validation_data <- data_spam[validation_idx, ]
test_data <- data_spam[test_idx, ]


library(caret)

train_idx <- createDataPartition(data_spam$class, p=.8, list =F)[, 1]
test_idx <- setdiff(1:nrow(data_spam), train_idx)

# re_train_idx <- sample(train_idx, length(train_idx)*.9)
# val_idx <- setdiff(train_idx, re_train_idx)

train_data <- data_spam[train_idx, ]
test_data <- data_spam[test_idx, ]

train_data %>% dim()
test_data %>% dim()

val_idx <- createFolds(train_data$class, k = 10, list = T)[[1]]
re_train_idx <- setdiff(1:nrow(train_data), val_idx)

# 이후 해볼 과제/...Cross Validation을 해보자...

re_train_idx %>% length
val_idx %>% length
test_idx %>% length


data_spam %>% nrow()
re_train_idx %>% length + val_idx %>% length + test_idx %>% length

re_train_data <- data_spam[re_train_idx, ]
validation_data <- data_spam[validation_idx, ]
test_data <- data_spam[test_idx, ]

re_train_data$class <- as.factor(re_train_data$class)
validation_data$class <- as.factor(validation_data$class)
test_data$class <- as.factor(test_data$class)

re_train_data %>% dim()
validation_data %>% dim()
test_data %>% dim()

re_train_data$class %>% table() %>% prop.table() # 약 50:50...왜 그럴까??
validation_data$class %>% table() %>% prop.table() # 약 50:50...왜 그럴까??
test_data$class %>% table() %>% prop.table()

train_data %>% nrow()
test_data %>% nrow()

# #  upSample().... ---------------------------------------------------------
# 
# upsampled <- upSample(train_data %>% dplyr::select(-Class), train_data$Class)
# 
# train_data %>% nrow()
# train_data %>% colnames()
# 
# upsampled %>% nrow()
# upsampled %>% colnames()
# 
# 
# train_data %>% dplyr::select(class) %>% table()
# upsampled %>% dplyr::select(Class) %>% table() # Why Class?? 왜 대문자로 바뀌었을까??...
# 
# 
# train_data %>% nrow()
# re_train_idx <- createDataPartition(train_data$class, p = .8, list = F)[, 1]
# validation_idx <- setdiff(1:nrow(train_data), re_train_idx)
# 
# 
# re_train_idx %>% length()
# validation_idx %>% length()
# 
# 
# re_train_data = train_data[re_train_idx, ]
# validation_data = train_data[validation_idx, ]


# Logistic Regression -----------------------------------------------------

train_data %>% class()
lm_full <- glm(class ~., data = re_train_data, family = binomial)
summary(lm_full)

# upsampled %>% class()
# lm_full_upsampled <- glm(Class ~ ., data = upsampled, family = binomial)
# summary(lm_full_upsampled)

# Validation --------------------------------------------------------------
library(ROCR)

y_obs <- as.numeric(as.character(validation_data$class))
yhat_lm <- predict(lm_full, newdata = validation_data, type = 'response')
pred_lm <- prediction(yhat_lm, y_obs)
performance(pred_lm, "auc")@y.values[[1]]

binomial_deviance <- function(y_obs, yhat) {
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1- epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs == 0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return((2*sum(a+b)))
}

binomial_deviance(y_obs, yhat_lm)


# Lasso -------------------------------------------------------------------

xx <- model.matrix(class ~.-1, data_spam) # -1 :: 절편항은 필요하지 않으므로...
x <- xx[train_idx, ]
y <- as.numeric(as.character(train_data$class))
glimpse(x)

data_cvfit <- cv.glmnet(x, y, family = "binomial")
plot(data_cvfit)

coef(data_cvfit, s = c("lambda.1se"))
coef(data_cvfit, s = c("lambda.min"))

predict.cv.glmnet(data_cvfit, s = "lambda.min", newx = x[1:5, ], type = "response")

yhat_glmnet <- predict(data_cvfit, s= "lambda.min", newx = xx[validation_idx, ], 
                       type = 'response')

yhat_glmnet <- yhat_glmnet[, 1] # change to a vector from [n*1] matrix....

pred_glmnet <- prediction(yhat_glmnet, y_obs)
performance(pred_glmnet, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_glmnet)

# Tree Model ---------------------------------------------------------------

data_tr <- rpart(class ~., data = re_train_data)
data_tr

printcp(data_tr)
summary(data_tr)

opar <- par(mfrow  = c(1, 1), xpd=NA)
plot(data_tr)
text(data_tr, use.n = T)
par(opar)

yhat_tr <- predict(data_tr, validation_data)
yhat_tr <- yhat_tr[, "1"]
pred_tr <- prediction(yhat_tr, y_obs)
performance(pred_tr, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_tr)

# RandomForest ------------------------------------------------------------

set.seed(2019)
data_rf <- randomForest(class ~., re_train_data)
data_rf

opar <- par(mfrow = c(1, 2))
plot(data_rf)
varImpPlot(data_rf)
par(opar)

yhat_rf <- predict(data_rf, newdata = validation_data, type = 'prob')[, '1']
pred_rf <- prediction(yhat_rf, y_obs)
performance(pred_rf, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_rf)

# 종합하기.... ----------------------------------------------------------------

method = c('glmnet', 'tr', 'rf')
auc = c(performance(pred_glmnet, "auc")@y.values[[1]], 
        performance(pred_tr, "auc")@y.values[[1]],
        performance(pred_rf, "auc")@y.values[[1]])

bin_dev = c(binomial_deviance(y_obs, yhat_glmnet),
            binomial_deviance(y_obs, yhat_tr),
            binomial_deviance(y_obs, yhat_rf))


data.frame(method = method, 
           auc = auc,
           bin_dev = bin_dev)

perf_glmnet <- performance(pred_glmnet, measure = "tpr", x.measure = "fpr")
perf_tr <- performance(pred_tr, measure = "tpr", x.measure = "fpr")
perf_rf <- performance(pred_rf, measure="tpr", x.measure = "fpr")
# perf_gbm <- performance(pred_gbm, measure = "tpr", x.measure = "fpr")

plot(perf_glmnet, col="black", main="ROC Curve")
plot(perf_tr, add=T, col="blue")
plot(perf_rf, add=T, col="red")
# plot(perf_gbm, add=T, col="cyan")
abline(0, 1)
legend('bottomright', insert.1, 
       legend=c("glmnet", "TREE", "RF"), 
       col = c('black', 'blue', 'red'), lty=1, lwd=2)

# rf is winner...
# we apply test data to randomforest model...

y_obs_test <- test_data$class %>% as.character() %>% as.numeric()
yhat_rf_test <- predict(data_rf, newdata=test_data, type='prob')[, '1']
pred_rf <- prediction(yhat_rf_test, y_obs_test)


performance(pred_rf, "auc")@y.values[[1]]
binomial_deviance(y_obs_test, yhat_rf_test)


# Boosting ----------------------------------------------------------------

set.seed(2019)
data_for_gbm <- train_data %>%
  mutate(class = as.numeric(as.character(class)))

data_gbm <- gbm(class ~., data = data_for_gbm, distribution = "bernoulli", 
                n.trees=100000, cv.folds=3, verbose=T)

(best_iter= gbm.perf(data_gbm, method="cv"))

yhat_gbm <- predict(data_gbm, n.trees=best_iter, newdata=validation_data, type='response')
pred_gbm <- prediction(yhat_gbm, y_obs)
performance(pred_gbm, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_gbm)

# -------------------------------------------------------------------------


method = c('lm', 'glmnet', 'rf', 'gbm')
auc = c(performance(pred_glmnet, "auc")@y.values[[1]], 
        performance(pred_tr, "auc")@y.values[[1]],
        performance(pred_rf, "auc")@y.values[[1]], 
        performance(pred_gbm, "auc")@y.values[[1]])

bin_dev = c(binomial_deviance(y_obs, yhat_glmnet),
            binomial_deviance(y_obs, yhat_tr),
            binomial_deviance(y_obs, yhat_rf),
            binomial_deviance(y_obs, yhat_gbm))


data.frame(method = method, 
           auc = auc,
           bin_dev = bin_dev)

perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
perf_glmnet <- performance(pred_glmnet, measure = "tpr", x.measure = "fpr")
perf_rf <- performance(pred_rf, measure="tpr", x.measure = "fpr")
perf_gbm <- performance(pred_gbm, measure = "tpr", x.measure = "fpr")

plot(perf_lm, col="black", main="ROC Curve")
plot(perf_glmnet, add=T, col="blue")
plot(perf_rf, add=T, col="red")
plot(perf_gbm, add=T, col="cyan")
abline(0, 1)
legend('bottomright', insert.1, 
       legend=c("GLM", "glmnet", "RF", "GBM"), 
       col = c('black', 'blue', 'red', 'cyan'), lty=1, lwd=2)

# rf is winner...
# we apply test data to randomforest model...

y_obs_test <- test_data$class %>% as.character() %>% as.numeric()
yhat_rf_test <- predict(data_rf, newdata=test_data, type='prob')[, '1']
pred_rf <- prediction(yhat_rf_test, y_obs_test)


performance(pred_rf, "auc")@y.values[[1]]
binomial_deviance(y_obs_test, yhat_rf_test)

# Visualization
data.frame(y_obs=y_obs,
                 yhat_lm = yhat_lm,
                 yhat_glmnet = yhat_glmnet, 
                 yhat_rf = yhat_rf,
                 yhat_gbm = yhat_gbm) %>%
  ggpairs()






