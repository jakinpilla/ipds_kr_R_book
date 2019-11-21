data <- tbl_df(read.table("wpbc.data", strip.white = T, 
                          sep = ",", header = F)) 

data %>% dim()

feature_names <- c('radius', 'texture', 'perimeter', 'area', 'smoothness', 'compactness', 
                   'convavity', 'concave_points', 'symmetry', 'fractal_dim')

length(feature_names)

c('id', 'class', 'time',
  paste0('mean_', feature_names), 
  paste0('se_', feature_names),
  paste0('worst_', feature_names),
  'tumor_size', 'lymp_status') -> col_nm

colnames(data) <- col_nm

data

# classificaion : N or R

data$class %>% unique()

data %>% ggplot(aes(class)) + geom_bar()

data %>%
  mutate(class = recode(class, 
                        'R' = 1, 
                        'N' = 0)) %>%
  mutate(class = as.factor(class)) -> data_1

data_1 %>%
  dplyr::select(lymp_status) %>%
  filter(lymp_status != '?') %>%
  mutate(lymp_status =as.numeric(as.character(lymp_status))) %>% pull() %>% mean() 

data_1 %>%
  mutate(lymp_status = as.character(lymp_status)) %>%
  mutate(lymp_status = case_when(lymp_status == '?' ~ '3', TRUE ~ lymp_status)) %>% # TRUE ~ lymp_status 옵션을 주지 않으면 '3'을 제외한 전 원소가 NA가 됨에 주의하자...
  mutate(lymp_status = as.numeric(lymp_status)) -> data_2

range(data_2$lymp_status)
data_2$lymp_status <- 72 - data_2$lymp_status

glimpse(data_2)


# Normalize... ------------------------------------------------------------

normalize <- function(x) {
  return( (x - min(x)) / (max(x) - min(x)) )
}

x <- c(1, 2, 3)
normalize(x)

data_2

data_2 %>%
  dplyr::select(-id, -class, -time) -> data_3

data_3

data_3 %>%
  mutate_all(list(~normalize(.))) -> data_4

data_4 %>% str()

data_2 %>%
  dplyr::select(class) %>%
  bind_cols(data_4) -> data_5


# Data Split....--------------------------------------------------------------

train_idx <- createDataPartition(data_5$class, p = .8, list = F)[, 1]
test_idx <- setdiff(1:nrow(data_5), train_idx)

train_data <- data_5[train_idx, ]
test_data <- data_5[test_idx, ]

train_data %>% count(class)
test_data %>% count(class)


# Modeling with glm...-------------------------------------------------------

m_glm <- glm(class ~ ., data = train_data, family = binomial)
m_glm

predict(m_glm, test_data, type = 'response') -> predicted_prob
predicted_class <- ifelse(predicted_prob > .5, 1, 0) %>% as.factor()
actual_class <- test_data$class %>% as.factor()

confusionMatrix(predicted_class, actual_class)

yhat_glm <- predict(m_glm, test_data, type = 'response')
y_obs <- test_data$class

y_pred_glm <- prediction(yhat_glm, y_obs)
y_perf_glm <- performance(y_pred_glm, measure = 'tpr', x.measure = 'fpr')

plot(y_perf_glm)

# Modeling with randomForest using caret package...--------------------------

train_data$class

fitControl = trainControl(method = 'repeatedcv', number = 10, repeats = 5)

m_rf <- train(class ~ ., data = train_data, method = 'rf', trControl = fitControl, verbose = T)

m_rf

par(mfrow = c(1, 2))
varImp(m_rf)
plot(varImp(m_rf))
plot(m_rf)
par(mfrow = c(1, 1))


yhat_rf <- predict(m_rf, newdata = test_data, type = 'prob')[, '1']
y_obs <- test_data$class

y_pred_rf <- prediction(yhat_rf, y_obs)
y_perf_rf <- performance(y_pred_rf, measure = 'tpr', x.measure  = 'fpr')

plot(y_perf_rf)

auc_rf <- performance(y_pred_rf, 'auc')@y.values[[1]]


# Modeling with rpart using caret package...--------------------------

fitControl = trainControl(method = 'repeatedcv', number = 10, repeats = 5)

# m_rpart <- train(class ~ ., data = train_data, method = 'rpart', trControl = fitControl, verbose = T)

m_rpart <- rpart(class ~ ., data = train_data)
m_rpart

yhat_rpart <- predict(m_rpart, newdata = test_data, type = 'prob')[, '1']
y_obs <- test_data$class

y_pred_rpart <- prediction(yhat_rpart, y_obs)
y_perf_rpart <- performance(y_pred_rpart, measure = 'tpr', x.measure  = 'fpr')

plot(y_perf_rpart)

auc_rpart <- performance(y_pred_rpart, 'auc')@y.values[[1]]
auc_rpart

# Modeling with gbm using caret package...--------------------------

fitControl = trainControl(method = 'repeatedcv', number = 10, repeats = 5)

m_gbm <- train(class ~ ., data = train_data, method = 'gbm', trControl = fitControl, verbose = T)

m_gbm

yhat_gbm <- predict(m_gbm, newdata = test_data, type = 'prob')[, '1']
y_obs <- test_data$class

y_pred_gbm <- prediction(yhat_gbm, y_obs)
y_perf_gbm <- performance(y_pred_gbm, measure = 'tpr', x.measure  = 'fpr')

plot(y_perf_gbm)

auc_gbm <- performance(y_pred_gbm, 'auc')@y.values[[1]]
auc_gbm


plot(y_perf_rpart)
plot(y_perf_glm, add = T, col = 'red')
plot(y_perf_rf, add = T, col = 'blue')
plot(y_perf_gbm, add = T, col = 'cyan')
legend('bottomright', inset=.1,
       legend=c("rpart", "glm", "RF", "gbm"),
       col = c("black", "red", "blue", "cyan"), lty = 1, lwd = 2)

# we choose lasso model...

# 'gbm' is winner...
m_gbm
predicted_prb <- predict(m_gbm, test_data, type = 'prob')[ , '1'] 
predicted_cls <- ifelse(predicted_prb > .5, 1, 0)
actual_cls <- test_data$class

table(predicted_cls, actual_cls)


# Lasso -------------------------------------------------------------------

# xx <- model.matrix(class ~ .-1, data_cancer)
# training

x <- model.matrix(class ~ .-1, train_data) # -1의 의미 : 첫 번째 컬럼인 class 변수를 제거한다.

x %>% head
# x <- xx[training_idx, ]

y <- as.numeric(as.character(train_data$class))
# glimpse(x)

data_cvfit <- cv.glmnet(x, y, family = "binomial") # glm()에서는 family = binomial로 한 반면, cv.glm()에서는 family = "binomial"로 한 것에 유의
plot(data_cvfit)

# lambda.1se :: 해석 가능한 모형을 위한 변수선택
# lambda.min :: 가장 정확한 예측값을 낳는 변수선택
coef(data_cvfit, s = c("lambda.1se"))
coef(data_cvfit, s = c("lambda.min"))

# Evaluation --------------------------------------------------------------

predict.cv.glmnet(data_cvfit, s = "lambda.min", newx = x[1:5, ], type = "response")

newx = model.matrix(class ~.-1, test_data)

yhat_glmnet <- predict(data_cvfit, s = "lambda.min", newx = newx, 
                       type = 'response')


yhat_glmnet <- yhat_glmnet %>% as_tibble() %>% pull() # change to a vector from [n*1] matrix

y_obs <- test_data$class %>% as.character() %>% as.numeric()
pred_glmnet <- prediction(yhat_glmnet, y_obs)
perf_glmnet <- performance(pred_glmnet, measure = 'tpr', x.measure = 'fpr')

plot(perf_glmnet)

performance(pred_glmnet, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_glmnet)


