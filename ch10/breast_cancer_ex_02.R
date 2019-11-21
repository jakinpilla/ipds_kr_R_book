# 연습문제 2번
# https://goo.gl/KaZD7Y
# https://goo.gl/mVFQUa


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
                        'N' = 0)) -> data_1


# EDA...
data_1$class <- as.factor(data_1$class)

data_1 %>%
  ggplot(aes(class)) + geom_bar() -> p1

data_1 %>%
  ggplot(aes(class, mean_concave_points)) + geom_jitter(col = 'gray') + geom_boxplot(alpha = .5) -> p2

data_1 %>%
  ggplot(aes(class, mean_radius)) + geom_jitter(col = 'gray') + geom_boxplot(alpha = .5) -> p3

data_1 %>%
  ggplot(aes(mean_concave_points, mean_radius)) + geom_jitter(col = 'gray') + geom_smooth() -> p4

grid.arrange(p1, p2, p3, p4, ncol = 2)

# Feature engineering----

data_1 %>%
  dplyr::select(lymp_status) %>%
  filter(lymp_status != '?') %>%
  mutate(lymp_status =as.numeric(as.character(lymp_status))) %>% pull() %>% mean() 

data_1 %>%
  mutate(lymp_status = as.character(lymp_status)) %>%
  mutate(lymp_status = case_when(lymp_status == '?' ~ '3', TRUE ~ lymp_status)) %>%
  mutate(lymp_status = as.numeric(lymp_status)) -> data_2

range(data_2$lymp_status)
data_2$lymp_status <- 72 - data_2$lymp_status

glimpse(data_2)


## Withput lymp_status ----------------------------------------------------
data_2 %>%
  dplyr::select(-id, -time, -lymp_status) -> data_3


# data spliting with not normal ---------------------------------------
n <- data_3 %>% nrow()

# train_idx <- sample(1:n, n*.8)
# test_idx <- setdiff(1:n, train_idx)

train_idx <- caret::createDataPartition(data_3$class, p = .8, list =F)[, 1]
test_idx <- setdiff(1:n, train_idx)


train_data <- data_3[train_idx, ]
test_data <- data_3[test_idx, ]

data_3$class %>% table() %>% prop.table()
train_data$class %>% table() %>% prop.table()
test_data$class %>% table() %>% prop.table()

train_data %>% nrow()
test_data %>% nrow()

# Logistic Regression... ----------------------------------------------

## training....

train_data %>% glimpse()
m_glm <- glm(class ~ ., data = train_data, family = binomial)
summary(m_glm)

# Evaluating...
y_obs <- test_data$class %>% as.character %>% as.numeric
yhat_glm <- predict(m_glm, newdata = test_data[, -1], type = 'response')

y_pred_glm_without_lymp <- prediction(yhat_glm, y_obs)
y_perf_glm_without_lymp <- performance(y_pred_glm, measure = 'tpr', x.measure ='fpr')
plot(y_perf_glm_without_lymp)

performance(y_pred_glm_without_lymp, 'auc')@y.values[[1]]

# 0.67----

# RandomForest... -----------------------------------------------------

m_rf <- randomForest(class ~ ., data = train_data)
m_rf
summary(m_rf)

par(mfrow = c(1, 2))
varImpPlot(m_rf)
plot(m_rf)
par(mfrow = c(1, 1))

y_obs <- test_data$class
yhat_rf <- predict(m_rf, newdata = test_data[, -1], type = 'prob')[, 2]

y_pred_rf_without_lymp <- prediction(yhat_rf, y_obs)
y_perf_rf_without_lymp <- performance(y_pred_rf, measure = 'tpr', x.measure = 'fpr')
plot(y_perf_rf_without_lymp)

performance(y_pred_rf_without_lymp, 'auc')@y.values[[1]]

# 0.63 ----

# With lymp_status...-------------------------------------------------------------------------

data_1 %>%
  dplyr::select(lymp_status) %>%
  filter(lymp_status != '?') %>%
  mutate(lymp_status =as.numeric(as.character(lymp_status))) %>% pull() %>% mean() 

data_1 %>%
  mutate(lymp_status = as.character(lymp_status)) %>%
  mutate(lymp_status = case_when(lymp_status == '?' ~ '3', TRUE ~ lymp_status)) %>%
  mutate(lymp_status = as.numeric(lymp_status)) -> data_2

range(data_2$lymp_status)
data_2$lymp_status <- 72 - data_2$lymp_status

glimpse(data_2)

# With lymp_status.....
data_2 %>%
  dplyr::select(-id, -time) -> data_3

# data spliting ---------------------------------------
n <- data_3 %>% nrow()

# train_idx <- sample(1:n, n*.8)
# test_idx <- setdiff(1:n, train_idx)

train_idx <- caret::createDataPartition(data_3$class, p = .8, list =F)[, 1]
test_idx <- setdiff(1:n, train_idx)


train_data <- data_3[train_idx, ]
test_data <- data_3[test_idx, ]

data_3$class %>% table() %>% prop.table()
train_data$class %>% table() %>% prop.table()
test_data$class %>% table() %>% prop.table()

train_data %>% nrow()
test_data %>% nrow()

# Logistic Regression... ----------------------------------------------

## training....

train_data %>% glimpse()
m_glm <- glm(class ~ ., data = train_data, family = binomial)
summary(m_glm)

# Evaluating...
y_obs <- test_data$class %>% as.character %>% as.numeric
yhat_glm <- predict(m_glm, newdata = test_data[, -1], type = 'response')

y_pred_glm_with_lymp <- prediction(yhat_glm, y_obs)
y_perf_glm_with_lymp <- performance(y_pred_glm_with_lymp, measure = 'tpr', x.measure ='fpr')
plot(y_perf_glm_with_lymp)

performance(y_pred_glm_with_lymp, 'auc')@y.values[[1]]

# 0.45 -----

# RandomForest... -----------------------------------------------------

m_rf <- randomForest(class ~ ., data = train_data)
m_rf
summary(m_rf)

par(mfrow = c(1, 2))
varImpPlot(m_rf)
plot(m_rf)
par(mfrow = c(1, 1))

y_obs <- test_data$class
yhat_rf <- predict(m_rf, newdata = test_data[, -1], type = 'prob')[, '1'] # '1' :: 1이 아님에 유의해야 한다.

y_pred_rf_with_lymp <- prediction(yhat_rf, y_obs)
y_perf_rf_with_lymp <- performance(y_pred_rf_with_lymp, measure = 'tpr')
plot(y_perf_rf_with_lymp)

performance(y_pred_rf_with_lymp, 'auc')@y.values[[1]]

# 0.29----

# With lymp_status and normalized data... -----------------------------------------

data_2$lymp_status %>% range()

normalize <- function(x) {
  return( (x - min(x)) / (max(x) - min(x)) )
}

data_2 %>% colnames()
data_2 %>% dplyr::select(-id) -> data_3

data_3[, -c(1, 2)] %>%
  mutate_all(list(~normalize(.))) -> data_4

data_5 <- bind_cols(data_3[, 1], data_4)


# data spliting with normal-------------------------------------------------------------------------

n <- nrow(data_5)
  
train_idx <- sample(1:n, n*.8)
test_idx <- setdiff(1:n, train_idx)

train_data_normal <- data_5[train_idx, ]
test_data_normal <- data_5[test_idx, ]

train_data_normal %>% dim()
test_data_normal %>% dim()


# Logistic Regression... ----------------------------------------------

## training....

train_data_normal %>% glimpse()
m_glm <- glm(class ~ ., data = train_data_normal, family = binomial)
summary(m_glm)

# Evaluating...
y_obs <- test_data_normal$class %>% as.character %>% as.numeric
yhat_glm <- predict(m_glm, newdata = test_data_normal[, -1], type = 'response')

y_pred_glm_with_lymp_normal <- prediction(yhat_glm, y_obs)
y_perf_glm_with_lymp_normal <- performance(y_pred_glm_with_lymp_normal, measure = 'tpr', x.measure ='fpr')
plot(y_perf_glm_with_lymp_normal)

performance(y_pred_glm_with_lymp_normal, 'auc')@y.values[[1]]

# 0.66 ----

# RandomForest... -----------------------------------------------------

m_rf <- randomForest(class ~ ., data = train_data_normal)
m_rf
summary(m_rf)

par(mfrow = c(1, 2))
varImpPlot(m_rf)
plot(m_rf)
par(mfrow = c(1, 1))

y_obs <- test_data_normal$class
yhat_rf <- predict(m_rf, newdata = test_data_normal[, -1], type = 'prob')[, '1']

y_pred_rf_with_lymp_normal <- prediction(yhat_rf, y_obs)
y_perf_rf_with_lymp_normal <- performance(y_pred_rf_with_lymp_normal, measure = 'tpr', x.measure = 'fpr')
plot(y_perf_rf_with_lymp_normal)

performance(y_pred_rf_with_lymp_normal, 'auc')@y.values[[1]]

# 0.48 -----

#

plot(y_perf_glm_without_lymp)
plot(y_perf_rf_without_lymp, add = T, col = 'red')
plot(y_perf_glm_with_lymp, add = T, col = 'blue')
plot(y_perf_rf_with_lymp, add = T, col = 'yellow')
plot(y_perf_glm_with_lymp_normal, add = T, col = 'brown')
plot(y_perf_rf_with_lymp_normal, add = T, col = 'cyan')



# caret thing... ---------------------------------------------------------


data_2$lymp_status
data_2 %>%
  dplyr::select(-id, -time) -> data_3

data_3 %>% glimpse()


train_idx <- createDataPartition(data_3$class, p = .8, list = F)[, 1]

train_data <- data_3[train_idx, ]
test_data <- data_3[-train_idx, ]

train_data %>% nrow()
test_data %>% nrow()


train_data %>% count(class)
test_data %>% count(class)



# -------------------------------------------------------------------------

fitControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 5)

m_rf <- train(class ~ ., data = train_data, method = 'rf', trControl = fitControl, verbose = T)
m_rf

# mtry: Number of trees available for splitting tree node... 

varImp(m_rf)

par(mfrow = c(1, 2))
plot(varImp(m_rf))
plot(m_rf)
par(mfrow = c(1, 1))

# 정확도와 kappa값을 고려했을때 mtry parameter 값을 2로 하였음..


yhat_rf <- predict(m_rf, newdata = test_data, type = 'prob')[, '1']
y_obs <- test_data$class

yhat_rf_class <- predict(m_rf, newdata = test_data, type = 'raw')

confusionMatrix(yhat_rf_class, y_obs)

?prediction
?performance
y_pred_rf <- prediction(yhat_rf, y_obs)
y_perf_rf <- performance(y_pred_rf, measure = 'tpr', x.measure ='fpr')

plot(y_perf_rf)
abline(0, 1)


# glm with caret -------------------------------------------------------------------------

train_data[!complete.cases(train_data), ]

train_data %>% dim()
train_data %>% na.omit() %>% dim()

fitControl <- trainControl(method = 'cv', repeats = 5)

str(train_data)
colnames(train_data) <- make.names(colnames(train_data))
train_data %>% colnames()

m_gbm <- train(class ~ ., data = train_data,
               method = 'gbm',
               trControl = fitControl, 
               verbose = T)

m_gbm

varImp(m_gbm)
plot(varImp(m_gbm))

yhat_gbm <- predict(m_gbm, newdata = test_data[, -1], type = 'prob')[, '1']
y_obs <- test_data$class
y_pred_gbm <- prediction(yhat_gbm, y_obs)
y_perf_gbm <- performance(y_pred_gbm, measure = 'tpr', x.measure = 'fpr')

plot(y_perf_gbm)
plot(y_perf_rf, add = T, col = 'red')
abline(0, 1)



