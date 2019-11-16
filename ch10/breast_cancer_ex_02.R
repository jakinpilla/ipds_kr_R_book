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

# ----
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

data_2 %>%
  dplyr::select(-id, -time, -lymp_status) -> data_3

# data_3[, -1] %>% scale()

# normalize <- function(x) {
#   return( (x - min(x)) / (max(x) - min(x)) )
# }
# 
# x <- c(-1, 2, 3, 100)
# min(x)
# normalize(x)
# 
# data_3[, -1] %>%
#   mutate_all(list(~normalize(.))) -> data_4
# 
# data_5 <- bind_cols(data_3[, 1], data_4)
# 
# 
# # data spliting with normal-------------------------------------------------------------------------
# 
# n <- nrow(data_5)
#   
# train_idx <- sample(1:n, n*.8)
# test_idx <- setdiff(1:n, train_idx)
# 
# train_data_normal <- data_5[train_idx, ]
# test_data_normal <- data_5[test_idx, ]
# 
# train_data_normal %>% dim()
# test_data_normal %>% dim()

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

y_pred_glm <- prediction(yhat_glm, y_obs)
y_perf_glm <- performance(y_pred_glm, measure = 'tpr', x.measure ='fpr')
plot(y_perf_glm)

performance(y_pred_glm, 'auc')@y.values[[1]]

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

y_pred_rf <- prediction(yhat_rf, y_obs)
y_perf_rf <- performance(y_pred_rf, measure = 'tpr', x.measure = 'fpr')
plot(y_perf_rf)

performance(y_pred_rf, 'auc')@y.values[[1]]




# without lymp_status...-------------------------------------------------------------------------

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

y_pred_glm <- prediction(yhat_glm, y_obs)
y_perf_glm <- performance(y_pred_glm, measure = 'tpr', x.measure ='fpr')
plot(y_perf_glm)

performance(y_pred_glm, 'auc')@y.values[[1]]

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

y_pred_rf <- prediction(yhat_rf, y_obs)
y_perf_rf <- performance(y_pred_rf, measure = 'tpr', x.measure = 'fpr')
plot(y_perf_rf)

performance(y_pred_rf, 'auc')@y.values[[1]]

