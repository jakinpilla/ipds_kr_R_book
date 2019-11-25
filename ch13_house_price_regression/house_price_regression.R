setwd("~/ipds_kr_R_book/ch13_house_price_regression")
read.table("housing.data", strip.white=T) %>% as_tibble() -> data_house
names(data_house) <- c('crim', 'zn', 'indus', 'chas', 'nox', 'rm', 'age', 'dis', 
                 'rad', 'tax', 'ptratio', 'b', 'lstat', 'medv')
glimpse(data_house)
summary(data_house)


# -------------------------------------------------------------------------

con <- dbConnect(
  MySQL(),
  user = "root", 
  password = "******"
)

dbSendQuery(con, "CREATE DATABASE house_price;")
dbSendQuery(con, "USE house_price;")

dbWriteTable(con, "data_house", data, overwrite = T)
dbDisconnect(con)

con_house_price <- dbConnect(
  MySQL(),
  user = "root", 
  password = "chr0n3!7!",
  dbname = "house_price"
)

data_from_db <- dbGetQuery(con_house_price, "select * from data_house;")
data_from_db %>% colnames()

data_from_db %>% select(-row_names) -> data_house
data_house %>% glimpse()


# -------------------------------------------------------------------------

data_house %>% ggpairs() 

set.seed(2019)
nrow(data_house) -> n
1:n %>% sample(., n*.6) -> train_idx
setdiff(1:n, train_idx) -> idx_without_train
idx_without_train %>% sample(., n*.2) -> validate_idx
setdiff(idx_without_train, validation_idx) -> test_idx

train_idx %>% length()
validate_idx %>% length()
test_idx %>% length()


train_data <- data_house[train_idx, ]
validate_data <- data_house[validate_idx, ]
test_data <- data_house[test_idx, ]


# Data split with caret... ------------------------------------------------

library(caret)

data_house %>% nrow()
data_house

train_idx <- createDataPartition(data_house$medv, p = .8, list = F)[, 1]
test_idx <- setdiff(1:nrow(data_house), train_idx)

train_set <- data_house[train_idx, ]

re_train_idx <- createDataPartition(train_set$medv, p = .9, list = F)[, 1]
val_idx <- setdiff(1:nrow(train_set), re_train_idx)

train_set <- data_house[train_idx, ]
re_train_set <- train_set[re_train_idx, ]
val_set <- train_set[val_idx, ]
test_set <- data_house[test_idx, ]

train_set %>% nrow()

re_train_set %>% nrow()
val_set %>% nrow()

test_set %>% nrow()


# lm modeling-------------------------------------------------------------------------

data_lm_full <- lm(medv ~., data=train_set)
summary(data_lm_full)

predict(data_lm_full, newdata=data_house[1:5, ])

plot(data_lm_full)

data_lm_full_2 <- lm(medv ~.^2, data=train_set)
summary(data_lm_full_2)

length(coef(data_lm_full_2))

library(MASS)
data_step <- step(data_lm_full, scope=list(upper = ~.^2, lower= ~1))

data_step <- stepAIC(data_lm_full, scope=list(upper = ~.^2, lower= ~1))

data_step
anova(data_step)
summary(data_step)
length(coef(data_step))


# Validation --------------------------------------------------------------

y_obs <- val_set$medv
yhat_lm <- predict(data_lm_full, newdata=val_set)
yhat_lm_2 <- predict(data_lm_full_2, newdata=val_set)
yhat_step <- predict(data_step, newdata=val_set)

library(modelr)
rmse(data_lm_full, val_set)
rmse(data_lm_full_2, val_set)
rmse(data_step, val_set)


# Lasso -------------------------------------------------------------------

xx <- model.matrix(medv ~. ^2-1, data)
x <- xx[train_idx, ]
y <- train_data$medv

glimpse(x)

data_cvfit <- cv.glmnet(x, y)
plot(data_cvfit)

coef(data_cvfit, s = c("lambda.1se"))
coef(data_cvfit, s = c("lambda.min"))


# Elastic -----------------------------------------------------------------

data_cvfit_es <- cv.glmnet(x, y, alpha=.5)
plot(data_cvfit_es)

coef(data_cvfit_es, s = c("lambda.1se"))
coef(data_cvfit_es, s = c("lambda.min"))

predict.cv.glmnet(data_cvfit, s = "lambda.min", newx = x[1:5, ])

y_obs <- validate_data$medv
yhat_glmnet <- predict(data_cvfit, s = "lambda.min", newx=xx[validate_idx, ])
yhat_glmnet <- yhat_glmnet[, 1]

data.frame(y_obs, yhat_glmnet) %>%
  as_tibble() %>%
  mutate(residue = y_obs - yhat_glmnet) %>%
  mutate(sq_residue  = residue^2) %>%
  summarise(mean.sum.sq_residue = sum(sq_residue)/nrow(.)) %>%
  mutate(rmse = sqrt(mean.sum.sq_residue)) -> df.rmse_glmnet

df.rmse_glmnet$rmse

# Tree for regression -----------------------------------------------------

data_tr <- rpart(medv ~., data=train_data)
yhat_tr <- predict(data_tr, newdata = validate_data)

rmse(data_tr, validate_data)


# RandomForest ------------------------------------------------------------

set.seed(2019)
data_rf <- randomForest(medv ~., train_data)
data_rf

plot(data_rf)
varImpPlot(data_rf)

yhat_rf <- predict(data_rf, newdata = validate_data)

rmse(data_rf, validate_data)


# Boosting ----------------------------------------------------------------

set.seed(2019)
data_gbm <- gbm(medv ~., data=train_data, 
                n.trees=40000, cv.folds = 3, 
                verbose = T)
(best_iter = gbm.perf(data_gbm, method="cv"))

yhat_gbm <- predict(data_gbm, n.trees=best_iter, newdata=validate_data)
rmse(data_gbm, validate_data)


# metirc ------------------------------------------------------------------

data.frame(
  method = c("lm", "glm", "dt", "rf", "gbm"),
  rmse = c(rmse(data_step, validate_data),
           df.rmse_glmnet$rmse,
           rmse(data_tr, validate_data),
           rmse(data_rf, validate_data), 
           rmse(data_gbm, validate_data))
  )
)


# Residue Boxplot....
y_obs
yhat_step
yhat_glmnet
yhat_tr
yhat_rf
yhat_gbm

residue_lm <- y_obs - yhat_step
residue_glmnet <- y_obs  - yhat_glmnet
residue_tr <- y_obs - yhat_tr
residue_rf <- y_obs - yhat_rf
residue_gbm <- y_obs - yhat_gbm

data.frame(residue_lm, residue_glmnet, residue_tr, residue_rf, residue_gbm) %>% 
  as_tibble() %>%
  gather() %>% 
  ggplot(aes(key, value, col=key)) + geom_boxplot(alpha=.5) + geom_jitter()





