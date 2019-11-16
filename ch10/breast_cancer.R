Packages <- c("tidyverse", "MASS", "glmnet", "randomForest", "gbm", "rpart",
              "boot", "data.table", "ROCR", "gridExtra")
lapply(Packages, library, character.only = T)
setwd("C:/Users/Daniel/Documents/MySQL_R")
getwd()


binomial_deviance <- function(y_obs, yhat) {
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1- epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs == 0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return((2*sum(a+b)))
}


setwd("~/ipds_kr_R_book/ch10")
data <- tbl_df(read.table("wdbc.data", strip.white = T, 
                          sep = ",", header = F)) 
feature_names <- c('radius', 'texture', 'perimeter', 'area', 'smoothness', 
                  'compactness', 'concavity', 'concave_points', 'symmetry', 
                  'factal_dim')

names(data) <- c(
  'id', 'class',
  paste0('mean_', feature_names),
  paste0('se_', feature_names), 
  paste0('worst_', feature_names)
)

data

library(RMySQL)

con <- dbConnect(
  MySQL(),
  user = "root", 
  password = "****",
  dbname = "breast_cancer",
)

con

# ?RMySQL
dbListTables(con)

# https://stackoverflow.com/questions/50745431/trying-to-use-r-with-mysql-the-used-command-is-not-allowed-with-this-mysql-vers?rq=1
# 
# The following steps should fix the dbWritetable() error in R:
#   
# 1. Login MySQL terminal by typing "MySQL -u user -p*" (followed by user password if you set one).
# 
# 2. Type "SET GLOBAL local_infile = true;" in the MySQL terminal command.
# 
# 3. Lastly, type "SHOW GLOBAL VARIABLES LIKE 'local_infile';" into the terminal and check the command line output for the ON status.
# 
# I'm not sure why the database function fails from MySQL 5.6 to 8.0, however, "local_infile" enables user access to data loads from local sources--- this solution should work for all database interference stacks (R, Python, etc)!
# dbWriteTable(con, "breast_cancer_data", data, overwrite = T)

dbWriteTable(con, "breast_cancer", data)

# head(data)
# dbSendQuery(con, "CREATE TABLE breast (id INT);")
# dbListTables(con)

# dbWriteTable(con, "breast", data, overwrite = T)


# dbGetQuery(con, "select * from breast;")
dbGetQuery(con, "select * from breast_cancer;")
# dbRemoveTable(con, "breast_cancer")

# 
# dbListTables(con)
# dbDisconnect(con)
# rm(con)
# 
# breast_cancer <- dbGetQuery(con, "select * from breast_cancer;")
# data <- breast_cancer %>% as_tibble()
# 
# # create database ---------------------------------------------------------
# 
# # dbSendQuery(con, "CREATE DATABASE bookstore;")
# # dbSendQuery(con, "USE bookstore;")
# # mydb = dbConnect(MySQL(), user='root', password='chr0n3!7!', host='localhost', dbname="bookstore")
# # 
# # dbSendQuery(mydb, "drop table if exists books, authors")
# 
# # creating tables in bookstore:
# 
# dbSendQuery(mydb, "CREATE TABLE books (book_id INT, 
#             title VARCHAR(50),
#             author VARCHAR(50));")
# 
# dbListTables(mydb);
# 
# dbSendQuery(mydb,
#             "ALTER TABLE books
#             CHANGE COLUMN book_id book_id INT AUTO_INCREMENT PRIMARY KEY,
#             CHANGE COLUMN author author_id INT,
#             ADD COLUMN description TEXT,
#             ADD COLUMN genre ENUM('novel', 'poetry', 'drama', 'tutorials', 'text', 'other'),
#             ADD COLUMN publisher_id INT,
#             ADD COLUMN pub_year VARCHAR(4),
#             ADD COLUMN isbn VARCHAR(20);")
# 
# # to set up the authors table ---------------------------------------------
# dbSendQuery(mydb, 
#             "CREATE TABLE authors
#             (author_id INT AUTO_INCREMENT PRIMARY KEY,
#             author_last VARCHAR(50),
#             author_first VARCHAR(50),
#             country VARCHAR(50));")
# 
# 
# # Adding data into tables -------------------------------------------------
# 
# dbSendQuery(mydb, 
#             "INSERT INTO authors 
#             (author_last, author_first, country) 
#             VALUES('Kim', 'Daniel', 'ROK');")
# 
# 
# # fetching last data insert id number -------------------------------------
# 
# last_id = fetch(dbSendQuery(mydb, "SELECT LAST_INSERT_ID();"))
# 
# dbSendQuery(mydb, "INSERT INTO books (title, author_id, isbn, genre, pub_year) VALUES ('R and MySQL', 1, '6900690075', 'tutorials', '2019');")
# 
# try1 = fetch(dbSendQuery(mydb, "SELECT book_id, title, description FROM books WHERE genre = 'tutorials';"))


# Loading data ---------------------------------------------------------------

data <- dbGetQuery(con, "select * from breast_cancer;")
data %>% glimpse()
summary(data)c

data_1 <- data %>% as_tibble() %>% dplyr::select(-row_names, -id)

# class 변수를 factor 변수로 변환
data_1$class <- as.factor(data_1$class)

# recode.....----
data_1 %>%
  mutate(class = recode(class, 'B' = 0, 'M' = 1)) %>%
  mutate(class = as.factor(class)) -> data_2 -> data_cancer

# data$class <- factor(ifelse(data$class == "B", 0, 1))

# recode and case_when.... ------------------------------------------------

df <- tribble(
  ~subject,
  "chemistry",
  "biology",
  "physics"
)

df %>% 
  mutate(subject2 = case_when(
    subject == "chemistry" ~ 1,
    subject == "biology" ~ 1,
    subject == "physics" ~ 2,
  ))

df %>% 
  mutate(subject2 = recode(
    subject, 
    "chemistry" = 1,
    "biology" = 1,
    "physics" = 2,
  ))


data_1 %>%
  # mutate(class = recode(class, 'B' = 0, 'M' = 1))  %>% 
  dplyr::select(class, mean_radius)


data_1 %>%
  mutate(class = recode(class, 'B' = 0, 'M' = 1))  %>% 
  dplyr::select(class, mean_radius)


data_1 %>%
  mutate(class = case_when(class == 'B' ~ 0, 
                           class == 'M' ~ 1))  %>% 
  dplyr::select(class, mean_radius)

##----
iris %>%
  mutate(Species2 = recode(Species,
                           'setosa' = 1,
                           'versicolor' = 2, 
                           'virginica' = 3
  ))

# 보통 하나의 컬럼에 대한 간단한 조건만을 식별하여 값을 변화시킬 땐 recode()를 이용...
iris %>%
  mutate(Species = recode(Species,
                           'setosa' = 1,
                           'versicolor' = 2, 
                           'virginica' = 3
  ))


iris %>%
  mutate(Species2 = case_when(Species == 'setosa' ~ 1,
                              Species == 'versicolor' ~ 2,
                              Species == 'virginica' ~ 3 
  ))


# 좀 더 여러 컬럼에 대한 복잡한 조건식을 적용하여 값을 변화시킬 땐 case_when()을 이용...
iris %>%
  mutate(Species2 = case_when(
    Species == 'setosa' & Sepal.Length >= 5.8 ~ 1,
    Species == 'setosa' & Sepal.Length < 5.8 ~ 2,
    Species == 'versicolor' & Sepal.Length >= 5.8 ~ 3,
    Species == 'versicolor' & Sepal.Length < 5.8 ~ 4,
    Species == 'virginica' & Sepal.Length >= 5.8 ~ 5,
    Species == 'virginica' & Sepal.Length < 5.8 ~ 6
  ))


iris$Sepal.Length %>% fivenum()
# 4.3 5.1 5.8 6.4 7.9


iris %>%
  mutate(sepal.legnth_is_greater_6.5 = 
           cut(Sepal.Length, c(4.3, 5.8, 7.9)))

iris %>%
  mutate(sepal.legnth_is_greater_6.5 = 
           cut(Sepal.Length, c(4.3, 5.8, 7.9))) %>%
  filter(Sepal.Length == 5.8)



# EDA ---------------------------------------------------------------------

data_cancer %>% as_tibble() %>% dplyr::select(class, starts_with('mean_')) %>%
  sample_n(min(1000, nrow(data))) -> sample_data_mean

pairs(sample_data_mean)

data_cancer %>% as_tibble() %>% dplyr::select(class, starts_with('se_')) %>%
  sample_n(min(1000, nrow(data))) -> sample_data_se

pairs(sample_data_se)

data_cancer %>% as_tibble() %>% dplyr::select(class, starts_with('worst_')) %>%
  sample_n(min(1000, nrow(data))) -> sample_data_worst

pairs(sample_data_worst)


# -------------------------------------------------------------------------

data_cancer %>%
  ggplot(aes(class)) + geom_bar() -> p1

data_cancer %>%
  ggplot(aes(class, mean_concave_points)) + geom_jitter(col = 'gray') + geom_boxplot(alpha = .5) -> p2

data_cancer %>%
  ggplot(aes(class, mean_radius)) + geom_jitter(col = 'gray') + geom_boxplot(alpha = .5) -> p3

data_cancer %>%
  ggplot(aes(mean_concave_points, mean_radius)) + geom_jitter(col = 'gray') + geom_smooth() -> p4

grid.arrange(p1, p2, p3, p4, ncol = 2)

# Split data -------------------------------------------------------------------------

set.seed(2019)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <-setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .2)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx, ]
validation <- data[validate_idx, ]
test <- data[test_idx, ]


# n <- nrow(data)
# train_idx <- sample(1:n, n*.8)
# test_idx <- setdiff(1:n, train_idx)
# 
# train_data <- data[train_idx, ]
# test_data <- data[test_idx, ]
# 
# train_data %>% dim()
# test_data %>% dim()
# -------------------------------------------------------------------------

data %>% as_tibble()

library(rsample)
data_split <- initial_split(data_cancer, .6)

# train_data <- training(data_split)
# test_data <- testing(data_split)
# 
# train_data %>% nrow()
# test_data %>% nrow()

training <- training(data_split)
val_testing <- testing(data_split)

training %>% nrow()
val_testing %>% nrow()

val_testing_split <- val_testing %>% initial_split(.5)
validating <- val_testing_split %>% training()

testing <- val_testing_split %>% testing()


# 60:20:20 비율로 데이터 나누기...
training %>% nrow()
validating %>% nrow()
testing %>% nrow()


# Logistic Regression -----------------------------------------------------

training %>% head()
str(training)


data_lm_full <- glm(class ~., data=training, family = binomial)
summary(data_lm_full)

predict(data_lm_full, newdata = data[1:5, ], type = 'response')


# Evaluation --------------------------------------------------------------

y_obs <- as.numeric(as.character(validating$class))
yhat_lm <- predict(data_lm_full, newdata = validating, type = "response")
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = 'tpr', x.measure = 'fpr')

performance(pred_lm, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_lm)

plot(perf_lm)



# Evaluation test ---------------------------------------------------------

y_obs <- testing$class %>% as.character %>% as.numeric
y_lm_hat <- testing %>% predict(data_lm_full, newdata = ., type = 'response')
y_pred <- ROCR::prediction(y_lm_hat, y_obs)
y_perf <- ROCR::performance(y_pred, measure = 'tpr', x.measure = 'fpr')
plot(y_perf, col = 'blue')
abline(0, 1)
legend('bottomright', inset = .1, 
       legend = c('Logistic Regression'), 
       col = 'blue', lty = 1)
ROCR::performance(y_pred, "auc")@y.values[[1]]

binomial_deviance

# Lasso -------------------------------------------------------------------

# xx <- model.matrix(class ~ .-1, data_cancer)
# training

x <- model.matrix(class ~ .-1, training) # -1의 의미 : 첫 번째 컬럼인 class 변수를 제거한다.

# x %>% head %>% View()
# x <- xx[training_idx, ]

y <- as.numeric(as.character(training$class))
# glimpse(x)

data_cvfit <- cv.glmnet(x, y, family = "binomial") # glm()에서는 family = binomial로 한 반면, cv.glm()에서는 family = "binomial"로 한 것에 유의
plot(data_cvfit)

# lambda.1se :: 해석 가능한 모형을 위한 변수선택
# lambda.min :: 가장 정확한 예측값을 낳는 변수선택
coef(data_cvfit, s = c("lambda.1se"))
coef(data_cvfit, s = c("lambda.min"))

# Evaluation --------------------------------------------------------------

predict.cv.glmnet(data_cvfit, s = "lambda.min", newx = x[1:5, ], type = "response")

newx = model.matrix(class ~.-1, validating)

yhat_glmnet <- predict(data_cvfit, s = "lambda.min", newx = newx, 
                       type = 'response')

yhat_glmnet <- yhat_glmnet %>% as_tibble() %>% pull() # change to a vector from [n*1] matrix
pred_glmnet <- prediction(yhat_glmnet, y_obs)
perf_glmnet <- performance(pred_glmnet, measure = 'tpr', x.measure = 'fpr')

plot(perf_glmnet)
plot(perf_lm, col = 'red', add = T)

performance(pred_glmnet, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_glmnet)

# Tree Model --------------------------------------------------------------

data_tr <- rpart(class ~., data=training)

printcp(data_tr)
plotcp(data_tr)
summary(data_tr)

opar <- par(mfrow = c(1,1), xpd = NA)
plot(data_tr)
text(data_tr, use.n = T)
par(opar)

training %>% count(class)
training %>% ggplot(aes(class)) + geom_bar() 

# worst_concave_points >= 0.1416 (오른쪽 가지)이면 악성이다.
# worst_concave_points < 0.1416 이고 worst_area >= 957.5이면 악성이다.
# worst_concave_points < 0.1414 이고 worst_area < 957.5 이면 양성이다.


yhat_tr <- predict(data_tr, validating)
yhat_tr <- yhat_tr[, "1"]

pred_tr <- prediction(yhat_tr, y_obs)
perf_tr <- performance(pred_tr, measure = 'tpr', x.measure = 'fpr')

plot(perf_glmnet)
plot(perf_lm, col = 'red', add = T)
plot(perf_tr, col = 'blue', add = T)

performance(pred_tr, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_tr)

predcited_class <- predict(data_tr, newdata = testing[, -1], type = 'class')
actual_class <- testing$class %>% as.character %>% as.numeric

table(predcited_class, actual_class)

caret::confusionMatrix(predcited_class %>% as.factor, actual_class %>% as.factor)

# Random Forest -----------------------------------------------------------

set.seed(2019)
data_rf <- randomForest(class ~., training)
data_rf

opar <- par(mfrow = c(1, 2))
plot(data_rf)
varImpPlot(data_rf)
par(opar)

yhat_rf <- predict(data_rf, newdata = validating, type = 'prob')[, '1']
pred_rf <- prediction(yhat_rf, y_obs)
perf_rf <- performance(pred_rf, measure = 'tpr', x.measure = 'fpr')

plot(perf_glmnet)
plot(perf_lm, col = 'red', add = T)
plot(perf_tr, col = 'blue', add = T)
plot(perf_rf, col = 'green', add = T)

performance(pred_rf, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_rf)

# Boosting ----------------------------------------------------------------
# 
# par(mfrow = c(1,1))
# set.seed(2019)
# data_for_gbm <- training %>% mutate(class = as.numeric(as.character(class)))
# 
# # consumming time----
# data_gbm <- gbm(class ~. , data = data_for_gbm, distribution = "bernoulli",
#                 n.trees = 50000, cv.folds = 3, verbose = T)
# 
# # (best_iter = gbm.perf(data_gbm, method = "cv"))
# 
# yhat_gbm <- predict(data_gbm, n.trees = 12735, newdata = validating, type = 'response')
# 
# pred_gbm <- prediction(yhat_gbm, y_obs) 
# perf_gbm <- performance(pred_gbm, measure = 'tpr', x.measure = 'fpr')
# 
plot(perf_glmnet)
plot(perf_lm, col = 'red', add = T)
plot(perf_tr, col = 'blue', add = T)
plot(perf_rf, col = 'green', add = T)
# plot(perf_gbm, col = 'cyan', add = T)

# performance(pred_gbm, "auc")@y.values[[1]]
# binomial_deviance(y_obs, yhat_gbm)

data.frame(method = c('lm', 'glmnet', 'rf'),
           auc = c(performance(pred_lm, "auc")@y.values[[1]],
                   performance(pred_glmnet, "auc")@y.values[[1]],
                   performance(pred_rf, "auc")@y.values[[1]]),
                   # performance(pred_gbm, "auc")@y.values[[1]]),
           bin_dev = c(binomial_deviance(y_obs, yhat_lm),
                       binomial_deviance(y_obs, yhat_glmnet),
                       binomial_deviance(y_obs, yhat_rf)))
                       # binomial_deviance(y_obs, yhat_gbm)))

perf_lm <- performance(pred_lm, measure = 'tpr', x.measure = 'fpr')
perf_glmnet <- performance(pred_glmnet, measure = 'tpr', x.measure = 'fpr')
perf_rf <- performance(pred_rf, measure = 'tpr', x.measure = 'fpr')
# perf_gbm <- performance(pred_gbm, measure = 'tpr', x.measure = 'fpr')

plot(perf_lm, col = 'black', main = 'ROC Curve')
plot(perf_glmnet, col = 'blue', add = T)
plot(perf_rf, col = 'red', add = T)
# plot(perf_gbm, col = 'cyan', add = T)
abline(0, 1)
legend('bottomright', inset=.1,
       legend=c("GLM", "glmnet", "RF"),
       col = c("black", "blue", "red"), lty = 1, lwd = 2)

# we choose lasso model...
# apply test dataset to lasso model...

y_obs_test <- as.numeric(as.character(testing$class))

newx = model.matrix(class ~ .-1, testing)

yhat_glmnet_test <- predict(data_cvfit, s = "lambda.min", newx = newx, type = 'response')

pred_glmnet_test <- prediction(yhat_glmnet_test, y_obs_test)

perf_glmnet_test <- performance(pred_glmnet_test, measure = 'tpr', x.measure  = 'fpr')

plot(perf_glmnet_test)

performance(pred_glmnet_test, "auc")@y.values[[1]]

binomial_deviance(y_obs_test, yhat_glmnet_test)

pairs(data.frame(y_obs = y_obs, 
                 yhat_lm = yhat_lm,
                 yhat_glmnet = c(yhat_glmnet),
                 yhat_rf = yhat_rf))
                 # yhat_gbm = yhat_gbm))







