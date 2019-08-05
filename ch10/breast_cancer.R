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



data <- tbl_df(read.table("./data/wdbc.data", strip.white = T, 
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
  password = "******",
  dbname = "breast_cancer",
)

con

# ?RMySQL
dbListTables(con)
dbWriteTable(con, "breast_cancer", data)

head(data)
dbSendQuery(con, "CREATE TABLE breast (id INT);")
dbListTables(con)

dbWriteTable(con, "breast", data, overwrite = T)


dbGetQuery(con, "select * from breast;")
dbGetQuery(con, "select * from breast_cancer;")
dbRemoveTable(con, "breast_cancer")


dbListTables(con)
dbDisconnect(con)
rm(con)

# create database ---------------------------------------------------------

dbSendQuery(con, "CREATE DATABASE bookstore;")
dbSendQuery(con, "USE bookstore;")
mydb = dbConnect(MySQL(), user='root', password='chr0n3!7!', host='localhost', dbname="bookstore")

dbSendQuery(mydb, "drop table if exists books, authors")

# creating tables in bookstore:

dbSendQuery(mydb, "CREATE TABLE books (book_id INT, 
            title VARCHAR(50),
            author VARCHAR(50));")

dbListTables(mydb);

dbSendQuery(mydb,
            "ALTER TABLE books
            CHANGE COLUMN book_id book_id INT AUTO_INCREMENT PRIMARY KEY,
            CHANGE COLUMN author author_id INT,
            ADD COLUMN description TEXT,
            ADD COLUMN genre ENUM('novel', 'poetry', 'drama', 'tutorials', 'text', 'other'),
            ADD COLUMN publisher_id INT,
            ADD COLUMN pub_year VARCHAR(4),
            ADD COLUMN isbn VARCHAR(20);")

# to set up the authors table ---------------------------------------------
dbSendQuery(mydb, 
            "CREATE TABLE authors
            (author_id INT AUTO_INCREMENT PRIMARY KEY,
            author_last VARCHAR(50),
            author_first VARCHAR(50),
            country VARCHAR(50));")


# Adding data into tables -------------------------------------------------

dbSendQuery(mydb, 
            "INSERT INTO authors 
            (author_last, author_first, country) 
            VALUES('Kim', 'Daniel', 'ROK');")


# fetching last data insert id number -------------------------------------

last_id = fetch(dbSendQuery(mydb, "SELECT LAST_INSERT_ID();"))

dbSendQuery(mydb, "INSERT INTO books (title, author_id, isbn, genre, pub_year) VALUES ('R and MySQL', 1, '6900690075', 'tutorials', '2019');")

try1 = fetch(dbSendQuery(mydb, "SELECT book_id, title, description FROM books WHERE genre = 'tutorials';"))


# Loading data ---------------------------------------------------------------

data <- dbGetQuery(con, "select * from breast;")
data %>% glimpse()
summary(data)

data <- data %>% dplyr::select(-row_names, -id)

# class 변수를 factor 변수로 변환
data$class <- factor(ifelse(data$class == "B", 0, 1))

# EDA ---------------------------------------------------------------------

data %>% as_tibble() %>% dplyr::select(class, starts_with('mean_')) %>%
  sample_n(min(1000, nrow(data))) -> sample_data_mean
pairs(sample_data_mean)


data %>% as_tibble() %>% dplyr::select(class, starts_with('se_')) %>%
  sample_n(min(1000, nrow(data))) -> sample_data_se

pairs(sample_data_se)

data %>% as_tibble() %>% dplyr::select(class, starts_with('worst_')) %>%
  sample_n(min(1000, nrow(data))) -> sample_data_worst

pairs(sample_data_worst)

data %>%
  ggplot(aes(class)) + geom_bar() -> p1

data %>%
  ggplot(aes(class, mean_concave_points)) + geom_jitter(col = 'gray') + geom_boxplot(alpha = .5) -> p2

data %>%
  ggplot(aes(class, mean_radius)) + geom_jitter(col = 'gray') + geom_boxplot(alpha = .5) -> p3

data %>%
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


# Logistic Regression -----------------------------------------------------

training %>% head()
str(training)

data_lm_full <- glm(class ~., data=training, family = binomial)
summary(data_lm_full)

predict(data_lm_full, newdata = data[1:5, ], type = 'response')


# Evaluation --------------------------------------------------------------

y_obs <- as.numeric(as.character(validation$class))
yhat_lm <- predict(data_lm_full, newdata = validation, type = "response")
pred_lm <- prediction(yhat_lm, y_obs)
performance(pred_lm, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_lm)

# Lasso -------------------------------------------------------------------

xx <- model.matrix(class ~ .-1, data)

x <- xx[training_idx, ]
y <- as.numeric(as.character(training$class))
glimpse(x)

data_cvfit <- cv.glmnet(x, y, family = "binomial")
plot(data_cvfit)

# lambda.1se :: 해석 가능한 모형을 위한 변수선택
# lambda.min :: 가장 정확한 예측값을 낳는 변수선택
coef(data_cvfit, s = c("lambda.1se"))
coef(data_cvfit, s = c("lambda.min"))

# Evaluation --------------------------------------------------------------

predict.cv.glmnet(data_cvfit, s = "lambda.min", newx = x[1:5, ], type = "response")

yhat_glmnet <- predict(data_cvfit, s = "lambda.min", newx = xx[validate_idx, ], 
                       type = 'response')

yhat_glmnet <- yhat_glmnet[, 1] # change to a vector from [n*1] matrix
pred_glmnet <- prediction(yhat_glmnet, y_obs)
performance(pred_glmnet, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_glmnet)


# Tree Model --------------------------------------------------------------

data_tr <- rpart(class ~., data=training)

printcp(data_tr)
summary(data_tr)

opar <- par(mfrow = c(1,1), xpd = NA)
plot(data_tr)
text(data_tr, use.n = T)
par(opar)

yhat_tr <- predict(data_tr, validation)
yhat_tr <- yhat_tr[, "1"]

pred_tr <- prediction(yhat_tr, y_obs)
performance(pred_tr, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_tr)


# Random Forest -----------------------------------------------------------

set.seed(2019)
data_rf <- randomForest(class ~., training)
data_rf

opar <- par(mfrow = c(1, 2))
plot(data_rf)
varImpPlot(data_rf)
par(opar)

yhat_rf <- predict(data_rf, newdata = validation, type = 'prob')[, '1']
pred_rf <- prediction(yhat_rf, y_obs)

performance(pred_rf, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_rf)


# Boosting ----------------------------------------------------------------

par(mfrow = c(1,1))
set.seed(2019)
data_for_gbm <- training %>% mutate(class = as.numeric(as.character(class)))

data_gbm <- gbm(class ~. , data = data_for_gbm, distribution = "bernoulli",
                n.trees = 50000, cv.folds = 3, verbose = T)

# (best_iter = gbm.perf(data_gbm, method = "cv"))

yhat_gbm <- predict(data_gbm, n.trees = 12735, newdata = validation, type = 'response')
pred_gbm <- prediction(yhat_gbm, y_obs)

performance(pred_gbm, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_gbm)

data.frame(method = c('lm', 'glmnet', 'rf', 'gbm'),
           auc = c(performance(pred_lm, "auc")@y.values[[1]],
                   performance(pred_glmnet, "auc")@y.values[[1]],
                   performance(pred_rf, "auc")@y.values[[1]],
                   performance(pred_gbm, "auc")@y.values[[1]]),
           bin_dev = c(binomial_deviance(y_obs, yhat_lm),
                       binomial_deviance(y_obs, yhat_glmnet),
                       binomial_deviance(y_obs, yhat_rf),
                       binomial_deviance(y_obs, yhat_gbm)))

perf_lm <- performance(pred_lm, measure = 'tpr', x.measure = 'fpr')
perf_glmnet <- performance(pred_glmnet, measure = 'tpr', x.measure = 'fpr')
perf_rf <- performance(pred_rf, measure = 'tpr', x.measure = 'fpr')
perf_gbm <- performance(pred_gbm, measure = 'tpr', x.measure = 'fpr')

plot(perf_lm, col = 'black', main = 'ROC Curve')
plot(perf_glmnet, col = 'blue', add = T)
plot(perf_rf, col = 'red', add = T)
plot(perf_gbm, col = 'cyan', add = T)
abline(0, 1)
legend('bottomright', inset=.1,
       legend=c("GLM", "glmnet", "RF", "GBM"),
       col = c("black", "blue", "red", "cyan"), lty = 1, lwd = 2)

# we choose lasso model...
# apply test dataset to lasso model...

y_obs_test <- as.numeric(as.character(test$class))
yhat_glmnet_test <- predict(data_cvfit, s = "lambda.min", newx = xx[test_idx, ], type = 'response')

pred_glmnet_test <- prediction(yhat_glmnet_test, y_obs_test)
performance(pred_glmnet_test, "auc")@y.values[[1]]
binomial_deviance(y_obs, yhat_glmnet_test)


pairs(data.frame(y_obs = y_obs, 
                 yhat_lm = yhat_lm,
                 yhat_glmnet = c(yhat_glmnet),
                 yhat_rf = yhat_rf,
                 yhat_gbm = yhat_gbm))













