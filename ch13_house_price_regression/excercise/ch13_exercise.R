library(tidyverse)
# library(data.table)

# 함수정의 --------------------------------------------------------------------

rmse <- function(yhat, y_obs) {
  (y_obs - yhat)^2 %>%
    mean() %>%
    sqrt()
  }


yhat <- c(1, 2, 3)
y_obs <- c(1.3, 2.1, 3.5)

rmse(yhat, y_obs)

# -------------------------------------------------------------------------

train_data <- read_csv('train.csv')
train_data 

train_data %>% dim()
train_data %>% str()

# NA...
na_c <- c()
for (i in 1:ncol(train_data)) {
  n_na_ith <- train_data[, i] %>% filter(is.na(.)) %>% nrow()
  na_c <- c(na_c, n_na_ith)
}

names(na_c) <- colnames(train_data)
na_c

# 각 열에 포함된 na의 갯수는 몇 개인가??
vec.n_na <- train_data %>% map(~sum(is.na(.x))) %>% unlist()
vec.n_na
vec.n_na %>% sort() %>% rev() %>% enframe() -> df

df %>%
  filter(value != 0) -> df_1

df_1$name <- factor(df_1$name, levels = rev(df_1$name))

df_1 %>% str()

df_1 %>% ggplot(aes(name, value)) + geom_bar(stat = 'identity') + coord_flip()
col_has_na <- df_1$name %>% as.character()

# 19개의 컬럼이 NA값들을 가지고 있다...
# 그 중 numeric 변수를 가지고 있는 컬럼에 있는  NA 값을 해당 컬럼의 median 값으로 대체한다.(imputation)

train_data[, col_has_na] %>%
  select_if(is.numeric) %>%
  colnames() -> num_has_na_col_nm

train_data %>%
  mutate_if(is.numeric, list(~ifelse(is.na(.), median(., na.rm = T), .))) -> train_data_1


train_data[, num_has_na_col_nm]
train_data_1[, num_has_na_col_nm]


# NA 값을 가지고 있는 컬럼 중 character 속성의 컬럼의 NA 값을 "NA"로 대체한다

train_data_1 %>%
  mutate_if(is.character, list(~ifelse(is.na(.), "NA", .))) -> train_data_2


train_data[, col_has_na] %>% select_if(is.character)
train_data_2[, col_has_na] %>% select_if(is.character)


train_data_2 %>% summary()
train_data_2 %>% map_int(~sum(is.na(.x)))

df <- train_data_2
cor_mat <- df %>% select_if(is.numeric) %>% cor()
# cor_mat > .5
# cor_mat[cor_mat > .5] # 숫자 벡터 반환....

cor_mat %>%
  as_tibble() %>%
  mutate(var_nm = colnames(.)) %>%
  dplyr::select(var_nm, dplyr::everything()) %>%
  gather(variable, value, -var_nm) %>% 
  # filter(value >= .5) %>%
  filter(var_nm != variable) -> high_cor_mat_df

cor_mat %>% row.names()

high_cor_mat_df %>%
  arrange(desc(value)) %>%
  filter(grepl("sale", var_nm)) -> cor_df_with_saleprice

cor_df_with_saleprice$variable <- factor(cor_df_with_saleprice$variable, 
                                         levels = rev(cor_df_with_saleprice$variable))

cor_df_with_saleprice %>% str()

cor_df_with_saleprice %>%
  ggplot(aes(variable, value)) + geom_bar(stat = 'identity') +
  coord_flip()

cor_df_with_saleprice %>%
  mutate(variable = as.character(variable)) %>%
  filter(value >= .3) %>% pull(variable) -> vec_1

nm <- c('saleprice', vec_1)

cor_mat_1 <- cor_mat[nm, nm]

library(corrgram)
library(corrplot)
# corrgram::corrgram(cor_mat_1, order = T, upper.panel = panel.cor) 
corrplot::corrplot(cor_mat_1, cl.offset = .9)


# test dataset... ---------------------------------------------------------

test_data <- read_csv('test.csv')
test_data %>% colnames()

train_col_nm <- train_data_2 %>% colnames() %>% tolower()
test_col_nm <- test_data %>% colnames() %>% tolower()

setdiff(train_col_nm, test_col_nm)
setdiff(test_col_nm, train_col_nm)

colnames(train_data_2) <- train_col_nm
colnames(test_data) <- test_col_nm

test_data %>%
  mutate(saleprice = 0) -> test_data_0

test_data_0 %>% mutate(cate = 'test') -> test_data_1

train_data_2 %>% mutate(cate = 'train') -> train_data_3

train_data_3 %>%
  bind_rows(test_data_1) -> total_data


total_data %>%
  mutate_if(is.numeric, list(~ifelse(is.na(.), median(., na.rm = T), .))) %>%
  mutate_if(is.character, list(~ifelse(is.na(.), "NA", .))) -> total_data_1

model.matrix(~., total_data_1) %>% as_tibble() -> total_data_2

total_data_2 %>% filter(catetrain == 1) %>%
  dplyr::select(-catetrain) -> train_data_4

total_data_2 %>% filter(catetrain == 0) %>%
  dplyr::select(-catetrain) -> test_data_4


train_data_4 %>% dim()
test_data_4 %>% dim()


# Data Split... -----------------------------------------------------------

library(caret)

train_idx <- createDataPartition(train_data_4$saleprice, p = .8, list = F)[, 1]
val_idx <- setdiff(1:nrow(train_data_4), train_idx)

train_idx %>% length()
val_idx %>% length()

train_set <- train_data_4[train_idx, ]
train_set %>% dim()

val_set <- train_data_4[val_idx, ]
val_set %>% dim()

train_set %>% str()
val_set %>% str()


# Modeling_with_lm --------------------------------------------------------

train_set %>% glimpse()
val_set %>% glimpse()

train_set %>% str()
val_set %>% str()

## modeling...
m_lm <- lm(saleprice ~ ., data = train_set)

summary(m_lm)

m_lm %>% coef() %>% length()

## prediction....
predict(m_lm, val_set)


# Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
# 요인(factor) utilities는 NA개의 새로운 수준(levels)들을 가지고 있습니다.

# 변수선택... stepwise...
library(MASS)
m_step <- stepAIC(m_lm, scope = list(upper = ~ ., lower = ~1))

# time consuming....

m_step %>% coef() %>% length()
m_lm %>% coef() %>% length()

# 261개의 모수에서 140개 모수로 감소...


# prediction with val_set -------------------------------------------------

val_set

m_lm

m_step

yhat_lm <- predict(m_lm, newdata = val_set)
y_obs <- val_set$saleprice

rmse(yhat_lm, y_obs)


yhat_step <- predict(m_step, newdata = val_set)
rmse(yhat_step, y_obs)


summary(m_lm)
summary(m_step)


# train set...-------------------------------------------------------------------------

test_data_4

predict(m_step, test_data_4) %>%
  enframe()

val_set %>%
  dplyr::select(saleprice) %>%
  mutate(yhat_val = predict(m_step, val_set))




# randomForest ------------------------------------------------------------

train_set
val_set
test_set <- test_data_4
test_set

library(randomForest)
train_col_nm <- make.names(colnames(train_set))
test_col_nm <- make.names(colnames(test_set))



m_rf <- randomForest(saleprice ~ ., data = train_set)


