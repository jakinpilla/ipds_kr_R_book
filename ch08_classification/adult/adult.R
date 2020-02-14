setwd('C:/Users/Daniel/Documents/ipds_kr_R_book/ch08_classification/adult')

Packages <- c('tidyverse', 'data.table', 'reshape2', 'caret', 'rpart', 'GGally',
              'ROCR', 'ISLR','glmnet', 'gbm', 'boot', 'randomForest', 'dummies', 
              'curl', 'gridExtra')
lapply(Packages, library, character.only=T)

adult <- read.csv("adult.data", header = F, strip.white = T)

names(adult) <- c("age", "workclass", "fnlwgt", "education", "education_num",
                  "marital_status", "occupation", "relationship", "race", "sex",
                  "capital_gain", "capital_loss", "hours_per_week", "native_country",
                  "wage")
adult %>% head()

glimpse(adult)
summary(adult)
levels(adult$wage)

# fct형 변수
# workclass, marital_status, occupation, relationship, race, sex, native_country, wage...

x <- model.matrix(~ race + sex + age, adult)
# glimpse(x)
class(x)

x <- x %>% as.data.frame() %>% as_tibble()
x %>% glimpse()
colnames(x)

model.matrix(~ . -wage, adult ) %>% as.data.frame() %>% as_tibble() -> x
dim(x)

# splitting data :: train:validation:test = 60:20:20

# why validation?? to find a proper model of trained models...

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

training %>% ggplot(aes(age, fill = wage)) + geom_density(alpha=.5)

training %>%
  filter(race %in% c('Black', 'White')) %>%
  ggplot(aes(age, fill = wage)) +
  geom_density(alpha = .5) +
  ylim(0, .1) +
  facet_grid(race ~ sex, scales = 'free_y')

training %>%
  ggplot(aes(education_num, fill = wage)) +
  geom_bar()

# at least 5 times tials of data visualization needed...
training$wage # Levels : <= 50K, >50K : In glm, it is considered that fisrt level as 0, other levels as 1
ad_glm_full <- glm(wage ~ ., data = training, family = binomial)
summary(ad_glm_full)

alias(ad_glm_full)

predict(ad_glm_full, newdata = adult[1:5, ], type = "response")

y_obs <- ifelse(validation$wage == ">50K", 1, 0)
yhat_lm <- predict(ad_glm_full, newdata = validation, type = 'response')

result_with_training <- data.frame(y_obs, yhat_lm)
result_with_training %>% str()

------------------------------------------library(gridExtra)
result_with_training %>%
  ggplot(aes(y_obs, yhat_lm, group = y_obs, fill = factor(y_obs))) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") -> p1

result_with_training %>%
  ggplot(aes(yhat_lm, fill=factor(y_obs))) +
  geom_density(alpha = .5) +
  scale_fill_brewer(palette = "Set1") -> p2

grid.arrange(p1, p2, ncol = 2)

# binomial deviance : more less good model...

binomial_deviance <- function(y_obs, yhat) {
  epsilon = 0.0001
  yhat = ifelse(yhat < epsilon, epsilon, yhat)
  yhat = ifelse(yhat > 1- epsilon, 1-epsilon, yhat)
  a = ifelse(y_obs == 0, 0, y_obs * log(y_obs/yhat))
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat)))
  return((2*sum(a+b)))
}

binomial_deviance(y_obs, yhat_lm)

library(ROCR)
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = 'tpr', x.measure = 'fpr')
plot(perf_lm, col = 'black', main = 'ROC Curve for GLM')
abline(0, 1)
performance(pred_lm, "auc")@y.values[[1]]





