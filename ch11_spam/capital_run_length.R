string <- 'I am BATMAN who defeats enemies! and You are THE WONDER WOMAN who are the most beautiful and strongest in the world.'
white_split_to_vec <- function(x) {
  str_split(x, "") %>% unlist()
} 


library(stringr)
library(purrr)


str_split(string, " ")[[1]] %>% as.list() %>% 
  map(~white_split_to_vec(.x)) -> ls_chr

ls_chr[[1]]

grepl(ls_chr[[1]], LETTERS) %>% sum()
grepl(ls_chr[[4]], LETTERS) %>% sum() # warning occur: only the first element will be used...

ls_chr[[4]] %in% LETTERS %>% sum()


# -------------------------------------------------------------------------


count_capital <- function(x) {
  x %in% LETTERS %>% sum()
}

count_capital(ls_chr[[4]])

ls_chr %>%
  map(~count_capital(.x)) %>% unlist()


# -------------------------------------------------------------------------

count_capital <- function(x) {
  x %in% LETTERS %>% sum()
}

str_split(string, " ")[[1]] %>% as.list() %>% 
  map(~white_split_to_vec(.x)) %>%
  map(~count_capital(.x)) %>% 
  unlist()

count_capital_per_words <- function(x) {
  str_split(string, " ")[[1]] %>% 
    as.list() %>% 
    map(~white_split_to_vec(.x)) %>%
    map(~count_capital(.x)) %>% 
    unlist()
}


count_capital_per_words(string)

`%not_in%` <- purrr::negate(`%in%`)

library(tidyverse)
library(purrr)

# capital_run_length_average <- count_capital_per_words(string) %>% 
#   enframe() %>% 
#   filter(value != 0) %>% 
#   summarise(mean.value = mean(value)) %>% pull() 

capital_run_length_average <- function(x) {
  count_capital_per_words(x) %>% 
    enframe() %>% 
    filter(value != 0) %>% 
    summarise(mean.value = mean(value)) %>% pull() 
}

capital_run_length_average(string)


capital_run_length_longest <- count_capital_per_words(string) %>% 
  enframe() %>% 
  filter(value == max(value)) %>%
  select(value) %>% 
  slice(1) %>% # 최대값이 같은 경우가 있으므로...
  pull()


capital_run_length_longest <- function(x) {
  count_capital_per_words(x) %>% 
    enframe() %>% 
    filter(value == max(value)) %>%
    select(value) %>% 
    slice(1) %>% # 최대값이 같은 경우가 있으므로...
    pull()
}

capital_run_length_longest(string)

# capital_run_length_total <-

?lag()

df_tmp <- count_capital_per_words(string) %>% enframe() 

df_tmp_1 <- df_tmp %>%
  filter(value != 0) %>%
  mutate(diff_name = name - lag(name))

idx <- df_tmp_1 %>% filter(diff_name == 1) %>% select(name) %>% pull()
idx_1 <- idx - 1
total_idx <- union(idx, idx_1) %>% sort()
total_idx

df_tmp[total_idx, ] %>% 
  summarise(total_length = sum(value)) %>% pull() %>% max()


capital_run_length_total <- function(x) {
  
  df_tmp <- count_capital_per_words(x) %>% enframe() 
  
  df_tmp_1 <- df_tmp %>%
    filter(value != 0) %>%
    mutate(diff_name = name - lag(name))
  
  idx <- df_tmp_1 %>% filter(diff_name == 1) %>% select(name) %>% pull()
  idx_1 <- idx - 1
  total_idx <- union(idx, idx_1)
  
  df_tmp[total_idx, ] %>% 
    summarise(total_length = sum(value)) %>% pull() %>% max()
}


capital_run_length_total(string)
