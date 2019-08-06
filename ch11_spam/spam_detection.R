library(tidyverse)
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

data %>% View()

library(RMySQL)
con <- dbConnect(
  MySQL(),
  user = "root", 
  password = "chr0n3!7!"
)

dbSendQuery(con, "CREATE DATABASE spambase;")
dbSendQuery(con, "USE spambase;")

dbWriteTable(con, "spambase", data, overwrite = T)
dbDisconnect(con)

con_spam <- dbConnect(
  MySQL(),
  user = "root", 
  password = "chr0n3!7!",
  dbname = "spambase"
)

data <- dbGetQuery(con_spam, "select * from spambase;")

data %>% glimpse()
