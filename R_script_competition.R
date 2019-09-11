# Competition1: Personality Profiling
# attach packages
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(tidyr)
library(readr)
library(tibble)
library(purrr)
library(dplyr)
library(stringr)
library(forcats)

setwd("~/Desktop/Behavioural Data Science/BDA/R Scripts")

list.files(path = "~/Desktop/Behavioural Data Science/BDA/R Scripts")

# source transcript text files
transcript_files = dir("~/Library/Mobile Documents/com~apple~CloudDocs/UvA/Big Data Analytics/ M2. Multiple Regression/Competition1/youtube-personality/transcripts", full.names = TRUE) 
head(transcript_files)

# encodevlogger ID 
vlogId = basename(transcript_files)
vlogId = str_replace(vlogId, pattern = ".txt$", replacement = "")
head(vlogId)

# store text files in a dataframe
transcripts_df = tibble(vlogId = vlogId, Text = map_chr(transcript_files, ~ paste(readLines(.x), collapse = "\\n")), filename = transcript_files)
transcripts_df %>% head()

# read  dataset
pers = read_delim("~/Library/Mobile Documents/com~apple~CloudDocs/UvA/Big Data Analytics/ M2. Multiple Regression/Competition1/youtube-personality/YouTube-Personality-Personality_impression_scores_train.csv", " ")
head(pers)

gender = read.delim("~/Library/Mobile Documents/com~apple~CloudDocs/UvA/Big Data Analytics/ M2. Multiple Regression/Competition1/youtube-personality/YouTube-Personality-gender.csv", head=FALSE, sep=" ", skip = 2)
names(gender) = c('vlogId', 'gender')
head(gender)

audiovisual = read.delim("~/Library/Mobile Documents/com~apple~CloudDocs/UvA/Big Data Analytics/ M2. Multiple Regression/Competition1/youtube-personality/YouTube-Personality-audiovisual_features.csv", header = T,sep=" ")
names(audiovisual) 
head(audiovisual)

vlogger_df = left_join(gender, pers)
vlogger_df = left_join(audiovisual, pers, by = "vlogId")
head(vlogger_df) 

# Test set: vlogs that has missing personality scores should be predicted 
testset_vloggers = vlogger_df %>% 
  filter(is.na(Extr))
head(testset_vloggers)




# Work in progress: Parsing and prediction --------------------------------



### Predictions

## 1. transcripts - sentiment analysis
transcripts_df
transcripts_df[1,]

get_sentiments("afinn") # words with values [-5,5]
get_sentiments("bing") # negative/positive
get_sentiments("nrc") # categories 





## 2. audiovisual clues - mlm
vlogger_df 
names(vlogger_df)

# fit_mlm <- lm(cbind(Extr, Agr, Cons, Emot, Open) ~ x1 + x2 + ..., data = vlogger_df)





#################### Cera

library(tidytext)
# every word as one token per row, instead of lines
transcripts_df %>% 
  unnest_tokens(Word, Text) # capital T, 

transcripts_testvlog <- semi_join(transcripts_df,testset_vloggers, by="vlogId")





# predict on test set
pred_mlm = predict(fit_mlm, new = testset_vloggers)
head(pred_mlm)
# remove old prediction from testset_features
testset_vloggers = testset_vloggers %>% 
  select(-Extr, -Agr, -Cons, -Emot, -Open)

# compute output data frame
testset_pred  <- 
  cbind(testset_vloggers, pred_mlm) %>%
  select(vlogId, Extr:Open)
head(testset_pred)

# wide to long format
testset_pred_long  <- 
  testset_pred %>% 
  gather(axis, value, -vlogId) %>%
  arrange(vlogId,axis)
head(testset_pred_long)

# into submission format: column names 'VLOG8_cAGR' as Id
testset_pred_long <- 
  testset_pred_long %>%
  unite(Id, vlogId, axis) 
head(testset_pred_long)

testset_pred_long %>%
  write_csv(path = "predictions.csv")

