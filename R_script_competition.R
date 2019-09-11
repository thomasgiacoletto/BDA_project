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


# Getting the data --------------------------------------------------------

setwd("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality")

list.files(path = "~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality")

# source transcript text files
transcript_files = dir("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality/transcripts", full.names = TRUE) 
head(transcript_files)

# encodevlogger ID 
vlogId = basename(transcript_files)
vlogId = str_replace(vlogId, pattern = ".txt$", replacement = "") ### Error
head(vlogId)

# store text files in a dataframe
transcripts_df = tibble(vlogId = vlogId, Text = map_chr(transcript_files, ~ paste(readLines(.x), collapse = "\\n")), filename = transcript_files)
transcripts_df %>% head()

# read  dataset
pers = read_delim("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality/YouTube-Personality-Personality_impression_scores_train.csv", " ")
head(pers)

gender = read.delim("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality/YouTube-Personality-gender.csv", sep=" ")
names(gender) = c('vlogId', 'gender')
head(gender)

audiovisual = read.delim("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality/YouTube-Personality-audiovisual_features.csv", sep=" ")
names(audiovisual) 
head(audiovisual)

vlogger = left_join(gender, pers)
vlogger_df = left_join(vlogger, audiovisual, by = "vlogId")
head(vlogger_df) 

# Test set: vlogs that has missing personality scores should be predicted 
testset_vloggers = vlogger_df %>% 
  filter(is.na(Extr))
head(testset_vloggers)
as.tibble()

# Parsing the vlogs -------------------------------------------------------

# every word as one token per row, instead of lines
transcripts_df2 = transcripts_df %>% 
  unnest_tokens(Word, Text) # capital T,

test_transcripts_data = semi_join(transcripts_df2,testset_vloggers, by="vlogId") ## Keeps the NAs
test_transcripts_data

train_transcripts_data = anti_join(transcripts_df2,testset_vloggers, by="vlogId") ## Removes the NAs
train_transcripts_data
 

##### Calculate the most used words (90%) for every individual vlog, and 

###### Create a new var which has the proportion of stop words used, and then remove the stop words to continue with the analysis


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

