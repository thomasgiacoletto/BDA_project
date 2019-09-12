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
vlogger = left_join(vlogger, audiovisual, by = "vlogId")
head(vlogger_df)

# Test set: vlogs that has missing personality scores should be predicted
testset_vloggers = vlogger_df %>%
  filter(is.na(Extr)) %>%
  as_tibble()
head(testset_vloggers)


# Audiovisual data --------------------------------------------------------

vlogger_train = as_tibble(vlogger) %>%
  drop_na() %>%
  mutate(gender = ifelse(gender == "Male", 0, 1))

# Parsing the vlogs -------------------------------------------------------

# every word as one token per row, instead of lines
transcripts_df2 = transcripts_df %>%
  unnest_tokens(Word, Text) # capital T!

test_transcripts_data = semi_join(transcripts_df2,testset_vloggers, by="vlogId") %>% ## Keeps the NAs
  select(-filename)
test_transcripts_data

train_transcripts_data = anti_join(transcripts_df2,testset_vloggers, by="vlogId") %>%  ## Removes the NAs
  select(-filename)
train_transcripts_data

######################## Here we need to find a way to add the rest of the vlogger_df data to these tibbles so we can run stuff


##### Calculate the most used words (90%) for every individual vlog, and


# Stop words -------------------------------------------------------------------

train_transcripts_data %>%
  group_by(vlogId) %>%
  count(Word, sort = T)  ### What are the most common words per vlogId?

train_data_parsed = train_transcripts_data %>%
  anti_join(stop_words, by = c("Word" = "word")) ### Removing the stop_words

train_data_parsed %>% ### Plot the most used words after removing all of the stop_words
  count(Word, sort = T) %>%
  filter(n >= 370) %>%
  mutate(Word = reorder(Word, n)) %>%
  ggplot(aes(Word, n)) +
  geom_col() +
  coord_flip()

stop_words2 = stop_words %>% ##### This version of stop_words doesn't contain 'I'
  filter(word != "i" & word != "I")

train_data_parsed2 = train_transcripts_data %>% ##### Without removint 'I'
  anti_join(stop_words2, by = c("Word" = "word"))

train_data_parsed2 %>% ### Plot with 'I' and the most commonly used words
  count(Word, sort = T) %>%
  filter(n >= 370) %>%
  mutate(Word = reorder(Word, n)) %>%
  ggplot(aes(Word, n)) +
  geom_col() +
  coord_flip()

#### Calculate the proportion of stop words which are used ####


train_transcripts_prop = train_transcripts_data %>% ### This adds proportions for every single word
  count(vlogId, Word) %>%
  group_by(vlogId) %>%
  mutate(proportion_word_vlog = n / sum(n))


prop_stop = train_transcripts_prop %>% ## Create a tibble with only the words which are stop_words, and add proportions to find the proportion of stop words within all of the words in the VLOG
  left_join(stop_words, by = c("Word" = "word")) %>%
  mutate(lexicon = replace(lexicon, lexicon == "SMART" | lexicon == "snowball" |  lexicon == "onix", 1)) %>%
  filter(lexicon == 1) %>%
  distinct() %>%
  group_by(vlogId) %>%
  mutate(prop_sw = sum(proportion_word_vlog))

prop_stop2 = prop_stop %>%
  select("vlogId", "prop_sw") %>%
  distinct()

train_transcripts_prop2 = train_transcripts_prop %>%
  left_join(prop_stop2, by = "vlogId") %>%
  select(-"n")


# Emotions and proportions ------------------------------------------------

# 10 sentiments in nrc
unique(get_sentiments("nrc")$sentiment)
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
nrc_trust <- get_sentiments("nrc") %>%
  filter(sentiment == "trust")
nrc_fear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")
nrc_negative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")
nrc_sadness <- get_sentiments("nrc") %>%
  filter(sentiment == "sadness")
nrc_anger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")
nrc_surprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")
nrc_positive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")
nrc_disgust <- get_sentiments("nrc") %>%
  filter(sentiment == "disgust")
nrc_anticipation <- get_sentiments("nrc") %>%
  filter(sentiment == "anticipation")


nrc_sentiments = rbind(nrc_joy,nrc_trust,nrc_fear,nrc_negative,nrc_sadness,nrc_anger,nrc_surprise,nrc_positive,nrc_disgust,nrc_anticipation)


names(train_data_parsed) <- c("vlogId","word")
train_data_parsed1 = train_data_parsed %>%
  select(vlogId,word) %>%
  group_by(vlogId) %>%
  left_join(nrc_sentiments, by = "word") %>%
  drop_na() %>%
  count(sentiment, sort = T) %>%  # need to add columns 'proportion of certain sentiment'
  mutate(ratio_emo = n/sum(n))

train_data_parsed1

train_data_parsed1$ratio_emo %>% replace_na(0)

train_data_parsed2 = train_data_parsed1 %>%
  select(-n) %>%
  spread(sentiment, ratio_emo, fill = 0)


# Binding it together -----------------------------------------------------

### Audiovisual:   vlogger_train
### Words:         train_transcripts_prop2
### Sentiments:    train_data_parsed2

final_data = vlogger_train %>%
  left_join(train_transcripts_prop2, train_data_parsed2, by = "vlogId")

cor(final_data[, c(2:7)])

lm.fit = lm(Extr ~ .-vlogId, final_data)
summary(lm.fit)$coefficients


## --> Compute emotion indexes to create variables we can use for analysis

# Work in progress: Parsing and prediction --------------------------------




# Predictions -------------------------------------------------------------


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
