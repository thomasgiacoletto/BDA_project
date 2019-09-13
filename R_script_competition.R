# Competition1: Personality Profiling
# attach packages
library(tidyverse)

Sys.setenv('R_MAX_VSIZE'=32000000000)

# Getting the data --------------------------------------------------------

setwd("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality")

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
head(vlogger)

# Test set: vlogs that has missing personality scores should be predicted
testset_vloggers = vlogger %>%
  filter(is.na(Extr)) %>%
  mutate(gender = ifelse(gender == "Male", 0, 1)) %>% 
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

#### Calculate the proportion of stop words which are used and proportion of 'I' (per vlog) ####

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

prop_stop_ii = train_transcripts_prop %>% 
  filter(Word == c("i", "I")) %>%
  mutate(i = sum(n)) %>% 
  select(-c("Word", "n", "proportion_word_vlog"))


prop_stop_youi = train_transcripts_prop %>% 
  filter(Word == "you") %>%
  mutate(you = sum(n)) %>% 
  select(-c("Word", "n", "proportion_word_vlog"))

prop_stop_you_i = train_transcripts_prop %>%
  left_join(prop_stop_ii, by = "vlogId") %>% 
  mutate(i = replace_na(i, 0)) %>% 
  left_join(prop_stop_youi, by = "vlogId") %>% 
  mutate(you = replace_na(you, 0)) %>% 
  mutate(you_i = i / (you + i)) %>% 
  select(-c("i", "you"))

prop_stop_i = train_transcripts_prop %>%
  group_by(vlogId) %>%
  filter(Word == c("i", "I")) %>%
  mutate(proportion_i = sum(proportion_word_vlog))

prop_stop_you = train_transcripts_prop %>%
  group_by(vlogId) %>%
  filter(Word == "you") %>%
  mutate(proportion_you = sum(proportion_word_vlog))

prop_stop2 = prop_stop %>%
  select("vlogId", "prop_sw") %>%
  distinct()

prop_stop_i2 = prop_stop_i %>% 
  select("vlogId", "proportion_i") %>% 
  distinct()

prop_stop_you2 = prop_stop_you %>% 
  select("vlogId", "proportion_you") %>% 
  distinct()

train_transcripts_prop2 = train_transcripts_prop %>%
  select(-c("n", "proportion_word_vlog", "Word")) %>% 
  left_join(prop_stop_i2, by = "vlogId") %>% 
  mutate(proportion_i = replace_na(proportion_i, 0)) %>% 
  left_join(prop_stop_you2, by = "vlogId") %>% 
  mutate(proportion_you = replace_na(proportion_you, 0)) %>% 
  left_join(prop_stop2, by = "vlogId") # %>% 

#  mutate(ratio_you_i = proportion_i / proportion_you)


# Emotions and proportions ------------------------------------------------

# nrc: 10 sentiments
unique(get_sentiments("nrc")$sentiment) # 10 emotions
nrc_sentiments = get_sentiments("nrc")

# bing: neg/pos 
bing_sentiments = get_sentiments("bing")

library_join = nrc_sentiments %>%
  full_join(bing_sentiments, by=c("word")) %>% 
  mutate(sentiment.y = replace_na(sentiment.y, 0)) %>% #replace NA in column
  mutate(sentiment.x = replace_na(sentiment.x, 0)) #replace NA in column

names(library_join) <- c("word","sentiment","bing")

names(train_data_parsed) <- c("vlogId","word")
train_data_parsed1 = train_data_parsed %>%
  select(vlogId,word) %>%
  group_by(vlogId) %>%
  left_join(library_join, by = "word") %>%
  drop_na() %>%
  count(sentiment, sort = T) %>%
  mutate(ratio_emo = n/sum(n)) 
train_data_parsed1

train_data_parsed1$ratio_emo %>% replace_na(0)

train_data_parsed2 = train_data_parsed1 %>%
  select(-n) %>%
  spread(sentiment, ratio_emo, fill = 0)
train_data_parsed2


# Doing it again, should change for final version -------------------------


test_transcripts_data = testset_vloggers %>% 
  left_join(transcripts_df2, by="vlogId") %>% 
  select(-filename)

# Stop words -------------------------------------------------------------------

test_transcripts_data %>%
  group_by(vlogId) %>%
  count(Word, sort = T)  ### What are the most common words per vlogId?

test_data_parsed = test_transcripts_data %>%
  anti_join(stop_words, by = c("Word" = "word")) ### Removing the stop_words


#### Calculate the proportion of stop words which are used ####


test_transcripts_prop = test_transcripts_data %>% ### This adds proportions for every single word
  count(vlogId, Word) %>%
  group_by(vlogId) %>%
  mutate(proportion_word_vlog = n / sum(n))


test_prop_stop = test_transcripts_prop %>% ## Create a tibble with only the words which are stop_words, and add proportions to find the proportion of stop words within all of the words in the VLOG
  left_join(stop_words, by = c("Word" = "word")) %>%
  mutate(lexicon = replace(lexicon, lexicon == "SMART" | lexicon == "snowball" |  lexicon == "onix", 1)) %>%
  filter(lexicon == 1) %>%
  distinct() %>%
  group_by(vlogId) %>%
  mutate(prop_sw = sum(proportion_word_vlog))

test_prop_stop_ii = test_transcripts_prop %>% 
  filter(Word == c("i", "I")) %>%
  mutate(i = sum(n)) %>% 
  select(-c("Word", "n", "proportion_word_vlog"))

test_prop_stop_youi = test_transcripts_prop %>% 
  filter(Word == "you") %>%
  mutate(you = sum(n)) %>% 
  select(-c("Word", "n", "proportion_word_vlog"))

test_prop_stop_you_i = test_transcripts_prop %>%
  left_join(test_prop_stop_ii, by = "vlogId") %>% 
  mutate(i = replace_na(i, 0)) %>% 
  left_join(test_prop_stop_youi, by = "vlogId") %>% 
  mutate(you = replace_na(you, 0)) %>% 
  mutate(you_i = i / (you + i)) %>% 
  select(-c("i", "you"))

test_prop_stop_i = test_transcripts_prop %>%
  group_by(vlogId) %>%
  filter(Word == c("i", "I")) %>%
  mutate(proportion_i = sum(proportion_word_vlog))

test_prop_stop_you = test_transcripts_prop %>%
  group_by(vlogId) %>%
  filter(Word == "you") %>%
  mutate(proportion_you = sum(proportion_word_vlog))

test_prop_stop2 = test_prop_stop %>%
  select("vlogId", "prop_sw") %>%
  distinct()

test_prop_stop_i2 = test_prop_stop_i %>% 
  select("vlogId", "proportion_i") %>% 
  distinct()

test_prop_stop_you2 = test_prop_stop_you %>% 
  select("vlogId", "proportion_you") %>% 
  distinct()

test_transcripts_prop2 = test_transcripts_prop %>%
  select(-c("n", "proportion_word_vlog")) %>% 
  left_join(test_prop_stop_i2, by = "vlogId") %>% 
  mutate(test_proportion_i = replace_na(proportion_i, 0)) %>% 
  left_join(test_prop_stop_you2, by = "vlogId") %>% 
  mutate(proportion_you = replace_na(proportion_you, 0)) %>% 
  left_join(test_prop_stop2, by = "vlogId")

# Emotions and proportions ------------------------------------------------

test_data_parsed1 = test_data_parsed %>%
  select(vlogId, Word) %>%
  group_by(vlogId) %>%
  left_join(nrc_sentiments, by = c("Word" = "word")) %>%
  drop_na() %>%
  count(sentiment, sort = T) %>%  # need to add columns 'proportion of certain sentiment'
  mutate(ratio_emo = n/sum(n)) %>% 
  filter(!is.na(ratio_emo)) %>% 
  select(-n) %>%
  spread(sentiment, ratio_emo, fill = 0)

final_test = testset_vloggers %>%
  left_join(test_transcripts_prop2, by = "vlogId") %>% 
  left_join(test_data_parsed1, by = "vlogId") ### 

# Binding it together -----------------------------------------------------

### Audiovisual:   vlogger_train
### Words:         train_transcripts_prop2
### Sentiments:    train_data_parsed2

final_data = vlogger_train %>%
  left_join(train_transcripts_prop2, by = "vlogId") %>% 
  left_join(train_data_parsed2, by = "vlogId") ### Check why trust is NA in the lm

final_data # final dataset training
final_test # final dataset test

cor(final_data[, c(2:7)])


# lm.fit = lm(Extr ~ .-vlogId, final_data) ### Error in object[[i]] : object of type 'closure' is not subsettable, ### too many variables included, crashed everytime running!!
# summary(lm.fit)$coefficients

head(final_data)
names(final_data) # 46 variables (incl. vlogId and 5 DV)

lm_fit = lm(cbind(Extr, Agr, Cons, Emot, Open) ~ . -vlogId - Word - trust, data = final_data)# categorical variable 'Word' need to be removed, need to remove one of the nrc emotions (because they add up to 1)

summary(lm_fit)


# Output ------------------------------------------------------------------

# predict on test set
pred_mlm = predict(lm_fit, new=final_test)
head(pred_mlm)


# compute output data frame
final_test_DVremoved = final_test %>% 
  select(-c(Extr:Open))

testset_pred  <- 
  cbind(final_test_DVremoved, pred_mlm) %>%
  select(vlogId, Extr:Open) %>% 
  unique()
head(testset_pred)
nrow(testset_pred) # 80 test data

# wide to long format
testset_pred_long  <- 
  testset_pred %>% 
  gather(axis, Expected, -vlogId) %>%
  arrange(vlogId,axis)
head(testset_pred_long)

# into submission format: column names 'VLOG8_cAGR' as Id
testset_pred_long <- 
  testset_pred_long %>%
  unite(Id, vlogId, axis) 
head(testset_pred_long)

testset_pred_long %>%
  write_csv(path = "predictions.csv")
