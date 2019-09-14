# Competition1: Personality Profiling
# attach packages
library(tidyverse)

# Getting the data --------------------------------------------------------

setwd("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality")

# source transcript text files
transcript_files = dir("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality/transcripts", full.names = TRUE) 

# encodevlogger ID 
vlogId = basename(transcript_files)
vlogId = str_replace(vlogId, pattern = ".txt$", replacement = "") ### Error

# store text files in a tibble
transcripts = tibble(vlogId = vlogId, Text = map_chr(transcript_files, ~ paste(readLines(.x), collapse = "\\n")), filename = transcript_files)

# read  dataset
pers = read_delim("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality/YouTube-Personality-Personality_impression_scores_train.csv", " ")

gender = read.delim("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality/YouTube-Personality-gender.csv", sep=" ")
names(gender) = c('vlogId', 'gender') #### Why are we doing this, it is already the case?

audiovisual = read.delim("~/Desktop/Behavioural Data Science/BDA/R Scripts/Competition/youtube-personality/YouTube-Personality-audiovisual_features.csv", sep=" ")

vlogs = gender %>% 
  left_join(pers, by = "vlogId") %>% 
  left_join(audiovisual, by = "vlogId") %>% 
  as_tibble()


# Parsing the vlogs -------------------------------------------------------

transcripts_data = transcripts %>% ## Transcripts as words
  unnest_tokens(Word, Text) %>% 
  semi_join(vlogs, by="vlogId") %>% ## Keeps the NAs
  select(-filename)

transcript_data_parsed = transcripts_data %>%
  anti_join(stop_words, by = c("Word" = "word")) ### Removing the stop_words

# Let's look at the words -------------------------------------------------

transcripts_data %>%
  group_by(vlogId) %>%
  count(Word, sort = T)  ### What are the most common words per vlogId?

transcripts_data %>%
  anti_join(stop_words, by = c("Word" = "word")) %>% ### Removing the stop_words
  count(Word, sort = T) %>%
  filter(n >= 370) %>%
  mutate(Word = reorder(Word, n)) %>%
  ggplot(aes(Word, n)) +
  geom_col() +
  coord_flip()

stop_words2 = stop_words %>% ##### This version of stop_words doesn't contain 'I'
  filter(word != "i" & word != "I")

transcripts_data %>% ##### Without removint 'I'
  anti_join(stop_words2, by = c("Word" = "word")) %>% ### Plot with 'I' and the most commonly used words
  count(Word, sort = T) %>%
  filter(n >= 370) %>%
  mutate(Word = reorder(Word, n)) %>%
  ggplot(aes(Word, n)) +
  geom_col() +
  coord_flip()


# Calculate the proportion of stop words which are used and proportion of 'I' and 'you' (per vlog) --------

transcripts_prop = transcripts_data %>% ### This adds proportions for every single word
  count(vlogId, Word) %>%
  group_by(vlogId) %>%
  mutate(prop_word_vlog = n / sum(n))

prop_stop = transcripts_prop %>% ## Create a tibble with only the words which are stop_words, and add proportions to find the proportion of stop words within all of the words in the VLOG
  left_join(stop_words, by = c("Word" = "word")) %>%
  mutate(lexicon = replace(lexicon, lexicon == "SMART" | lexicon == "snowball" |  lexicon == "onix", 1)) %>%
  filter(lexicon == 1) %>%
  distinct() %>%
  group_by(vlogId) %>%
  mutate(prop_sw = sum(prop_word_vlog))

prop_stop_i = transcripts_prop %>% 
  group_by(vlogId) %>% 
  filter(Word == c("i", "I")) %>%
  mutate(sum_i = sum(n)) %>% 
  mutate(prop_i = sum(prop_word_vlog)) %>% 
  select(-c("Word", "n", "prop_word_vlog"))

prop_stop_you = transcripts_prop %>% 
  group_by(vlogId) %>%
  filter(Word == "you") %>%
  mutate(sum_you = sum(n)) %>% 
  mutate(prop_you = sum(prop_word_vlog)) %>% 
  select(-c("Word", "n", "prop_word_vlog"))

prop_stop_you_i = transcripts_prop %>%
  left_join(prop_stop_i, by = "vlogId") %>% 
  mutate(sum_i = replace_na(sum_i, 0), prop_i = replace_na(prop_i, 0)) %>% 
  left_join(prop_stop_you, by = "vlogId") %>% 
  mutate(sum_you = replace_na(sum_you, 0), prop_you = replace_na(prop_you, 0)) %>% 
  mutate(prop_you_i = sum_i / (sum_you + sum_i), prop_you_i =  replace_na(prop_you_i, 0)) %>% 
  select(-c("Word", "n", "prop_word_vlog")) %>% 
  distinct()

prop_stop2 = prop_stop %>%
  select("vlogId", "prop_sw") %>%
  distinct()

prop_stop_i2 = prop_stop_i %>% 
  select("vlogId", "prop_i")

prop_stop_you2 = prop_stop_you %>% 
  select("vlogId", "prop_you")

transcripts_final = transcripts_prop %>%
  select(-c("n", "prop_word_vlog", "Word")) %>% 
  left_join(prop_stop_you_i, by = "vlogId") %>% 
  mutate(prop_i = replace_na(prop_i, 0)) %>% 
  left_join(prop_stop2, by = "vlogId") %>% 
  distinct()


# Assigning sentiments  ---------------------------------------------------

#nrc: 10 sentimets
unique(get_sentiments("nrc")$sentiment) # 10 emotions
nrc_sentiments = get_sentiments("nrc")

# bing: neg/pos 
bing_sentiments = get_sentiments("bing")

library_join = nrc_sentiments %>%
  full_join(bing_sentiments, by=c("word")) %>% 
  mutate(sentiment.y = replace_na(sentiment.y, 0)) %>% #replace NA in column
  mutate(sentiment.x = replace_na(sentiment.x, 0)) #replace NA in column

names(library_join) = c("word","sentiment","bing")

names(transcript_data_parsed) = c("vlogId","word")

emotion_data_final = transcript_data_parsed %>%
  ##  rename('Word' = 'word') %>%   ###############3 We should figure out how to do this one instead
  select(vlogId,word) %>%
  group_by(vlogId) %>%
  left_join(library_join, by = "word") %>%
  drop_na() %>%
  count(sentiment, sort = T) %>%
  mutate(ratio_emo = n/sum(n), ratio_emo = replace_na(ratio_emo, 0)) %>% 
  select(-n) %>%
  spread(sentiment, ratio_emo, fill = 0)

transcripts_final
emotion_data_final


# Binding it together -----------------------------------------------------

final_data = vlogs %>%
  left_join(transcripts_final, by = "vlogId") %>% 
  left_join(emotion_data_final, by = "vlogId") %>% ### Check why trust is NA in the lm
  mutate(gender = ifelse(gender == "Male", 0, 1))

final_data # final dataset training

# Split test-train --------------------------------------------------------

test_data = final_data %>% # Final test dataset
  filter(is.na(Extr))

train_data = final_data # final train dataset


# Visualisation and modelling ---------------------------------------------

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
