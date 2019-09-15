# Competition1: Personality Profiling
# attach packages
library(tidyverse)
library(tidytext)
library(caret)
# library(styler)
# library(lintr)

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
  mutate(prop_sw = sum(prop_word_vlog)) %>%
  select("vlogId", "prop_sw") %>%
  distinct()

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

transcripts_final = transcripts_prop %>%
  select(-c("n", "prop_word_vlog", "Word")) %>% 
  left_join(prop_stop_you_i, by = "vlogId") %>% 
  mutate(prop_i = replace_na(prop_i, 0)) %>% 
  left_join(prop_stop, by = "vlogId") %>% 
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

# add: 'bing' variables: prop_positive, prop_negative
bing_data = transcript_data_parsed %>%
  group_by(vlogId) %>%
  left_join(library_join, by = c("word")) %>%
  drop_na() %>%
  count(bing, sort = T) %>% 
  filter(bing %in% c("negative","positive")) %>% 
  mutate(prop_pos_or_neg = n/sum(n), prop_pos_or_neg = replace_na(prop_pos_or_neg, 0)) %>% 
  select(-n) %>% 
  spread(bing, prop_pos_or_neg, fill = 0) %>% 
  rename(prop_positive = positive, prop_negative = negative) 

# add: 'bing' variables: num_pos, num_neg, nPos_to_nNeg
bing_data_final = transcript_data_parsed %>%
  group_by(vlogId) %>%
  left_join(library_join, by = c("word")) %>%
  drop_na() %>%
  count(bing, sort = T) %>% 
  filter(bing %in% c("negative","positive")) %>% 
  mutate(num_pos_or_neg = n, num_pos_or_neg = replace_na(num_pos_or_neg, 0)) %>% 
  select(-n) %>% 
  spread(bing, num_pos_or_neg, fill = 0) %>% 
  rename(num_pos = positive, num_neg = negative) %>% 
  mutate(nPos_to_nPsNg = num_pos/(num_pos+num_neg), nPos_to_nPsNg = replace_na(nPos_to_nPsNg, 0)) %>% 
  left_join(bing_data, by = c("vlogId"))


# all emotion variables (nrc, bing) included 
emotion_data_final = transcript_data_parsed %>%
  select(vlogId,word) %>%
  group_by(vlogId) %>%
  left_join(library_join, by = "word") %>%
  drop_na() %>%
  count(sentiment, sort = T) %>%
  mutate(ratio_emo = n/sum(n), ratio_emo = replace_na(ratio_emo, 0)) %>% 
  select(-n) %>%
  spread(sentiment, ratio_emo, fill = 0) %>% 
  left_join(bing_data_final, by = "vlogId") %>% 
  select(-`0`)

str(transcripts_final)
str(emotion_data_final)

# Binding it together -----------------------------------------------------

final_data = vlogs %>%
  left_join(transcripts_final, by = "vlogId") %>% 
  left_join(emotion_data_final, by = "vlogId") %>% 
  mutate(gender = ifelse(gender == "Male", 0, 1))

final_data # final dataset training

# Split test-train --------------------------------------------------------

test_data = final_data %>% # Final test dataset
  filter(is.na(Extr))
str(test_data)

train_data = final_data %>% # final train dataset
  filter(!is.na(Extr)) %>% 
  select(- vlogId)

str(train_data)

# Visualisation and modelling ---------------------------------------------

# Visualising the data -----------------------------------------------------

# Extr ~ prop_sw
ggplot(train_data, aes(x=prop_sw, y=Extr, color = as.factor(gender))) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Proportion of Stop Words") +
  ylab("Extraversion") +
  ggtitle("  Extraverted people use less stop words!")

# Extr ~ sum_you
ggplot(train_data, aes(x=anger, y=Open, color = as.factor(gender))) +
  geom_point() + 
  geom_smooth(method=lm) +
  xlab("Number of 'you'words used") +
  ylab("Extraversion") +
  ggtitle("  Extraverted people use more 'you'! ")

# Cons ~ time.speaking
ggplot(train_data, aes(x=time.speaking, y=Cons, color = as.factor(gender))) +
  geom_point() + 
  geom_smooth(method=lm) +
  xlab("time speaking") +
  ylab("Conscientiousness") +
  ggtitle("  Conscientious people speak longer in videos! ")

# Agr ~ nPos_to_nPsNg
ggplot(train_data, aes(x=nPos_to_nPsNg, y=Agr, color = as.factor(gender))) +
  geom_point() + 
  geom_smooth(method=lm) +
  xlab("Proportion of Positive words among pos/neg") +
  ylab("Agreeableness") +
  ggtitle("  Agreeable people used more positive words in their speech!")

# Find correlations -------------------------------------------------------

cor_mat = train_data %>% 
  select(-c("Extr", "Agr", "Cons", "Emot", "Open")) %>% 
  cor() %>% 
  findCorrelation(cutoff = .75) + 5 
train_data2 = train_data[,-cor_mat] ## Removes the 8 variables which have are highly correlated (>75)

# function model and summary for each trait
ModelSummary = function(trait){
  big5 = c("Extr", "Agr", "Cons", "Emot","Open")
  
    cor_mat_without_trait = train_data2 %>% 
    select(-c(big5[! (trait == big5) ])) %>% 
    cor()
  
  cor_mat2 = enframe(cor_mat_without_trait[, 2]) %>% 
    filter(!name == trait) %>% 
    filter(abs(value) >= .1) %>% 
    pull(name)
  
  formula = as.formula(paste(trait, paste(cor_mat2, collapse = " + "), sep = " ~ "))
  lm_fit = lm(formula, train_data, na.action = na.exclude) 
  summary(lm_fit)
}


ModelSummary("Extr") # R-squared 0.2877 
ModelSummary("Agr")  # R-squared 0.3216 
ModelSummary("Open") # R-squared 0.1012  
ModelSummary("Cons") # R-squared 0.1212  
ModelSummary("Emot") # R-squared 0.1349

------------


# Test --------------------------------------------------------------------



# Output ------------------------------------------------------------------

lm_fit_extr
lm_fit_agr
lm_fit_cons
lm_fit_emot
lm_fit_open

predictions_Ids <- testset %>%
  select(1) %>% 
  cbind(Extr, Agr, Cons, Emot, Open) %>% 
  gather(axis, value, -vlogId) %>%
  arrange(vlogId,axis) %>% 
  unite(Id, vlogId, axis)



# predict on test set
pred_mlm = predict(lm_fit_extr, test_data, type = "resp")
head(pred_mlm)
str(pred_mlm)

Extr = predict(lm_fit_extr, test_data, type = "resp")
Agr = predict(lm_fit_agr, test_data, type = "resp")
Cons = predict(lm_fit_cons, test_data, type = "resp")
Emot = predict(lm_fit_emot, test_data, type = "resp")
Open = predict(lm_fit_open, test_data, type = "resp")


# compute output data frame
final_test_DVremoved = test_data %>% 
  select(1) %>% 
  cbind(extr, agr, cons, emot, open) %>% 
  gather(axis, Expected, -vlogId) %>%
  arrange(vlogId,axis) %>% 
  unite(Id, vlogId, axis)

head(final_test_DVremoved)

final_test_DVremoved %>%
  write_csv(path = "predictions.csv")



predict_mlm = cbind(extr, agr, cons, emot, open)




# compute output data frame
final_test_DVremoved = test_data %>% 
  select(-c(Extr:Open))

testset_pred  <- 
  cbind(final_test_DVremoved, predict_mlm) %>%
  select(vlogId, extr:open) %>% 
  unique()
head(testset_pred)
nrow(testset_pred) # 80 test data

# wide to long format
testset_pred_long  <- 
  testset_pred %>% 
  gather(axis, Expected, - vlogId) %>%
  arrange(vlogId, axis)
head(testset_pred_long)

# into submission format: column names 'VLOG8_cAGR' as Id
testset_pred_long <- 
  testset_pred_long %>%
  unite(Id, vlogId, axis) 
head(testset_pred_long)

----------------------------------------------------------------#

predictions =  test_data %>%
  select(vlogId) %>% 
  cbind(Extr, Agr, Cons, Emot, Open) %>% 
  gather(axis, Expected, -vlogId) %>%
  arrange(vlogId, axis) %>% 
  unite(Id, vlogId, axis)

predictions %>%
  write_csv(path = "predictions.csv")




