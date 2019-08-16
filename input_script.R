# Prepare input data for model:

library('stringi')
library('tidyverse')
library('tidytext')
library('syuzhet')
library('caret')
data("stop_words")
set.seed(123)

# Load models & data input format
load('models/model_input_df.rda')
load('models/model_rf.rda')


input_dat = tibble(id = 1, review = "Poor execution of what was a beautiful and wonderful book!")

dat = input_dat %>%
  mutate(sentiment = get_sentiment(review),
         number_of_words = stri_count(review, regex="\\S+"),
         number_of_letters = nchar(review),
         avg_word_length = number_of_letters / number_of_words) %>%
  select(id, sentiment, number_of_words, number_of_letters,avg_word_length)


text_dat = input_dat %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words, by = 'word') %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  filter(word != 'br') %>%
  filter(word != 'movie') %>%
  filter(word != 'film') %>%
  distinct(id, word) %>%
  mutate(n = 1) %>%
  spread(key = word, value = n)

prep_dat = dat %>% left_join(text_dat, by = 'id') %>% select(-id)
final_dat = model_input_df %>% 
  bind_rows(prep_dat) %>% 
  select(-id)
final_dat[is.na(final_dat)] = 0

predict(model_rf, final_dat)

