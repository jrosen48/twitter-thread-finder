# loading, setting up
library(tidyverse)
library(rtweet)
source('processing-twitter-threads-funcs.R')

d <- read_csv("ex-data.csv")

d$status_id <- as.character(d$status_id)

# getting replies recursively
new_reply_tweets <- get_replies_recursive(d$reply_to_status_id)

# processing threads
dd <- rbind(d, new_reply_tweets) %>% 
  mutate(day = lubridate::round_date(created_at, "day"))

nrow(d); nrow(dd) # added 928 tweets

threads <- map(dd$status_id, thread_finder, d = dd)

thread_list <- tibble(ID = 1:length(threads),
                      thread = threads)

thread_df <- thread_list %>% 
  unnest(thread) %>% 
  group_by(ID) %>% 
  mutate(thread_string = toString(thread)) %>% 
  select(ID, thread_string) %>% 
  distinct(thread_string, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(row_number = 1:nrow(.))


# df of all of the threads
nrow(thread_df) # 1083

# not necessary in this case, but necessary in some - same n as before
thread_df <- thread_df %>% 
  unique()
nrow(thread_df) # 1083

# recursively searching for shorter version of longer threads
# 513 unique threads because some are shorter versions of longer ones
shorter_thread_list <- map2(.x = thread_df$thread_string, .f = remove_short_threads, d = thread_df, .y = 1:nrow(thread_df))

# this is our 513 unique longest threads and their status IDs
tibble(ID = 1:length(shorter_thread_list),
       thread_string = map_chr(shorter_thread_list, ~.)) %>% 
  select(-ID) %>% # this gets rid of our old ID
  left_join(thread_df) %>% 
  select(ID, thread_string) %>% # this uses the ID from our larger df
  mutate(thread_string = str_split(thread_string, ", ")) %>% 
  unnest(thread_string) %>% 
  rename(status_id = thread_string) %>%
  group_by(status_id) %>% 
  summarize(id_string = toString(ID)) %>% View()

# this creates a df with every status ID and what thread they're part of
new_thread_df <- tibble(ID = 1:length(shorter_thread_list),
                        thread_string = map_chr(shorter_thread_list, ~.)) %>% 
  select(-ID) %>% # this gets rid of our old ID
  left_join(thread_df) %>% 
  select(ID, thread_string) %>% # this uses the ID from our larger df
  mutate(thread_string = str_split(thread_string, ", ")) %>% 
  unnest(thread_string) %>% 
  rename(status_id = thread_string) %>%
  group_by(status_id) %>% 
  summarize(id_string = toString(ID)) %>% 
  mutate(id_string = str_split(id_string, ", ")) %>% 
  unnest(id_string) %>% 
  mutate(status_id = as.character(status_id)) %>% 
  mutate(id_string = str_pad(id_string, 4, pad = "0")) %>% 
  mutate(status_id = as.character(status_id)) %>% 
  distinct() %>% 
  arrange(id_string)

# putting together the final dataset
out_df <- dd %>% 
  left_join(new_thread_df) %>% 
  arrange(created_at) %>% 
  mutate(id_factor = as.integer(fct_inorder(as.factor(id_string), ordered = TRUE))) %>% 
  mutate(id_factor = str_pad(as.character(id_factor), 4, pad = "0")) %>% 
  arrange(id_string)

nrow(out_df) # this adds some rows because of tweets that appear in multiple threads

out_df %>% select(status_id, id_string, text)
