# Load packages ----
library(dplyr)
library(magrittr)
library(ggplot2)
library(janitor)
library(readxl)
library(stringr)
library(tidyr)

library(wordcloud) # handling and visualisaing text data

# Load raw data ----
# Get the question names 

short_names <- readr::read_csv('questions.csv')


df_raw <- read_xlsx("data/2022 Survey of Croquet Players - with U18 section (BPE).xlsx",
                    skip = 1,
                    col_types = c('date', rep_len('text', 195)), # keep the datestamp as date format
                    col_names = short_names$short_q %>% head(-1) # remove final row as not referenced
) %>% 
  mutate(responseID = 1:nrow(.)) # Create response IDs

questions <- colnames(df_raw) %>% as_tibble()

# Initial cleaning ----
# * Bring columns together from control flow
temp <- short_names %>% # List of shortname vars with '2' in them
  filter(str_ends(short_q, '2')) %>% 
  mutate(short_q2 = str_sub(short_q, end = -2))
temp2 <- str_sub(temp$short_q, end = -2) #List of shortname vars with duplicates

# columns without duplicated question columns
df_temp <- df_raw %>% 
  select(-all_of(temp2), -all_of(temp$short_q))

# Combine columns of duplicated question columns created by Google Forms control flow - using dplyr::coalesce()
df_multi_col_qs <- df_raw
# see https://stackoverflow.com/questions/70442283/coalesce-pairs-of-variables-within-a-dataframe-based-on-a-regular-expression
df_multi_col_qs <- df_multi_col_qs %>% 
  select(all_of(temp2), all_of(temp$short_q)) %>% 
  names() %>% 
  split(temp2) %>% 
  purrr::map_dfc(~coalesce(!!!df_multi_col_qs[.x][c(1,2)])) # need to check if coalesce works properly

df_all <- df_temp %>% cbind(df_multi_col_qs) %>% 
  select(responseID, everything())

# Basic refactoring
df_all <- df_all %>% 
  mutate(region = as.factor(region)) %>% 
  mutate(across(contains("year_"), as.numeric)) # doesn't quite work for all columns mentioning "year" just yet


