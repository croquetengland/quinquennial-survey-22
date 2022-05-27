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
# Load short question names 
short_names <- readr::read_csv('questions.csv')

# Load raw data
df_raw <- read_xlsx("data/2022 Survey of Croquet Players - with U18 section (BPE).xlsx",
                    skip = 1,
                    col_types = c('date', rep_len('text', 195)), # keep the datestamp as date format
                    col_names = short_names$short_q %>% head(-1) # remove final row as not referenced
) %>% 
  mutate(responseID = 1:nrow(.)) # Create response IDs

# Get long list of questions for further manipulation
questions <- colnames(df_raw) %>% as_tibble()

# Basic refactoring
df_raw <- df_raw %>% 
  mutate(region = as.factor(region)) %>% 
  mutate(across(contains("year_"), as.numeric)) # doesn't quite work for all columns mentioning "year" just yet

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

# Load corrections list ----
df_corrections <- read_xlsx("data/2022 Survey of Croquet Players (final-anonymised with tidying suggestions.xlsx",
                            range = cell_cols("GO:GQ"))
summary_corrections <- df_corrections %>% count(`Comment from Beatrice`)

# Implement corrections ----
# * Incorrect U18s ----
correct_u18 <- df_corrections %>% 
  filter(`Comment from Beatrice` == 'Not under 18 - answers need transferring')

# double check these are the relevant rows
tbl_correct_u18 <- df_all %>% 
  filter(responseID %in% correct_u18$`Response ID`) %>% 
  select(responseID, is_u18, what_year_born, comments)

df_clean <- df_all %>% 
  mutate(is_u18 = if_else(responseID %in% tbl_correct_u18$responseID, 
                          "No", 
                          is_u18),
         # what_year_born2 = ifelse(responseID %in% tbl_correct_u18$responseID, 
         #                          NA, 
         #                          what_year_born2),
         what_year_born = ifelse(responseID == 4, 
                                 1949,
                                 if_else(responseID == 1079, 
                                         1935,
                                         what_year_born)
         )
  )

# * changing wording of main code to be same in both U18 and >18 sections
df_clean <- df_clean %>% 
  mutate(main_code_played = case_when(
    main_code_played == "I am mostly a GC player" ~ "I am predominantly a GC player",
    main_code_played == "I am mostly an AC player" ~ "I am predominantly an AC player",
    TRUE ~ main_code_played
  ))

df_clean %>% 
  count(main_code_played)

