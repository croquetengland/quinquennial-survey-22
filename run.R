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
df_raw <- read_xlsx("data/2022 Survey of Croquet Players (final-anonymised with tidying suggestions.xlsx",
                    skip = 1,
                    col_types = c('date', rep_len('text', 195), 'numeric'), # keep the datestamp as date format
                    col_names = c(short_names$short_q %>% head(-1), "responseID"), # remove final row as not referenced
                    range = cell_cols("A:GO")
) %>% 
  slice(-1) # Drop top row the header column as the 'skip' argument is ignored by the presence of 'range'
  # mutate(responseID = 1:nrow(.)) # Create response IDs

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

# df_clean %>% 
#   count(main_code_played)

# * Set factor levels for club subs
df_clean <- df_clean %>% 
  mutate(club_subs = factor(club_subs, 
                            levels = c(
                              "There is no annual subscription at my club",
                              "£50 or less per year",
                              "£51 - £100",
                              "£101 - £150",
                              "£151 - £200",
                              "£201 - £250",
                              "£251 - £300",
                              "More than £300 per year"

                              )
                            )
         )

# * Set factor levels for distances to main club
df_clean <- df_clean %>% 
  mutate(distance_home_to_main_club = factor(
    distance_home_to_main_club,
    levels = c(
      "less than 1 mile",
      "Between 1 and 3 miles",
      "Between 3 and 5 miles",
      "Between 5 and 10 miles",
      "Between 10 and 25 miles",
      "more than 25 miles" 
    )
  ))

# * Set regions to named Federations
df_clean2 <- df_clean %>% 
  mutate(
    region2 = case_when(
      # Clean free text
      region == "Australia - ACT" ~ "Australia",
      region == "Canada (Ontario)" ~ "Canada",
      region == "Derbyshire" ~ "East Midlands",
      region %in% c("Mid wales", "Wales but part of WMF", "West Wales") ~ "West Midlands",
      region == "NZ" ~ "New Zealand",
      region == "Southeast USA" ~ "USA",
      region == "the pennines" ~ "East Midlands",
      region == "overseas" ~ "Overseas",
      region == "Sussex" ~ "South East",
      region == "Los Angeles, CA" ~ "USA",
      region == "United States" ~ "USA",
      # Tidy up groupings
      region == "South East (Surrey, Kent, West and East Sussex)" ~ "South East",
      region == "West Midlands (Staffordshire, Shropshire, Hereford and Worcester, Warwickshire, Central Wales)" ~ "West Midlands",
      region == "Southern (Oxfordshire, Berkshire, Buckinghamshire, Hampshire, Isle of Wight)" ~ "Southern",
      region == "South West (Gloucestershire, Somerset, Wiltshire, Dorset, Devon, Cornwall, South Wales)" ~ "South West",
      region == "East Anglia (Norfolk, Suffolk, Cambridgeshire, Bedfordshire, Hertfordshire, Essex)" ~ "East Anglia",
      region == "North West (Lancashire, Greater Manchester, Merseyside, Cheshire, Cumbria, North Wales)" ~ "North West",
      region == "North (Northumberland, Tyne and Wear, Durham, Cleveland)" ~ "North",
      region == "East Midlands (Derbyshire, Nottinghamshire, Lincolnshire, Leicestershire, Rutland, Northamptonshire)" ~ "East Midlands",
      TRUE ~ region
    )
  )
 
df_clean2 <- df_clean2 %>% 
  mutate(played_le_5yr = year_first_played_croquet >= 2017)
