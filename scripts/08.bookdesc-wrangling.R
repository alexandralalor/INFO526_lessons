# load libraries that we need for this wrangling
library(tidyverse)
library(spacyr)
library(janitor)

# read data in and clean up names
programming_books <- read_csv("raw-data/prog_book.csv") %>%
  clean_names()

scifi_books <- read_csv("raw-data/sf_aliens.csv") %>%
  clean_names()

# fix column names and make sure we have no repeated observations
scifi_books_selected <- scifi_books %>%
  select(title = book_title,
         author = author_name,
         description = book_description) %>%
  distinct(title, author, description)

# same as above but we don't have author so add column with NAs
# note: not using author anyway, but keep it for possible data merging
programming_books_selected <- programming_books %>%
  select(title = book_title,
         description) %>%
  mutate(author = NA) %>%
  distinct(title, author, description)

####DATA ANNOTATION ####
# initialize spacy with english model (need python and spacy installed to run this)
spacy_initialize(model = "en_core_web_lg")

# annotate data
scifi_annotated <- spacy_parse(scifi_books_selected$description,
                                   pos = TRUE,
                                   tag = FALSE,
                                   entity = FALSE,
                                   dependency = FALSE,
                                   nounphrase = FALSE)

programming_books_annotated <- spacy_parse(programming_books_selected$description,
                               pos = TRUE,
                               tag = TRUE,
                               entity = TRUE,
                               dependency = TRUE,
                               nounphrase = TRUE)

# count stuff, calculate total number of words per description/observation
# then calculate normalized frequency by dividing count per total words
scifi_pos_count <- scifi_annotated %>%
  group_by(doc_id, pos) %>%
  summarize(count = n()) %>%
  mutate(total_words = sum(count),
         norm_freq = count/total_words,
         type = "sci-fi") 

programming_books_pos_count <- programming_books_annotated %>%
  group_by(doc_id, pos) %>%
  summarize(count = n()) %>%
  mutate(total_words = sum(count),
         norm_freq = count/total_words,
         type = "programming")

# merge data, remove total words and counts, no need to keep that
books_data <- bind_rows(scifi_pos_count,
                        programming_books_pos_count) %>%
  select(-total_words, -count) %>%
  filter(!(pos %in% c("NUM", "SPACE", "SYM", "X"))) %>%
  pivot_wider(names_from = pos,
              values_from = norm_freq) %>%
  replace(is.na(.), 0) 

# write file to disk
books_data %>%
  write_csv("data/book_data.csv")

