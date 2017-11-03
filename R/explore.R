# Explore data scraped from legislation.gov.uk

library(tidyverse)
library(stringr)
library(here)

leg <- read_tsv(here("lists", "scraped.tsv"))

# Are all the years readsonable, and the distribution?
years <-
  leg %>%
  mutate(year = as.integer(str_sub(`year-and-number`, 1L, 4L)))

# The years look completely reasonable
sort(unique(years$year))

# There's precious little until the C21st, especially before the C19th.
years %>%
  count(year, sort = TRUE) %>%
  ggplot(aes(year, n)) +
  geom_bar(stat = "identity")

# It really only kicks off in 1972, with another big year in 1970.
# There's a drop in the last three years -- is there a lag?  Is legislation
# dated by the year it was written, rather than the year it was passed?
years %>%
  count(year, sort = TRUE) %>%
  filter(year >= 1900) %>%
  ggplot(aes(year, n)) +
  geom_bar(stat = "identity")

# There was noticably more legislation from 2010 to 2014.
years %>%
  count(year, sort = TRUE) %>%
  filter(year >= 2000) %>%
  ggplot(aes(year, n)) +
  geom_bar(stat = "identity")

# What formats of year and number are there?  A lot
# This looks for the strings other than the year and the number
split_year_number <- function(x) {
  unlist(str_split(x, "\\s", n = Inf))
}

x <-
  leg %>%
  mutate(substring = map(`year-and-number`, split_year_number)) %>%
  unnest() %>%
  group_by(slug) %>%
  mutate(id = row_number()) %>%
  ungroup()

x %>%
  filter(!(id %in% c(1, 3))) %>%
  count(id, substring, sort = TRUE)

x %>%
  group_by(slug) %>%
  summarise(n_substrings = max(id)) %>%
  count(n_substrings, sort = TRUE)

x %>%
  group_by(slug) %>%
  filter(max(id) == 5) %>%
  ungroup() %>%
  slice(1:10) %>%
  select(`year-and-number`, slug)

# How many titles are SHOUTY CAPS?  Only 32 after all
leg %>%
  filter(name == toupper(name)) %>%
  print(n = Inf)

# How many titles begin with "the"? Most of them
leg %>%
  mutate(name = tolower(name),
         the = str_detect(name, "^the")) %>%
  count(the, sort = TRUE)

# Are the legislation types reasonable?
count(leg, type, sort = TRUE) %>%
  print(n = Inf)

# What's the distribution of types of legislation?  Not much from Scotland
years %>%
  filter(year >= 1970) %>%
  ggplot(aes(year, fill = type)) +
  geom_bar(stat = "count", position = "stack")
