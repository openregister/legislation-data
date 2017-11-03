# Scrape legislation.gov.uk for a list of legislation

library(tidyverse)
library(rvest)
library(unpivotr)
library(here)

tsv_path <- here("lists", "scraped.tsv")
xurl <- "http://www.legislation.gov.uk/all?page=5628"
base_url <- "http://www.legislation.gov.uk/all?page="
pages <- 1:5628
all_urls <- paste0(base_url, pages)

cell_url <- function(x) {
  if (is.na(x)) return(NA)
  x %>%
    read_html %>%
    html_node("a") %>%
    html_attr("href")
}

scrape_page <- function(xurl) {
  cat(xurl, "\n")
  html <- read_html(xurl)
  out <-
    html %>%
    html_table() %>%
    .[[1]]
  out$slug <-
    html %>%
    tidy_table() %>%
    .[[1]] %>%
    filter(col == 1, row >= 2) %>%
    mutate(slug = map_chr(html, cell_url)) %>%
    pull(slug)
  write_tsv(out, tsv_path, append = TRUE)
}

file.remove(tsv_path)
writeLines("name\tyear-and-number\ttype\tslug", tsv_path)
legislation <- map_df(all_urls, scrape_page)
