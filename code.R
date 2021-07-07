library(rvest)
library(dplyr)
library(purrr)
library(xml2)
library(readr)
library(stringr)
library(tidytext)
library(ggplot2)
library(lubridate)

# ###########################
# cdc news releases
# ###########################
url_base <- "https://www.cdc.gov"
   
# cdc
h <- read_html("https://www.cdc.gov/coronavirus/2019-ncov/whats-new-all.html")
nodes <- h %>%  html_nodes(css = 'a')
nodes <- nodes[str_detect(nodes, '^<a href="/coronavirus/2019-ncov/')]
nodes <- nodes[!(str_detect(nodes, 'index')|str_detect(nodes,'omepage'))]
urls <- paste0(url_base, html_attr(nodes, "href"))

releases <- sapply(urls, function(url) {
  news <- read_html(url)
  news <- news %>% html_nodes(".last-updated , .h3 , .splash-col li , .splash-col p")
  news_list <- news %>% as_list(news)
  news_unlisted <-unlist(news_list)
  news <- tibble(news_unlisted)
} )

# unnest tokens from each news release
tokens <- lapply(releases, function(release) {
  news_unlisted <-unlist(release)
  words <- unnest_tokens(tibble(news_unlisted), word, news_unlisted)
  filtered_words <- words %>% filter(!word %in% stop_words$word) 
  })

# extract date of each news release
dates <- releases %>% str_extract("[A-z]*.?\\s\\d{1,2},\\s\\d{4}") %>% mdy()

#tok <- tokens[1]
finn <- get_sentiments("afinn")

sentiment_scores <- lapply(tokens, function(tok) {
  afinn <- tok %>% inner_join(finn) %>% summarize(sentiment = sum(value))})

news_sentiment <- data.frame(date = dates, score = matrix(unlist(sentiment_scores)), website = urls)

# ###########################
# US covid counts
# ###########################
counts <- read_csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
new_case_counts <- counts %>% group_by(submission_date) %>% rename(date = submission_date) %>% mutate(date = mdy(date)) %>% summarize(cases = sum(new_case))
new_case_counts %>% ggplot(aes(date, cases)) + geom_line()

# #############################
# plot sentiments and case counts
# #############################

ggplot() + geom_col(data = new_case_counts, aes(date, cases/1000), color = 'red') + geom_col(data = news_sentiment, aes(date, score), color = 'blue') + ylab("sentiment score") 