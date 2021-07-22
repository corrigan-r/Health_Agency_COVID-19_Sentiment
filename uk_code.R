library(dplyr)
library(ggplot2)
library(grid)
library(gtable)
library(lubridate)
library(purrr)
library(readr)
library(rvest)
library(stringr)
library(tidytext)
library(XML)
library(xml2)

# UK - get urls for webpages of news releases
url_uk_page1 <- 
  "https://www.gov.uk/search/all?level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&content_purpose_supergroup%5B%5D=news_and_communications&order=updated-newest"

pages <- seq(1:97) + 1 # sequence represents the remaining pages for the webpage

urls_uk_page2_plus <- 
  paste0("https://www.gov.uk/search/all?content_purpose_supergroup%5B%5D=news_and_communications&level_one_taxon=5b7b9532-a775-4bd2-a3aa-6ce380184b6c&order=updated-newest&page=", pages) # urls for pages 2 through 9

urls_uk <- c(url_uk_page1, urls_uk_page2_plus)

# find the news links for each page
h_uk <- lapply(urls_uk[1:4], function(url){read_html(url)})

nodes <- unlist(lapply(h_uk, function(h) {
  html_nodes(h, '.gem-c-document-list__item-link') %>% html_attr('href')
  }))

urls_uk <- paste0("https://www.gov.uk", nodes)

# huk <- list(rep(NULL,50))
# 
# huk9 <- read_html(urls_uk[9])
# huk10 <- read_html(urls_uk[10])
# huk11 <- read_html(urls_uk[11])
# huk12 <- read_html(urls_uk[12])
# huk13 <- read_html(urls_uk[13])
# huk14 <- read_html(urls_uk[14])
# huk15 <- read_html(urls_uk[15])
# huk16 <- read_html(urls_uk[16])
# huk17 <- read_html(urls_uk[17])
# huk18 <- read_html(urls_uk[18])
# huk19 <- read_html(urls_uk[19])
# huk20 <- read_html(urls_uk[20])
# huk21 <- read_html(urls_uk[21])
# huk22 <- read_html(urls_uk[22])
# huk23 <- read_html(urls_uk[23])
# huk24 <- read_html(urls_uk[24])
# huk25 <- read_html(urls_uk[25])
# huk26 <- read_html(urls_uk[26])
# huk27 <- read_html(urls_uk[27])
# huk28 <- read_html(urls_uk[28])
# huk29 <- read_html(urls_uk[29])
# huk30 <- read_html(urls_uk[30])
# huk31 <- read_html(urls_uk[31])
# huk32 <- read_html(urls_uk[32])
# huk33 <- read_html(urls_uk[33])
# huk34 <- read_html(urls_uk[34])
# huk35 <- read_html(urls_uk[35])
# huk36 <- read_html(urls_uk[36])
# huk37 <- read_html(urls_uk[37])
# huk38 <- read_html(urls_uk[38])
# huk39 <- read_html(urls_uk[39])
# huk40 <- read_html(urls_uk[40])
# huk41 <- read_html(urls_uk[41])
# huk42 <- read_html(urls_uk[42])
# huk43 <- read_html(urls_uk[43])
# huk44 <- read_html(urls_uk[44])
# 
# h_uk <- list(huk1, huk2, huk3, huk4, huk5, huk6, huk7, huk8, huk9, huk10, huk10, huk10, huk10, huk10, huk11, huk12, huk13, huk14, huk15, huk16, huk17, huk18, huk20, huk21, huk22, huk23, huk24, huk25, huk26, huk27, huk27, huk28, huk29, huk30, huk31, huk32, huk33, huk34, huk35, huk36, huk37, huk38, huk39, huk40, huk41, huk42, huk43, huk44)


nodes_releases_uk <- sapply(urls_uk, function(url){# get nodes for each news release
  news <- read_html(url)
  nodes <- news %>% html_nodes(".gem-c-metadata__definition , .govspeak li , .govspeak p")
  if (length(nodes)<3) a <- NULL
  else (nodes)
  })

nodes_releases_uk <- nodes_releases_uk[!sapply(nodes_releases_uk_test, is.null)] # remove the null elements


dates_uk <- lapply(nodes_releases_uk, function(node){ # parse dates for each release (updated releases have date on 3rd node)
    date <- (str_extract(node[3], "\\d{1,2}\\s[A-z]*\\s20\\d{2}"))
    if (is.na(date)) str_extract(node[2], "\\d{1,2}\\s[A-z]*\\s20\\d{2}")
    else (date)#{date <- node[2]}#str_extract(node[2], "\\d{1,2}\\s[A-z]*\\s20\\d{2}")
    }) %>% unlist() %>% unname()

dates_uk <- dmy(dates_uk)

organization <- lapply(nodes_releases_uk_test, function(node){ # parse organization for each release (organization is in 1st node)
  str_extract(node[1], 'organisations/[A-z-]*') %>% 
    str_remove('organisations/')}) %>% unlist() %>% matrix()

organization <- matrix(unlist(organization))

tib_uk <- sapply(urls_uk, function(url){# get nodes for each news release
  news <- read_html(url)
  nodes <- news %>% 
    html_nodes(".gem-c-metadata__definition , .govspeak li , .govspeak p")
  if (length(nodes)<2) a <- NULL
  else (a <- tibble(text = unlist(as_list(nodes))))
})

tib_uk <- (tib_uk[!sapply(tib_uk, is.null)]) # remove the null elements
save(tib_uk, file = "tib_uk.RData")


# #################################
# sentiment analysis
# #################################

finn <- get_sentiments("afinn")
custom_stop_words <- 
  bind_rows(tibble(word = "prick", lexicon = "SMART"), stop_words)

tokens_uk <- lapply(tib_uk, function(release) { # unnest tokens from each news release tibble
  news_unlisted <-unlist(release)
  words <- unnest_tokens(tibble(news_unlisted), word, news_unlisted)
  filtered_words <- words %>% filter(!word %in% custom_stop_words$word) 
  #words <- unnest_tokens(release, word, text)
})


#################################################
sentiment_uk <- Reduce(rbind, tokens_uk)
sentiment_uk <- sentiment_uk %>% inner_join(finn) %>% 
  count(word, sentiment=value, sort = TRUE)
#################################################


sentiment_tokens_uk <- lapply(tokens_uk, function(tok) { # get sentiment values for each release's tokens
  afinn <- tok %>% inner_join(finn)})


sentiment_scores <- lapply(sentiment_tokens_uk, function(tok) { # get sentiment values for each release's tokens
  afinn <- tok %>% inner_join(finn) %>% summarize(sentiment = sum(value))}) # sum sentiment values for each release


sentiment_dat_uk <- data.frame(date = dates_uk, organization, 
                               score = matrix(unlist(sentiment_scores)))

sentiment_dat_uk %>% mutate(week = round_date(date, "week")) %>% # sentiment vs time plot
  group_by(week) %>%  summarize(mean_score = mean(score)) %>% 
  ggplot(aes(week, mean_score)) + geom_smooth(span = 0.5)

#news_sentiment_uk 
sentiment_dat_uk %>% mutate(week = round_date(date, "week")) %>% # weekly throughput plot
  group_by(week) %>% summarize(count = n()) %>% ggplot(aes(week, count)) + 
  geom_col() + xlim(ymd(c("2019-12-01", "2021-07-11")))

news_sentiment_uk 
sentiment_dat_uk %>% group_by(organization) %>% # sentiment by organization
  summarize(count = n()) %>% filter(count > 10) %>% 
  inner_join(sentiment_dat_uk) %>% group_by(organization) %>% 
  summarize(mean_score = mean(score)) %>% 
  mutate(organization = reorder(organization, desc(mean_score))) %>%  
  ggplot(aes(organization, mean_score)) + geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ##################
# retrieve covid data
# ##################

covid_deaths <- read_csv("data_2021-Jul-08.csv") %>% 
  rename(deaths = newDailyNsoDeathsByDeathDate) %>% select(date, deaths) 

covid_cases <- read_csv("data_2021-Jul-14.csv") %>% 
  rename(cases = newCasesByPublishDate) %>% select(date, cases)
#virus_data <- bind_rows(covid_deaths, covid_cases)
sent <- news_sentiment_uk %>% select(date, score) %>% 
  mutate(date = round_date(date, "week")) %>% group_by(date) %>% 
  summarize(mean_score = mean(score))


# ########################################
# stacked plot of covid deaths and sentiment
# ############################################33
library(gtable)
library(grid)
covid_cases_plot <- covid_cases %>% ggplot() + 
  geom_col(aes(date, cases), fill = "lightorange") + 
  xlim(ymd(c("2019-12-01", "2021-07-13")))

covid_deaths_plot <- covid_deaths %>% ggplot() + 
  geom_col(aes(date, deaths), fill = "darkorange2") + 
  xlim(ymd(c("2019-12-01", "2021-07-13")))

sentiment_plot <- news_sentiment_uk %>% mutate(date = round_date(date, "week")) %>% 
  group_by(date) %>% summarize(score = mean(score)) %>% 
  ggplot(aes(date, score)) + geom_smooth(span = .2) + 
  xlim(ymd(c("2019-12-01", "2021-07-13")))

release_rate_plot <- news_sentiment_uk %>% 
  mutate(week = round_date(date, "week")) %>% group_by(week) %>% 
  summarize(count = n()) %>% 
  
ggplot(aes(week, count)) + geom_col() + xlim(ymd(c("2019-12-01", "2021-07-11")))

g1 <- ggplotGrob(covid_cases_plot)
g2 <- ggplotGrob(covid_deaths_plot)
g3 <- ggplotGrob(release_rate_plot)
g4 <- ggplotGrob(sentiment_plot)
g <- rbind(g1, g2, g4)
grid.draw(g)


# ###############################
# words contributing to sentiment
# ###############################

sentiment_uk %>% group_by(sentiment) %>% slice_max(order_by = n, n=5) %>% 
  ungroup() %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment )) + geom_col(show.legend = FALSE) + 
  scale_y_log10() + facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Most comon words contributing to each of the 10 sentiment values", 
       x = NULL) + 
  coord_flip() + scale_fill_gradient2(low = "#F8766D", mid = "grey88", 
                                      high =  "#00BFC5")

# ###############33
# correlation
# ################
dat <- inner_join(covid, sent, "date")
cor(dat$deaths, dat$mean_score)

####################################################################

# original
nodes_releases_uk <- sapply(urls_uk, function(url){
  news <- read_html(url)
  nodes <- news %>% html_nodes(".gem-c-metadata__definition , .govspeak li ,
                               .govspeak p")
  news_list <- as_list(news)
  news_unlisted <- unlist(news_list)
  news <- tibble(news_unlisted)
})


unname(which(dates_uk<ymd("2019-06-06")))
