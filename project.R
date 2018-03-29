set.seed(10000)
library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(psych)

games.data <- GET('https://api.mysportsfeeds.com/v1.2/pull/nhl/2015-2016-regular/full_game_schedule.json', authenticate('njpsy', 'asdfasdf'), 
            add_headers('Content-Type'='application/json', 'Accept-Encoding'='gzip')) %>%
  content(as='text', encoding='UTF-8') %>%
  fromJSON(flatten=TRUE) %>%
  extract2(1) %>%
  extract2(2) %>%
  as.tibble()

game.dates <- games.data %>%
  select(6) %>%
  mutate(date=str_replace(date, '(\\d+)-(\\d+)-(\\d+)', '\\1\\2\\3')) %>%
  unique()

API.Query <- function(date){
  Sys.sleep(1)
  GET('https://api.mysportsfeeds.com/v1.2/pull/nhl/2015-2016-regular/scoreboard.json', authenticate('njpsy', 'asdfasdf'), 
      add_headers('Content-Type'='application/json', 'Accept-Encoding'='gzip'),
      query=list('fordate'=date)) %>%
    content(as='text', encoding='UTF-8') %>%
    fromJSON(flatten=TRUE) %>%
    extract2(1) %>%
    extract2(2) %>%
    as.tibble()
}

box.data <- game.dates$date %>%
  map_df(~API.Query(.)) %>%
  select(8, 24) %>%
  unnest() %>%
  mutate(goals = awayScore %>% as.numeric() + homeScore %>% as.numeric()) %>%
  select(-c(3:6)) %>%
  setNames(c('game.ID'='id', '@number'='period', 'goals'='goals')) %>%
  filter(period %in% 1:3) %>%
  as.tibble()


box.data %>%
  filter(period == '1') %>%
  select(goals) %>%
  describe()

box.data %>%
  filter(period == '2') %>%
  select(goals) %>%
  describe()

box.data %>%
  filter(period == '3') %>%
  select(goals) %>%
  describe()

ggplot(box.data) +
  geom_bar(aes(x=goals, y=(..count../sum(..count..)), fill=period))  +
  facet_wrap(~period, nrow=3, ncol=1) +
  labs(x='Goals', 
       y='Proportion of Goals Scored',
       fill='Period',
       title='Proportion of Goals Scored')







