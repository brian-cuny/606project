library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(psych)
library(ggrepel)
library(scales)
library(directlabels)

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

summary.box.data <- box.data %>%
  group_by(period) %>%
  summarize(m = mean(goals)) %>%
  rename(period2 = period)

box.data %>%
  mutate(period=paste('Period ', period)) %>%
  ggplot() +
  geom_bar(aes(x=goals, y=(..count../sum(..count..)), fill=period))  +
  scale_x_discrete(limits=0:7, breaks=0:7) + 
  scale_y_continuous(limits=c(0, 0.12), breaks=seq(0, 0.12, 0.02), expand=c(0, 0), labels=percent) +
  facet_wrap(~period, ncol=1) +
  labs(x=NULL,
       y=NULL,
       title='Proportion of Goals Scored by Period') +
  theme_bw() + 
  theme(legend.position='None',
        strip.background=element_rect(fill='grey70'),
        strip.text=element_text(color='black', size=12),
        axis.text=element_text(size=10),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank())


ggplot(box.data) +
  geom_histogram(aes(x=goals, y=..count../sum(..count..), fill=period), bins=8, show.legend=FALSE)  +
  scale_x_discrete(limits=0:7, breaks=0:7) + 
  scale_y_continuous(limits=c(0, 0.12), breaks=seq(0, 0.12, 0.02), expand=c(0, 0), labels=percent) +
  facet_wrap(~period, ncol=1) +
  labs(x=NULL,
       y=NULL,
       title='Proportion of Goals Scored by Period') +
  theme_bw() + 
  theme(legend.position='None',
        strip.background=element_rect(fill='grey70'),
        strip.text=element_text(color='black', size=12),
        axis.text=element_text(size=10),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank())

ggplot(box.data) +
  geom_density(aes(x=goals, fill=period, color=period), alpha=0.2, adjust=3)  +
  geom_vline(data=summary.box.data, aes(xintercept=m, color=period2)) +
  scale_x_discrete(limits=0:7, breaks=0:7, expand=c(0, 0)) + 
  scale_color_discrete(breaks=NULL) + 
  scale_y_continuous(expand=c(0,0,.01,.01)) +
  labs(x='Goals', 
       y='Proportion of Goals Scored',
       fill='Period',
       title='Proportion of Goals Scored') +
  guides(fill=guide_legend(ncol=3, label.position='top')) +
  theme_bw() + 
  theme(legend.position=c(.9, .8), 
        legend.justification=c(1, 1), 
        legend.background=element_rect(color='darkblue'),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        legend.title.align=0.5,
        legend.text.align=0.5,
        legend.key.size=unit(0.8, 'cm'),
        axis.text=element_text(size=12))


ggplot(box.data) +
  geom_violin(aes(x=period, y=goals, fill=period), show.legend=FALSE, adjust=3)  +
  scale_y_discrete(limits=0:7, breaks=0:7, expand=c(0, 0, .05, .05)) +
  labs(x='Period', 
       y='Goals Scored',
       fill='Period',
       title='Number of Goals Scored by Period') +
  scale_fill_brewer(palette='Pastel1') +
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(),
        panel.background=element_rect(color='black'),
        axis.line.x=element_line(color='black'),
        axis.text=element_text(size=12))

ggplot(box.data, aes(fill=factor(goals), group=goals)) +
  geom_bar(aes(x=period), position=position_fill(reverse=TRUE)) +
  scale_fill_brewer(palette = 'YlOrRd') +
  scale_y_continuous(labels=percent_format(),
                     expand=c(0, 0)) +
  labs(fill='Goals',
       x='Period',
       y=NULL,
       title='Makeup of Scoring by Period') + 
  guides(fill=guide_legend(reverse=TRUE))


box.data %>%
  group_by(period) %>%
  count(a = goals %in% max(goals),
        b = max(goals)) %>%
  filter(a == TRUE)
