library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(psych)
library(ggrepel)
library(scales)
library(directlabels)

#Is there a relationship between the number of shots taken by both teams, the period and the number of goals scored?
#goals ~ shots + period


# data collection ---------------------------------------------------------
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
  map_df(~type.convert(.)) %>%
  mutate(goals = awayScore + homeScore,
         shots = awayShots + homeShots) %>%
  select(-c(3:6)) %>%
  rename(id=game.ID, period=`@number`) %>%
  filter(period %in% 1:3) %>%
  mutate(id = as.character(id),
         period = as.factor(period)) %>%
  as.tibble()
# data collection ---------------------------------------------------------


# explore individual data -------------------------------------------------
box.data %>%
  group_by(period) %>%
  summarize(min = min(shots),
            max = max(shots))

box.data %>%
  mutate(period = paste0('Period ', period)) %>%
  ggplot() +
  geom_histogram(aes(shots, fill=period), bins=37) +
  facet_wrap(~period, ncol=1) +
  labs(x=NULL,
       y=NULL,
       title='Frequency of Shots by Period') +
  scale_x_continuous(limits=c(0, 40), breaks=seq(0, 40, 5)) +
  theme_bw() + 
  theme(legend.position='None',
        strip.background=element_rect(fill='grey70'),
        strip.text=element_text(color='black', size=12),
        axis.text=element_text(size=10),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank())

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
# explore individual data -------------------------------------------------


# regression fit ----------------------------------------------------------

fit <- lm(goals ~ shots + period, data=box.data)

summary(fit)

#goals = 0.066448*shots + 0.285678*period2 + 0.472701*period3 + 0.185791
#shots range 7-40

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# regression fit ----------------------------------------------------------


# precondition check ------------------------------------------------------

ggplot() +
  geom_point(aes(1:3690, fit$residuals)) +
  geom_hline(yintercept=0, color='yellow')

#3 more

# precondition check ------------------------------------------------------




box.data %>%
  filter(period == '1') %>%
  select(goals) %>%
  describe()

box.data %>%
  filter(period == '2') %>%
  select(goals) %>%
  as.matrix() %>%
  describe()

box.data %>%
  filter(period == '3') %>%
  describe()









