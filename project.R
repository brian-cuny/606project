library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(psych)
library(ggrepel)
library(scales)
library(directlabels)

load('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\606\\606project\\nhl.RData')

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
  summarize(n = n(),
            mean = mean(shots),
            sd = sd(shots),
            median = median(shots),
            min = min(shots),
            max = max(shots),
            range = max - min)

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
  group_by(period) %>%
  summarize(n = n(),
            mean = mean(goals),
            sd = sd(goals),
            median = median(goals),
            min = min(goals),
            max = max(goals),
            range = max - min)

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

aov(goals ~ period, box.data) %>%
  summary()
# explore individual data -------------------------------------------------


# regression fit ----------------------------------------------------------

box.data %<>%
  mutate(period = factor(period))

fit <- lm(goals ~ shots + period, data=box.data)

summary(fit)

#goals = 0.066448*shots + 0.285678*period2 + 0.472701*period3 + 0.185791


coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

ggplot(box_data, aes(x=shots, y=goals, color=factor(period))) +
  geom_jitter(aes(shape=factor(period)), alpha=0.5, show.legend=FALSE) +
  geom_smooth(method='lm') +
  labs(x='Shots',
       y='Goals',
       color='Period',
       title='Multiple Regression of Goals ~ Shots + Period') +
  theme_bw() + 
  theme(legend.position = c(0.1, 0.9),
        legend.title.align = 0.5,
        legend.background = element_rect(color='black', fill='grey95'),
        legend.text = element_text(size=20),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        axis.ticks.length = unit(0.3, 'cm')
        ) +
  guides(color = guide_legend(ncol=3,
         label.position = 'top',
         label.hjust = 0.5)
  )

# regression fit ----------------------------------------------------------


# precondition check ------------------------------------------------------


#The residuals of the model are nearly normal
data.frame(r=fit$residuals) %>%
ggplot() +
  geom_histogram(aes(r, ..density..), bins=35) +
  stat_function(fun=dnorm, args=list(mean=mean(fit$residuals), sd=sd(fit$residuals)), color='red', size=2) +
  labs(x='Residuals',
       y='Density',
       title='Residuals are Nearly Normal')

#The variability of the residuals is nearly constant
data.frame(fitted = cut(fit$fitted.values, breaks=6), resid = abs(fit$residuals)) %>%
  ggplot(aes(fitted, resid)) +
  geom_jitter(shape=1, alpha=1/5)
  #geom_violin() +
  labs(x='Fitted Values',
       y='Absolute Value Residuals',
       title='Variability of Residuals is Nearly Constant')

#The residuals are independent
data.frame(order=1:3690, resid=fit$residuals) %>%
  ggplot(aes(order, resid)) +
  geom_point() +
  geom_hline(yintercept=0, color='yellow') +
  labs(x='Order of Collection',
       y='Residuals',
       title='Residuals are Independent')

#Each variable is linearly related to the outcome
resid.box.data <- box.data %>%
  mutate(resid = fit$residuals)

ggplot(resid.box.data, aes(shots, resid)) +
  geom_jitter(shape=1, alpha=1/5) +
  scale_x_continuous(labels=seq(3, 43, 5), limits=c(3, 43), breaks=seq(3, 43, 5)) +
  labs(x='Shots',
       y='Residuals')

ggplot(resid.box.data, aes(period, resid)) +
  geom_boxplot(aes(group=period)) +
  geom_point(shape=1, alpha=1/5)


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



# simulation --------------------------------------------------------------

simulation.data <- box.data %>%
  summarize(n = n(),
            mean = mean(goals),
            sd = sd(goals),
            median = median(goals),
            min = min(goals),
            max = max(goals),
            range = max - min)

set.seed(200)

sim.data <- rnorm(1230, mean=simulation.data$mean, sd=simulation.data$sd)

ggplot(data=data.frame(x=c(-4, 7)), aes(x)) +
  stat_function(fun=dnorm, args=list(mean=simulation.data$mean, sd=simulation.data$sd)) +
  stat_function(fun=dnorm, args=list(mean=simulation.data$mean, sd=simulation.data$sd), xlim=c(1.91, 7), geom='area') +
  geom_vline(xintercept=simulation.data$mean) +
  labs(x='Goals',
       y='Proportion',
       title='Simulation of 3rd Period Goals')

pnorm(q=1.91, mean=simulation.data$mean, sd=simulation.data$sd, lower.tail=FALSE)

# simulation --------------------------------------------------------------





















