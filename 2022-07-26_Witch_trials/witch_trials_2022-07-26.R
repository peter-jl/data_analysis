
library(tidyverse)
library(scales)
theme_set(theme_light())

#data from Data is Plural - Structured Archive google sheet, https://docs.google.com/spreadsheets/d/1wZhPLMCHKJvwOkP4juclhjFgqIY8fQFMemwKL2c64vk/edit#gid=0
#edition 2020.10.28, position 5
witch_trials <- read_csv("https://raw.githubusercontent.com/JakeRuss/witch-trials/master/data/trials.csv")

witch_trials %>% 
  summary()

witch_trials %>% 
  ggplot(aes(decade)) +
  geom_histogram(binwidth=10)


witch_trials %>% 
  ggplot(aes(decade)) +
  stat_ecdf()

countries <- unique(witch_trials$gadm.adm0) %>% c("UK")

witch_trials %>% 
  ggplot(aes(lon, lat, colour=gadm.adm0)) +
  geom_point() +
  borders("world", regions = countries) +
  ggthemes::theme_map() +
  theme(legend.position = "top")

library(gganimate)

witch_trials %>% 
  filter(!is.na(decade)) %>%
  mutate(decade = map(decade, ~seq(., 1850, by = 10))) %>% 
  unnest(decade) %>% 
  ggplot(aes(lon, lat,colour=gadm.adm0)) +
  geom_point(alpha = .1) +
  borders("world", regions = countries) +
  ggthemes::theme_map() +
  theme(legend.position = "none") +
  transition_manual(decade) +
  labs(title = "Witch trials in decade: { current_frame }")


witch_trials %>% 
  count(city, sort = TRUE)

witch_trials %>% 
  filter(gadm.adm0 == "Netherlands") %>% 
  count(city, sort = TRUE)

witch_trials %>% 
  count(gadm.adm2, sort = TRUE)


witch_trials %>% 
  mutate(decade_trunc = decade - century) %>% 
  count(century, decade_trunc) %>% 
  ggplot(aes(century, decade_trunc, fill = n)) +
  geom_tile() + 
  geom_text(aes(label = comma(n)), check_overlap = TRUE) +
  scale_y_continuous(breaks = seq(0,90, by=10), label = glue::glue("{seq(0,90, by=10)}s")) +
  scale_x_continuous(breaks = seq(1300,1800, by=100)) +
  scale_fill_continuous(trans="log10", labels = comma) +
  labs(x = "Century",
       y = "Decade",
       title = "Witch trials by century and decade")


witch_trials %>% 
  #filter(!is.na(deaths)) %>% 
  #filter(deaths>0) %>% 
  count(century, country = gadm.adm0, wt = tried) %>% 
  mutate(country = fct_reorder(country, n, sum)) %>% 
  ggplot(aes(century, country, fill = n)) +
  geom_tile() + 
  scale_x_continuous(breaks = seq(1300, 1800, by=100)) +
  scale_fill_continuous(trans="log10", labels = comma) +
  labs(x = "Century",
       y = "Country",
       title = 'Number of alleged "witches" tried by country and century')


witch_trials %>% 
  arrange(desc(deaths))

witch_trials %>% 
  count(record.source, wt=tried, sort = TRUE)


witch_trials %>% 
  group_by(record.source) %>% 
  summarise(trials = n(),
            num_tried = sum(tried, na.rm=TRUE)) %>% 
  arrange(desc(trials))

witch_trials %>% 
  filter(fct_lump(gadm.adm0, 9)!="Other") %>% 
  group_by(country = gadm.adm0, decade) %>% 
  summarise(num_trials = n(),
            tot_tried = sum(tried, na.rm = TRUE),
            tot_deaths = sum(deaths, na.rm = TRUE)) %>% 
  ungroup() %>% 
  complete(country, decade, fill = list (num_trials = 0)) %>% 
  mutate(country = fct_reorder(country, num_trials, sum, .desc = TRUE)) %>% 
  ggplot(aes(decade, num_trials, group = country, colour = country)) +
  geom_line() +
  facet_wrap(~country, scales = "free_y") +
  scale_colour_discrete(guide = "none")
