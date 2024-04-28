
library(tidyverse)
library(scales)
theme_set(theme_light())

#data from Data is Plural - Structured Archive google sheet, https://docs.google.com/spreadsheets/d/1wZhPLMCHKJvwOkP4juclhjFgqIY8fQFMemwKL2c64vk/edit#gid=0
#edition 2020.10.28, position 5
witch_trials <- read_csv("https://raw.githubusercontent.com/JakeRuss/witch-trials/master/data/trials.csv") %>% 
  rename(country = gadm.adm0,
         region = gadm.adm1,
         subregion = gadm.adm2)

witch_trials %>% 
  summary()

#many records are missing the year of the trials whereas decade is present for
#all records, so it's best to analyze these data at the level of decades.
#all records have country, and 159 out of 10,940 records are missing region, so
#there's possibility of regional analysis
map_df(witch_trials, ~sum(is.na(.)))
witch_trials |> map_int(\(col) sum(is.na(col))) |> bind_rows() #new syntax
#931 missing year
#3826 missing deaths
#5213 missing city
#1047 missing subregion
#159 missing region
#5803 missing lon and lat

#test


#summarize overall number of witches tried, deaths
#over 43,000 tried for witchcraft in europe, over 16,000 deaths
#note that the sum of deaths is only for valid records, 3826 records are missing that info
witch_trials |> 
  summarise(tot_tried = sum(tried),
            tot_deaths = sum(deaths, na.rm = TRUE))




#for each country, what proportion of records have info about deaths?
#France and UK relatively high, both ~75%, whereas Germany only 42%
#Switzerland at 93%, potential for regional comparison of death rates?
witch_trials |> 
  group_by(country) |> 
  summarise(deaths_valid = sum(!is.na(deaths)),
            tried_valid = sum(!is.na(tried)),
            prop_deaths_tried_valid = deaths_valid / tried_valid) |> 
  ungroup() |> 
  arrange(desc(tried_valid))

#Switzerland has 93% valid death data to tried data ratio, good chance to look
#into regional variation in % trials where witches were sentenced to death
#Seems like Vaud had high % death, more western regions like Geneve, Neuchatel,
#and Lucerne had low % death
witch_trials |> 
  filter(country == "Switzerland", 
         !is.na(deaths),
         !is.na(region)) |> 
  group_by(region) |> 
  summarise(tot_tried = sum(tried),
            tot_death = sum(deaths),
            prop_death = tot_death / tot_tried) |> 
  arrange(desc(prop_death)) |> 
  print(n = 26)



#which trials had the most individuals on trial?
#looks like trials in Vaud in the 1600s had 500 on trial, but this looks like it
#was for the entire decade of e.g., 1610, 1620, etc., not individual trials
witch_trials |> 
  slice_max(tried, n = 10)

#how about looking at only records with a valid year?
#this looks more interesting, but even so, are these individuals who were tried
#in multiple trials over the year, or multiple individuals per trial?
witch_trials |> 
  filter(!is.na(year)) |> 
  slice_max(tried, n = 10)



#basic line plot of number of witches tried over time
witch_trials %>% 
  count(decade, wt = tried, name = "tried") %>% 
  ggplot(aes(decade, tried)) +
  geom_line()

witch_trials %>% 
  ggplot(aes(decade)) +
  geom_histogram(binwidth=10)

witch_trials %>% 
  ggplot(aes(decade)) +
  stat_ecdf()


#breakdown by countries
countries <- unique(witch_trials$country) %>% c("UK")

#point map of witch trials
witch_trials %>% 
  ggplot(aes(lon, lat, colour = country)) +
  geom_point(alpha=.4, aes(size = tried)) +
  borders("world", regions = countries) +
  ggthemes::theme_map() +
  theme(legend.position = "top") +
  coord_equal()

#number of witches tried by country
witch_trials |> 
  count(country, wt = tried, sort = TRUE)


library(gganimate) 
library(gifski)

#animated dot plot
anim1 <- witch_trials %>% 
  # mutate(decade = map(decade, ~seq(., 1850, by = 10))) %>% 
  # unnest(decade) %>% #don't need these 2 lines, just use cumulative=TRUE below
  ggplot(aes(lon, lat,colour=country)) +
  geom_point(alpha = .1, aes(size = tried)) +
  borders("world", regions = countries) +
  ggthemes::theme_map() +
  theme(legend.position = "none") +
  transition_manual(decade, cumulative = TRUE) +
  labs(title = "Witch trials in decade: { current_frame }") 
animate(anim1, renderer = gifski_renderer())
#there's so many data points with lon and lat missing, probably better to do a choropleth instead


anim2 <- witch_trials %>% 
  filter(!is.na(deaths)) %>%
  #mutate(decade = map(decade, ~seq(., 1850, by = 10))) %>% 
  #unnest(decade) %>% 
  ggplot(aes(lon, lat,colour=country, size = tried)) +
  geom_point() +
  borders("world", regions = countries) +
  ggthemes::theme_map() +
  theme(legend.position = "none") +
  transition_manual(decade) +
  labs(title = "Witch trials in decade: { current_frame }")
animate(anim2, fps = 5, renderer = gifski_renderer())


#heatmap of number tried by country and by century
witch_trials %>% 
  #filter(!is.na(deaths)) %>% 
  #filter(deaths>0) %>% 
  count(century, country = country, wt = tried) %>% 
  mutate(country = fct_reorder(country, n, sum)) %>% 
  ggplot(aes(century, country, fill = n)) +
  geom_tile() + 
  scale_x_continuous(breaks = seq(1300, 1800, by=100)) +
  scale_fill_continuous(trans="log10", labels = comma) +
  labs(x = "Century",
       y = "Country",
       title = 'Number of alleged witches tried by country and century')
#UK has many cases that don't appear in the above map

by_country <- witch_trials %>% 
  group_by(country) %>% 
  summarise(trials = n(),
            tot_tried = sum(tried, na.rm = TRUE),
            tot_deaths = sum(deaths, na.rm = TRUE),
            n_death = sum(!is.na(deaths)), 
            tot_tried_d = sum(tried * (!is.na(deaths)), na.rm=TRUE),
            death_rate = n_death/tot_tried_d,
            tot_regions = n_distinct(region, na.rm = TRUE),
            regions = paste(unique(region), collapse = ", "), 
            tot_sources = n_distinct(record.source, na.rm = TRUE), 
            sources = paste(unique(record.source), collapse = ", "))

#for countries with >100 deaths, show # of trials by country, with # tried as
#size of circle, and death rate as shade
#death rate is iffy, might be more likely to have recorded trials which result in death?
by_country %>% 
  filter(n_death > 100) %>% 
  mutate(country = fct_reorder(country, trials)) %>% 
  ggplot(aes(trials, country, size = tot_tried_d+1, color = death_rate)) +
  geom_point() +
  scale_size_continuous(labels = comma) +
  scale_colour_gradient2(low = "blue", high = "red",
                         midpoint = .5, labels = percent) +
  scale_x_log10(labels = comma) +
  labs(x = "Number of trials (log)",
       y = NULL,
       size = "Number tried",
       colour = "Death Rate",
       title = "Witch trials by country",
       subtitle = "Number tried only includes trials with death info")




by_decade_country <- witch_trials %>% 
  group_by(country, decade) %>% 
  summarise(trials = n(),
            tot_tried = sum(tried, na.rm = TRUE),
            tot_deaths = sum(deaths, na.rm = TRUE),
            n_death = sum(!is.na(deaths)), 
            tot_tried_d = sum(tried * (!is.na(deaths)), na.rm=TRUE),
            death_rate = n_death/tot_tried_d,
            tot_regions = n_distinct(region, na.rm = TRUE),
            regions = paste(unique(region), collapse = ", "), 
            tot_sources = n_distinct(record.source, na.rm = TRUE), 
            sources = paste(unique(record.source), collapse = ", ")) %>% 
  ungroup()

# No of people tried as witches overall, by country
by_decade_country |> 
  group_by(country) |> 
  summarise(tot_tried = sum(tot_tried)) |> 
  arrange(desc(tot_tried))

#faceted line plots for each country
#only top 9 countries in terms of number of people tried as witches
#witch trials in Spain abruptly stopped around 1610 - why???
by_decade_country %>% 
  filter(fct_lump(country, 9, w = tot_tried, ties.method = "first")!="Other") %>% 
  mutate(country = fct_reorder(country, tot_tried, sum, .desc = TRUE)) %>% 
  ggplot(aes(decade, tot_tried, group = country)) +
  geom_line() +
  facet_wrap(~ country)

#stacked area plot of top 5 countries + other
#could arrange the countries better
by_decade_country |> 
  mutate(country = fct_lump(country, 5, w = tot_tried, ties.method = "first"),
         country = fct_reorder(country, tot_tried, sum)) |> 
  ggplot(aes(decade, tot_tried, fill = country)) +
  geom_area()

#filled area plot of top 5 countries + other
by_decade_country |> 
  mutate(country = fct_lump(country, 5, w = tot_tried, ties.method = "first"),
         country = fct_reorder(country, tot_tried, sum)) |> 
  ggplot(aes(decade, tot_tried, fill = country)) +
  geom_area(position = "fill")

#witch trials contagion across countries?
#i.e., order countries by the median decade of all decades in which they had witch trials
library(ggridges)
witch_trials %>% 
  #only include countries with trials over 25 decades
  #add_count(country) %>% 
  #filter(n > 25) %>% 
  mutate(country = fct_reorder(country, decade, .desc = TRUE)) %>% 
  ggplot(aes(decade, country, fill = country)) +
  geom_density_ridges() +
  scale_x_continuous(breaks = seq(1300, 1900, 100)) +
  theme(legend.position = "none") +
  labs(y = NULL,
       x = "Decade",
       title = "Waves of witch hunts in Europe")

#estimated total number of deaths
witch_trials %>% 
  summarise(deaths_recorded= sum(deaths, na.rm = TRUE),
            pct_missing=mean(is.na(deaths)), 
            estimated_total = mean(deaths, na.rm=TRUE)*n())
#25,117 estimated total deaths

witch_trials %>% 
  summarise(trials_deaths = sum(!is.na(deaths) & deaths>0), 
            pct_missing = mean(!is.na(deaths & deaths>0)),
            estimated_total = mean(deaths>0, na.rm=TRUE)*n())
#2,684 trials ended with death(s)

#from wikipedia:
#Current scholarly estimates of the number of people who were executed for
#witchcraft vary from about 35,000 to 50,000.[a] The total number of witch
#trials in Europe which are known to have ended in executions is around
#12,000.[69] Prominent contemporaneous critics of witch-hunts included
#Gianfrancesco Ponzinibio (fl. 1520), Johannes Wier (1515–1588), Reginald Scot
#(1538–1599), Cornelius Loos (1546–1595), Anton Praetorius (1560–1613), Alonso
#Salazar y Frías (1564–1636), Friedrich Spee (1591–1635), and Balthasar Bekker
#(1634–1698).[70] Among the largest and most notable of these trials were the
#Trier witch trials (1581–1593), the Fulda witch trials (1603–1606), the
#Würzburg witch trial (1626–1631) and the Bamberg witch trials
#(1626–1631).[citation needed]




witch_trials %>% 
  count(region, sort = TRUE)
#scotland has the most trials listed

witch_trials_scotland <- witch_trials %>% 
  filter(region=="Scotland")

witch_trials_scotland %>% 
  map_df(~sum(is.na(.)))
#Scotland has 3,129 trials listed, but no lon or lat data, and no city data
witch_trials_scotland %>%
  count(record.source, subregion, wt = tried, sort = TRUE)
#many of those tried do not have subregion data
#all but one trial comes from Goodare et al (2003)
witch_trials_scotland %>% 
  count(decade, wt = tried)


witch_trials %>% 
  count(subregion, sort = TRUE)


witch_trials %>% 
  filter(region=="Scotland") %>% 
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
  count(city, sort = TRUE)

witch_trials %>% 
  filter(country == "Netherlands") %>% 
  count(city, sort = TRUE)





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
  #filter top 9 countries only
  filter(fct_lump(country, 9)!="Other") %>% 
  group_by(country, decade) %>% 
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



witch_trials %>% 
  top_n(10, tried)
# seems like Bader (1945) states that Vaud in Switzerland tried and killed 500
# witches for eac decade from 1610 to 1650, how reliable is this data?
# likewise, Moeller (2007) states that 403 witches were tried for each decade
# from 1600 to 1620; i'm guessing that the text listed 1209 trials from 1600 to
# 1629 or so, and whoever compiled this data set just averaged it out to 403 for
# each decade


