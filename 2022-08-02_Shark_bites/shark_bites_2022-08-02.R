
#data found on Data Is Plural:
#https://docs.google.com/spreadsheets/d/1wZhPLMCHKJvwOkP4juclhjFgqIY8fQFMemwKL2c64vk/edit#gid=0

#

# Madeline Riley et al. describe the Australian Shark-Incident Database, which
# contains details about 1,100+ shark bites (and attempted shark bites) between
# 1791 and early 2022, gathered by the Taronga Conservation Society using
# “questionnaires provided to shark-bite victims or witnesses, media reports,”
# and information from state agencies. Read more: “New dataset shows shark bites
# in Australia are increasing and researchers want to know why” (The Guardian).



library(tidyverse)
library(scales)
library(lubridate)
theme_set(theme_light())

activity <- read_csv("https://raw.githubusercontent.com/cjabradshaw/AustralianSharkIncidentDatabase/main/data/activityDat.csv") %>% 
  janitor::clean_names() %>% 
  mutate(recovery_status = str_to_lower(recovery_status))

injuries <- read_tsv("https://raw.githubusercontent.com/cjabradshaw/AustralianSharkIncidentDatabase/main/data/injurydat.txt") %>% 
  janitor::clean_names() %>% 
  mutate(date = ymd(str_c(incident_year, incident_month, incident_day, sep="-")),
         victim_injury = str_to_lower(victim_injury))


activity %>% 
  summary()
activity %>% map_df(~sum(is.na(.)))


#distribution of incidents over time
activity %>% 
  ggplot(aes(incident_year)) +
  geom_histogram()

#most frequent recovery status
activity %>% 
  count(recovery_status, sort = TRUE)

activity %>% 
  count(provoked_unprovoked, sort = TRUE)

activity %>% 
  count(victim_activity, sort = TRUE)


#change in frequencies over time?

activity %>% 
  filter(incident_year>=1900) %>% 
  count(incident_year, recovery_status) %>% 
  complete(incident_year, recovery_status, fill=list(n=0)) %>% 
  mutate(recovery_status=fct_reorder(recovery_status, n, sum)) %>% 
  ggplot(aes(incident_year, n, fill = recovery_status)) +
  geom_area(position="fill") +
  scale_y_continuous(labels = percent)


activity %>% 
  filter(incident_year>=1900,
         !is.na(victim_activity),
         fct_lump(victim_activity, 5) != "Other") %>% 
  count(incident_year, victim_activity) %>% 
  complete(incident_year, victim_activity, fill=list(n=0)) %>% 
  mutate(victim_activity=fct_reorder(victim_activity, n, sum)) %>% 
  ggplot(aes(incident_year, n, fill = victim_activity)) +
  geom_area(position="fill") +
  scale_y_continuous(labels = percent)


##########

#injury data
injuries %>% 
  summary()

injuries %>% map_df(~sum(is.na(.)))

library(gganimate)

injuries %>% 
  ggplot(aes(longitude, latitude, colour = victim_injury)) +
  geom_point(alpha=.3) +
  borders("world", regions="australia") +
  transition_manual(incident_year, cumulative = TRUE) +
  ggthemes::theme_map() +
  labs(title="Year: { current_frame }")


injuries %>% 
  count(state, sort = TRUE)
    
injuries %>% 
  separate_rows(location, sep=", ") %>% 
  count(location, sort = TRUE) 

injuries %>% 
  count(injury_severity, sort = TRUE)

injuries %>% 
  count(victim_injury, sort = TRUE)

#interested in the point way to the west on the map
injuries %>% 
  filter(longitude==min(longitude, na.rm=TRUE))

injuries %>% 
  count(shark_common_name, sort = TRUE)

#shark size
injuries %>% 
  ggplot(aes(shark_length_m)) +
  geom_histogram()
#note that there's peaks at 2m, 3m, 4m, 5m, 6m - suggests a lot of guessing of
#the shark's size


#what's the longest sharks?
injuries %>% 
  top_n(10, shark_length_m) %>% View


#are longer sharks more fatal?
injuries %>% 
  ggplot(aes(shark_length_m, fill = victim_injury)) +
  geom_density(alpha=.5)

injuries %>% 
  filter(!is.na(victim_injury),
         !is.na(shark_length_m)) %>% 
  mutate(victim_injury = fct_reorder(victim_injury, shark_length_m)) %>% 
  ggplot(aes(shark_length_m, victim_injury, color = victim_injury)) +
  geom_boxplot(outlier.shape = NA, varwidth = TRUE) +
  geom_jitter(width=0, alpha = .5)
#longer sharks more likely to kill, shorter sharks more likely to injure, medium
#sharks likely to leave uninjured
#don't tangle with a large shark


#which sharks tend to be the biggest?
library(ggridges)
injuries %>% 
  filter(!is.na(shark_common_name)) %>% 
  add_count(shark_common_name) %>% 
  filter(n > 10) %>% 
  mutate(shark_common_name = fct_reorder(shark_common_name, shark_length_m, na.rm=TRUE)) %>% 
  ggplot(aes(shark_length_m, shark_common_name)) +
  geom_density_ridges()
#note the multiple modes at whole number intervals

#are certain sharks more deadly, controlling for size?
library(effects)
library(emmeans)

fatalities <- injuries %>% 
  filter(!is.na(shark_common_name),
         !is.na(shark_length_m)) %>% 
  add_count(shark_common_name) %>% 
  filter(n > 50) %>% 
  mutate(shark_length_centered = shark_length_m - mean(shark_length_m),
         fatal = victim_injury=="fatal",
         killed_or_injured = victim_injury != "uninjured")

mod_fatal <- fatalities %>% 
  glm(fatal ~ shark_length_m + shark_common_name, data=., family=binomial)

summary(mod_fatal)
plot(predictorEffects(mod_fatal), type="response")
emmeans(mod_fatal, pairwise~shark_common_name)

#why is the CI for wobbegong so wide?
fatalities %>% 
  group_by(shark_common_name) %>% 
  summarise(attacks = n(),
            prob_fatality = mean(fatal),
            median_length = median(shark_length_m),
            min_length = min(shark_length_m),
            max_length = max(shark_length_m))
#there's perfect separation for wobbegong, not one of their attacks was fatal
#wobbegong's tend to be the smallest sharks
#do they ever injure?

fatalities %>% 
  count(shark_common_name, victim_injury)
#they are quite likely to injure (89 were injured in 92 encounters)



#are certain sharks more likely to kill OR injury, controlling for size?
mod_killed_or_injured <- fatalities %>% 
  glm(killed_or_injured ~ shark_length_m + shark_common_name, data=., family=binomial)

summary(mod_killed_or_injured)
plot(predictorEffects(mod_killed_or_injured), type="response")
emmeans(mod_killed_or_injured, pairwise~shark_common_name, type = "response")

#issue with this model: this data set is compiled from reports of encounters
#with sharks, but it's probable that an encounter is more likely to come to
#attention and be reported if it results in injury or death?

library(splines)
#model with ns for shark length
mod_killed_or_injured_ns <- fatalities %>% 
  glm(killed_or_injured ~ ns(shark_length_m, 3) + shark_common_name, data=., family=binomial)

summary(mod_killed_or_injured_ns)
car::Anova(mod_killed_or_injured_ns)
plot(predictorEffects(mod_killed_or_injured_ns), type="response")
emmeans(mod_killed_or_injured_ns, pairwise~shark_common_name, type = "response")
pwpp(emmeans(mod_killed_or_injured_ns, pairwise~shark_common_name, type = "response"))




#are certain sharks more likely to attack at night?
#684 cases are missing time
injuries %>% 
  mutate(time_of_incident = parse_number(time_of_incident)) %>% 
  filter(!is.na(time_of_incident)) %>% 
  ggplot(aes(time_of_incident)) +
  geom_histogram()
#not a lot of attacks occuring before 500 (5am) and after 2000 (10pm), probs bc
#people aren't as likely to swim at night


injuries %>% 
  count(no_sharks, sort = TRUE)
#1 incident involved 10 sharks, yikes!
injuries %>% 
  filter(no_sharks == 10) %>% View
