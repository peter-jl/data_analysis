
#data from:
#https://www.thearda.com/Archive/NatBaylor.asp


#Wave I (2005)
#https://www.thearda.com/Archive/Files/Codebooks/BRS2005_CB.asp
#Baylor University. 2005. The Baylor Religion Survey. Waco, TX: Baylor Institute for Studies of Religion [producer].
#Wave V (2017)
#https://www.thearda.com/Archive/Files/Codebooks/BRS5_CB.asp
#Baylor University. 2017. The Baylor Religion Survey, Wave V. Waco, TX: Baylor Institute for Studies of Religion [producer].


library(tidyverse)
library(readxl)
theme_set(theme_light())



religion_0 <- Hmisc::spss.get("2022-08-08_Baylor_religion_survey/data/Baylor Religion Survey, Wave I (2005).SAV") %>% 
  janitor::clean_names() %>% 
  as_tibble()
religion_1 <- Hmisc::spss.get("2022-08-08_Baylor_religion_survey/data/Baylor Religion Survey, Wave V (2017).SAV") %>% 
  janitor::clean_names() %>% 
  as_tibble()

var_labels_2004 <- map_chr(religion_0, Hmisc::label)
#write_csv(var_labels2, file = "2022-08-08_Baylor_religion_survey/data/varnames_to_recode.csv")
#write names to an excel file, useful for consulting during analysis
var_labels_2017 <- map_chr(religion_1, Hmisc::label)


#matching variable labels in 2004 and 2017 data using stringdist
distmat <- stringdist::stringdistmatrix(var_labels_2004, var_labels_2017)
closest_matches <- distmat %>% 
  as_tibble() %>% 
  select(where(~between(min(.x), 50,60))) %>% #can change this cutoff
  map_dbl(~which.min(.x))
tibble(col_2004=closest_matches,
       name_2004=names(religion_0[closest_matches]),
       lab_2004=var_labels_2004[closest_matches],
       col_2017=names(closest_matches) %>% str_remove("V") %>% as.numeric()) %>% 
  mutate(name_2017=names(religion_1[col_2017]),
         lab_2017=var_labels_2017[col_2017]) %>% 
  View()

distmat <- stringdist::stringdistmatrix(var_labels_2017, var_labels_2004)
closest_matches <- distmat %>% 
  as_tibble() %>% 
  select(where(~between(min(.x), 50,60))) %>% #can change this cutoff
  map_dbl(~which.min(.x))
tibble(col_2004=names(closest_matches) %>% str_remove("V") %>% as.numeric(),
       col_2017=closest_matches,
       name_2017=names(religion_1[closest_matches]),
       lab_2017=var_labels_2017[closest_matches]) %>% 
  mutate(name_2004=names(religion_0[col_2004]),
         lab_2004=var_labels_2004[col_2004], 
         .after=col_2004
         ) %>% 
  View()



vars_2004 <- c("q1", "q5", "q8", "q9", "q11", "q13a", "q13b", "q13c", "q15", 
               "q16", "q20", "q21", "q23b", "q23c", "q23d", "q23f", "q23m", 
               "q23o", "q33", "q39d", "q39l", "q39m", "q41a", "q43c", "q43e", 
               "q43i", "q46a", "q50a", "q53", "q54", "q56", "q58", "q62", "q63", 
               "q65", "i_race", "i_religion", "i_age", "i_educ")
vars_2017 <- c("q1", "q4", "q7", "q8", "q9", "q14a", "q14b", "q14c", "q11", 
               "r14", "q17", "q18", "q20b", "q20j", "q20i", "q20g", "q20d", 
               "q20f", "r25", "mp12b", "mp12d", "mp12f", "mp12e", "q30c", "q30e", 
               "mp8a", "q31", "q36a", "q79", "rachisp2", "q93", "d9", "q83", 
               "d14", "q85", "i_race", "i_religion", "i_age", "i_educ")

new_var_names <- c("religion", "serv_attnd_frequency", "serv_attnd_years", "serv_attnd_num_people", 
                   "serv_money_contrib", "friends_same_service", "friends_different_service", 
                   "friends_no_service", "time_reading_sacred_book", "time_meditation", 
                   "belief_bible", "belief_God", "god_critical", "god_distant", 
                   "god_ever_present", "god_forgiving", "god_punishing", "god_wrathful", 
                   "serv_attnd_age12", "govt_advocate_christian_values", "govt_relig_public_spaces", 
                   "govt_prayer_public_schools", "god_favours_america", "pres_elect_donate_money", 
                   "pres_elect_attend_rally", "pres_elect_watch_speeches", "political_affil", 
                   "trust_people", "us_citizen", "hispanic", "kids_under_18_in_household", 
                   "marital_status", "hours_worked_last_week", "employment", "employer_locally_owned", 
                   "race", "religion_tradition", "age", "education")

tibble(
  new_var_names,
  labs2004=var_labels_2004[vars_2004],
  labs2017=var_labels_2017[vars_2017]
) %>% View()

###########

#combine datasets with common variables
religion_0_renamed <- religion_0 %>% 
  select(one_of(vars_2004)) %>% 
  set_names(new_var_names) %>% 
  mutate(year="y2004")
religion_1_renamed <- religion_1 %>% 
  select(one_of(vars_2017)) %>% 
  set_names(new_var_names) %>% 
  mutate(year="y2017")

#full dataset
religion_full <- rbind(religion_0_renamed, 
                       religion_1_renamed) %>% 
  mutate(religion_tradition=fct_collapse(religion_tradition,
                                         none=c("None", "No Religion"),
                                         protestant=c("Protestant", "Evangelical Protestant", "Mainline Protestant", "Black Protestant"),
                                         other=c("Other", "Jewish or other religions")),
    belief_bible=fct_collapse(belief_bible,
                              literally_word_for_word=c("It means exactly what it says/should be taken literally", "The Bible means exactly what it says. It should be taken literally, word-for-word, on all subjects"),
                              perfect_not_taken_literally=c("It is perfectly true, should not be taken literally", "The Bible is perfectly true, but it should not be taken literally, word-for-word. We must interpret its meaning"),
                              human_error=c("The Bible contains some human error"), 
                              book_of_history_legends=c("The Bible is an ancient book of history and legends"),
                              dont_know=c("I don't know")
                              ))


###############


religion_full %>% 
  filter(!is.na(religion_tradition)) %>% 
  ggplot(aes(year, fill=religion_tradition)) +
  geom_bar(position="fill")

religion_full %>% 
  filter(!is.na(belief_bible)) %>% 
  ggplot(aes(year, fill=belief_bible)) +
  geom_bar(position="fill")

religion_full %>% 
  filter(!is.na(belief_God)) %>% 
  ggplot(aes(year, fill=belief_God)) +
  geom_bar(position="fill")

