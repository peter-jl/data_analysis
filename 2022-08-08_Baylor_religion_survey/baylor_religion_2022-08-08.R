
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
library(survey)
theme_set(theme_light())



# read in data, view variables and labels ---------------------------------

religion_0 <- Hmisc::spss.get("2022-08-08_Baylor_religion_survey/data/Baylor Religion Survey, Wave I (2005).SAV") %>% 
  janitor::clean_names() %>% 
  as_tibble()
religion_1 <- Hmisc::spss.get("2022-08-08_Baylor_religion_survey/data/Baylor Religion Survey, Wave V (2017).SAV") %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(h5a_num=as.numeric(h5a) - 1, .after=h5a) %>%  #factor levels from 1-11, but scale goes from 0-10
  mutate(h6_hr_num=as.numeric(as.character(h6_hr)), .after=h6_hr)


#view variable labels, religion0
religion_0_labels <- haven::read_sav("2022-08-08_Baylor_religion_survey/data/Baylor Religion Survey, Wave I (2005).SAV")
religion_0_labels %>% 
  map_chr(attr, "label") %>% 
  enframe() %>% 
  View("variables0")
religion_0_labels %>% 
  map(attr, "labels") %>% 
  compact() %>% 
  map_df(~data.frame(labels=names(.x),
                     values=as.character(.x)), .id="name") %>% 
  View("values0")

#view variable labels, religion1
religion_1_labels <- haven::read_sav("2022-08-08_Baylor_religion_survey/data/Baylor Religion Survey, Wave V (2017).SAV")
religion_1_variables <- religion_1_labels %>% 
  map_chr(attr, "label") %>% 
  enframe() %>% 
  mutate(col=row_number(),
         var = names(religion_1), .before=name)
religion_1_variables%>% 
  View("variables1")
religion_1_labels %>% 
  map(attr, "labels") %>% 
  compact() %>% 
  map_df(~data.frame(labels=names(.x),
                     values=as.character(.x)), .id="name") %>% 
  left_join(religion_1_variables, by="name") %>% 
  View("values1")



# finding variables that match and combining datasets ---------------------

var_labels_2004 <- map_chr(religion_0, Hmisc::label)
#write_csv(var_labels2, file = "2022-08-08_Baylor_religion_survey/data/varnames_to_recode.csv")
#write names to an excel file, useful for consulting during analysis
var_labels_2017 <- map_chr(religion_1, Hmisc::label)


#matching variable labels in 2004 and 2017 data using stringdist
view_similar <- function(df1=religion_0, df2=religion_1, distmat, lcut=50, hcut=60){
  var_labels1 <- map_chr(df1, Hmisc::label)
  var_labels2 <- map_chr(df2, Hmisc::label)
  distmat <- stringdist::stringdistmatrix(var_labels1, var_labels2)
  closest_matches <- distmat %>% 
    as_tibble() %>% 
    select(where(~between(min(.x), lcut,hcut))) %>% #can change this cutoff
    map_dbl(~which.min(.x))
  tibble(col_1=closest_matches,
         name_1=names(df1[closest_matches]),
         lab_1=var_labels1[closest_matches],
         col_2=names(closest_matches) %>% str_remove("V") %>% as.numeric()) %>% 
    mutate(name_2=names(df2[col_2]),
           lab_2=var_labels2[col_2]) %>% 
    View()
}

view_similar(religion_0, religion_1, 10, 20)
view_similar(religion_1, religion_0, 10, 20)



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
#39 variables that pretty closely matched



#combine datasets with common variables
religion_0_renamed <- religion_0 %>% 
  select(one_of(vars_2004)) %>% 
  set_names(new_var_names) %>% 
  mutate(year="y2004")
religion_1_renamed <- religion_1 %>% 
  select(one_of(vars_2017)) %>% 
  set_names(new_var_names) %>% 
  mutate(year="y2017")



# full dataset, unweighted analysis ---------------------------------------


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
                              ),
    belief_God=fct_collapse(belief_God,
                            no_opinion=c("I have no opinion","I don't know and there is no way to find out"),
                            do_not_believe=c("I do not believe in God - Skip to Question 21", "I don't believe in anything beyond the physical world"),
                            higher_power=c("I believe in a higher power or cosmic force"),
                            yes_sometimes=c("I believe in God, but with some doubts","I sometimes believe in God"),
                            yes_no_doubts=c("I have no doubst that God exists","I have no doubts that God exists")))

# full dataset, unweighted analysis ---------------------------------------


# full dataset, unweighted analysis ---------------------------------------


religion_full %>% pull(belief_God) %>% unique()



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




# using survey package for weighted analyses ####

#using 2017 dataset
design2017 <- svydesign(
  ids = ~1, #formula indicating there are no clusters
  data=religion_1,
  strata = NULL,
  weights= ~weight
  )

#whoohooo!
svytable(~q1, design=design2017) %>% 
  as.data.frame.table() %>% 
  arrange(desc(Freq)) 

#how certain get into heaven?
svytable(~r22, design2017, round=TRUE) %>% 
  as.data.frame.table() %>% 
  mutate(prop=Freq/sum(Freq))
#https://www.thearda.com/data-archive?fid=BRS5&tab=2 
#same numbers! But missing not shown

#compare with regular count
religion_1 %>% 
  count(r22)

svytable(~r22, design2017, round=TRUE, Ntotal=100) %>%
  as.data.frame.table() 

#political affiliation
svytable(~q32, design2017, round=TRUE) %>% 
  as.data.frame.table()


#NUMERIC
#health out of 10, current
svytable(~h5a, design2017, round=TRUE) %>% 
  as.data.frame.table()

#cant compute mean prob cuz this is a factor
svymean(~h5a_num, design2017, na.rm=TRUE)
#whoohooo!

#maybe bc there were na?
svymean(~h5a, design2017, na.rm=TRUE)
#not sure what this is doing


#h6hr, during past month, how many hours of sleep?
svymean(~h6_hr_num, design2017, na.rm=TRUE)
svyvar(~h6_hr_num, design2017, na.rm=TRUE)
sqrt(19.126)
#matches website, woohoo!


