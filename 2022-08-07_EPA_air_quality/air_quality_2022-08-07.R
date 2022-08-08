

#analysis from Coursera course Exploratory Data Analysis with Roger Peng, Week 4 - Air Pollution Case Study
#RD_501_88101_1999-0.txt - roger's file name, mine is daily_88101_1999.csv
#roger's file has 117,421 rows - 13,217 missing values = 104,204 valid rows, which is close to the data file I have (103,210)
#RD_501_88101_2012-0.txt - roger's file name, mine is daily_88101_2012.csv
#roger's file has 1,304,207 rows - 73,133 missing values = 1,231,074 valid rows, which is much more than what I have (276,671) 

#AQS parameter code for PM2.5 is 88101
#https://aqs.epa.gov/aqsweb/documents/codetables/parameter_classes.html
#PM 2.5 describes fine inhalable particles, with diameters that are generally 2.5 micrometers and smaller

#data from:
#https://www.epa.gov/technical-air-pollution-resources
#https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily 
#scroll down to 88101, download 1999 and 2012
#Roger notes that PM2.5 monitoring began in 1999

#data documentation
#https://aqs.epa.gov/aqsweb/airdata/FileFormats.html#_daily_summary_files


library(tidyverse)
library(lubridate)
theme_set(theme_light())

# files <- list.files("2022-08-07_EPA_air_quality/data/", pattern=".csv", full.names = TRUE)
# air_data <- map_df(files, ~read_csv(.x)) %>% 
#   janitor::clean_names()

pm0 <- read_csv("2022-08-07_EPA_air_quality/data/daily_88101_1999.csv") %>% 
  janitor::clean_names()
pm1 <- read_csv("2022-08-07_EPA_air_quality/data/daily_88101_2012.csv") %>% 
  janitor::clean_names()
pm_full <- bind_rows(pm0, pm1) %>% 
  mutate(year=year(date_local),
         month=month(date_local, label=TRUE),
         county_site=str_c(county_code, site_num, sep="-"))


pm0 %>% 
  glimpse()
Hmisc::describe(pm0)




#daily average PM2.5
pm0 %>% 
  ggplot(aes(arithmetic_mean)) +
  geom_histogram(binwidth=2)
#very large positive skew

pm0 %>% 
  summarize(mean=mean(arithmetic_mean),
            q25=quantile(arithmetic_mean, probs=.25),
            median=median(arithmetic_mean),
            q75=quantile(arithmetic_mean, probs=.75),
            n=n(),
            sd=sd(arithmetic_mean))
pm0$arithmetic_mean %>% summary()
#this looks very similar to Roger's values

pm1$arithmetic_mean %>% summary()
#this is somewhat different from Roger's values
#max for 2012 is 236.254, in Roger's it's 909.00
#min for 2012 is -6.312, in Roger's it's -10.00
#Roger explains that we measure mass of PM2.5 per unit of air flow, shouldn't have negative mass
sum(pm1$arithmetic_mean<0) #1,130 values are negative, compared to 26,474 for Roger's data
mean(pm1$arithmetic_mean<0) #0.4% of values are negative, compared to 2% for Roger's data
#Rogers questions whether there's a pattern for negative values, certain times of year or locations?

pm_full %>% 
  ggplot(aes(arithmetic_mean)) +
  geom_histogram(binwidth=2) +
  facet_wrap(~year, nrow=2, scales="free_y")

pm_full %>% 
  mutate(year = fct_rev(factor(year))) %>% 
  ggplot(aes(arithmetic_mean+7, year)) +
  geom_boxplot(varwidth=TRUE) +
  scale_x_log10()


#dates
pm_full %>% 
  mutate(negative_vals=arithmetic_mean<0) %>% 
  ggplot(aes(month, fill = negative_vals)) +
  geom_bar(position="stack") +
  facet_wrap(~as.character(year), ncol=1) +
  scale_fill_manual(values=c("TRUE"="red", "FALSE"="lightblue")) +
  scale_y_log10()
#Roger notes that PM2.5 just started being measured in 1999
#more consistent measures across the year for 2012
#negative values in 2012 are relatively consistent across months




#focus in on monitors in New York State, only those that have data for both 1999 and 2012
#controls for possible changes in monitors and locations by focusing in on specific monitor at specific location
ny_monitors <- pm_full %>% 
  filter(state_name=="New York") %>% 
  group_by(county_site) %>% 
  filter(n_distinct(year)==2) %>% 
  ungroup()

ny_monitors %>% 
  ftable(year~county_site, data=.) %>% 
  as.matrix() %>% 
  addmargins()

#focus on county_site "063-2008"
ny_site <- ny_monitors %>% 
  filter(county_site == "063-2008")

ny_site %>% 
  mutate(date_local=update(date_local, year=1999)) %>% 
  group_by(year) %>% 
  mutate(median_year=median(arithmetic_mean)) %>% 
  ungroup() %>% 
  ggplot(aes(date_local, arithmetic_mean, colour=as.character(year))) +
  geom_point() +
  geom_hline(aes(yintercept=median_year, colour=as.character(year)), linetype=2) +
  scale_x_date(date_breaks="month", date_labels="%b") +
  facet_wrap(~as.character(year), ncol=2) +
  theme(legend.position = "none") +
  labs(title = "Daily PM2.5 mean for New York 063-2008 monitor",
       subtitle="1999 vs 2012",
       x="", y="mean PM2.5")



#average value by state over time
#note for some reason Illinois doesn't have 2012 data
pm_full %>% 
  filter(state_name!="Illinois") %>% 
  group_by(state_name, year) %>% 
  summarize(mean_pm25=mean(arithmetic_mean)) %>% 
  mutate(decreasing=ifelse(diff(mean_pm25)<0, "decreasing", "increasing")) %>% 
  ungroup() %>% 
  arrange(desc(decreasing)) %>% 
  ggplot(aes(as.character(year), mean_pm25, group=state_name, colour=decreasing)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = state_name, hjust=ifelse(year==1999, 1.1, -0.1)), check_overlap=TRUE) +
  scale_colour_manual(values=c("decreasing"="gray", "increasing"="red")) +
  theme(legend.position="none") +
  labs(x = "Year", y = "Mean PM2.5",
       title="PM2.5 Change by State",
       subtitle = "Red = increase over time")



#### additional graphs

#strip plot of daily mean PM2.5 over time
# pm_full %>% 
#   mutate(arithmetic_mean=pmax(0, arithmetic_mean)) %>% #set negative values to 0
#   ggplot(aes(arithmetic_mean+1, as.character(year))) +
#   geom_jitter(width=0, alpha=.5) +
#   scale_x_log10()
#trying to plot 379,881 data points with jitter takes a long time
#not a great plot


#dumbell plot showing mean change over time by state
pm_full %>% 
  filter(state_name!="Illinois") %>% 
  group_by(state_name, year) %>% 
  summarize(mean_pm25=mean(arithmetic_mean)) %>% 
  mutate(decreasing=ifelse(diff(mean_pm25)<0, "decreasing", "increasing")) %>% 
  ungroup() %>% 
  arrange(year, mean_pm25,state_name) %>% 
  mutate(year=as.character(year),
         state_name=fct_inorder(factor(state_name))) %>% 
  ggplot(aes(mean_pm25, state_name, colour=year)) +
  geom_point(aes(shape=decreasing))



pm_full %>% 
  group_by(year=as.character(year), state=state_name, month) %>% 
  summarize(mean_pm25=mean(arithmetic_mean)) %>% 
  ungroup() %>% 
  ggplot(aes(month, mean_pm25, colour=year, group=year)) +
  geom_line() +
  facet_wrap(~state)
#Idaho huge increase in PM2.5 in September 2012 due to forrest fires
#Montana peaks in same time period, probably the same forrest fires
#Georgia, Alabama, South Carolina, North Carolina peaks Aug 1999
#would be nice if could facet into rough geographical state location, see regional trends more clearly

library(geofacet)
pm_full %>% 
  group_by(year=as.character(year), state=state_name, month) %>% 
  summarize(mean_pm25=mean(arithmetic_mean)) %>% 
  group_by(year) %>% 
  mutate(year_median=median(mean_pm25)) %>% 
  ungroup() %>% 
  ggplot(aes(month, mean_pm25, colour=year, group=year)) +
  geom_line() +
  facet_geo(~state) +
  theme(axis.text.x=element_text(angle=90, vjust=0),
        legend.position = c(0.1,0.95))
#could not match DC, PR, Virgin Islands
#no Maine data?

