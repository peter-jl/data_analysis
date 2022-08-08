
#data from UCI machine learning data repository
#https://archive.ics.uci.edu/ml/datasets/human+activity+recognition+using+smartphones#
#used as an example for Coursera's Exploratory Data Analysis course with Roger
#Peng, Week 4 - Clustering Case Study video
#topics: hierarchical clustering, singular value decomposition, k-means clustering

#using training data set

library(tidyverse)
theme_set(theme_light())
#"2022-08-07_Samsung_human_activity/UCI HAR Dataset/train/subject_train.txt"

files <- list.files(path = "2022-08-07_Samsung_human_activity/UCI HAR Dataset/train/", 
                    pattern=".txt",
                    full.names=TRUE)

#these are the 21 subjects in the training set
#1, 3, 5, 6, 7, 8, 11, 14, 15, 16, 17, 19, 21, 22, 23, 25, 26, 27, 28, 29, 30
#these are the 9 subjects in the test set
#2  4  9 10 12 13 18 20 24

subject_train <- read_fwf(files[1]) %>% 
  rename(subject=X1)
x_names <- read_lines(file = "2022-08-07_Samsung_human_activity/UCI HAR Dataset/features.txt") %>% 
  str_remove("^\\d* ")
x_train <- read_fwf(files[2]) %>% 
  set_names(x_names) %>% 
  janitor::clean_names()
y_train <- read_fwf(files[3]) %>% 
  rename(activity=X1) %>% 
  mutate(activity=case_when(activity==1~"walking",
                            activity==2~"walking_upstairs",
                            activity==3~"walking_downstairs",
                            activity==4~"sitting",
                            activity==5~"standing",
                            activity==6~"laying"))
#subject_train.txt = 7,352 rows x 1 column
#X_train.txt = 7,352 rows x 561 columns
#y_train.txt = 7,352 rows x 1 column, numbers 1 to 6 corresponding with the 6 activities:
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING

#bind together subjects, y, and x data
train_data <- bind_cols(x_train, subject_train, y_train) %>% 
  mutate(activity=fct_inorder(activity)) #to match coursera graphs
  
#slide 4/18
train_data %>% 
  count(activity)


# plotting average acceleration for first subject ####
#slide 5/18
sub1 <- train_data %>% 
  filter(subject==1)

sub1 %>% 
  select(1, 2, activity) %>% 
  mutate(index=1:nrow(.)) %>% 
  pivot_longer(1:2, names_to="measure", values_to="value") %>% 
  ggplot(aes(index, value, colour = activity)) +
  geom_point() +
  scale_y_continuous(breaks=seq(-.6, 4, .2), labels = ~round(.x, 2)) +
  facet_wrap(~measure)
  

# clustering based just on average acceleration ####
#slide 6/18
sub1df <- as.data.frame(sub1)
rownames(sub1df) <- make.unique(as.character(sub1df$activity)) #rownames must be unique, add unique number then remove when adding label colours

distance_matrix <- dist(sub1df[1:3])
hclustering <- hclust(distance_matrix)
#plot(hclustering)

colorCodes <- c("standing"="red", "sitting"="green", "laying"="lightblue",
                "walking"="blue", "walking_downstairs"="yellow", "walking_upstairs"="purple" )

## function to set label color
#https://stackoverflow.com/questions/18802519/label-and-color-leaf-dendrogram
labelCol <- function(x) {
  if (is.leaf(x)) {
    ## fetch label
    label <- attr(x, "label")
    code <- str_remove(label, "\\..*$") #remove the unique parts of rownames added by make.unique() above
    ## use the following line to reset the label to one letter code
    # attr(x, "label") <- code
    attr(x, "nodePar") <- list(lab.col=colorCodes[code])
  }
  return(x)
}

## apply labelCol on all nodes of the dendrogram
d <- dendrapply(as.dendrogram(hclustering), labelCol)
plot(d)



# plotting max acceleration for first subject ####
#slide 7/18

sub1 %>% 
  select(10, 11, activity) %>% 
  mutate(index=1:nrow(.)) %>% 
  pivot_longer(1:2, names_to="measure", values_to="value") %>% 
  ggplot(aes(index, value, colour = activity)) +
  geom_point() +
  facet_wrap(~measure, scales="free_y")


# clustering based just on maximum acceleration ####
#slide 8/18

distance_matrix <- dist(sub1df[10:12])
hclustering <- hclust(distance_matrix)
#plot(hclustering)

## apply labelCol on all nodes of the dendrogram
d <- dendrapply(as.dendrogram(hclustering), labelCol)
plot(d)


# singular value decomposition ####
#slide 9/18

svd1 <- svd(scale(sub1[-c(562,563)]))

tibble(index=1:nrow(sub1),
       activity=sub1$activity,
       svd_1=svd1$u[,1],
       svd_2=svd1$u[,2]) %>% 
  pivot_longer(contains("svd"), names_to="component", values_to="value") %>% 
  ggplot(aes(index, value, colour=activity)) +
  geom_point() +
  facet_wrap(~component, scales="free_y")

#first component = walking vs standing/sitting/laying (moving vs still)
#second component = standing/walking/walking downstairs vs walking upstairs (moving upward vs not)
#(sitting and laying contribute to both poles but more the first)


# find maximum contributor ####
#slide 10/18

plot(svd1$v[,2], pch=19)



# new clustering with maximum contributor ####
#slide 11/18

max_contrib <- which.max(svd1$v[, 2]) #which of the 561 features contributes most of the variations across observations

#cluster with accelerations + maximum contributor
distance_matrix <- dist(sub1df[, c(10:12, max_contrib)])
hclustering <- hclust(distance_matrix)
#plot(hclustering)

## apply labelCol on all nodes of the dendrogram
d <- dendrapply(as.dendrogram(hclustering), labelCol)
plot(d)

# colorCodes <- c("standing"="red", "sitting"="green", "laying"="lightblue",
#                 "walking"="blue", "walking_downstairs"="yellow", "walking_upstairs"="purple" )

#now the various activities are separating out more
#3 clear clusters, "walking_upstairs"="purple", "walking"="blue", "walking_downstairs"="yellow"



# maximum contributor ####
#slide 12/18 

names(sub1[max_contrib])
#body acceleration mean frequency in z dimension


# K-means clustering (nstart=1, first try) ####
#slide 13/18

#note there's some randomness each time kmeans() is run
#you may get a suboptimal solution by chance if you just use nstart=1
#best to use more nstarts to increase chance of getting more optimal solution

set.seed(2021)
k_clust <- kmeans(sub1[-c(562,563)], centers = 6, nstart=1)
table(k_clust$cluster, sub1$activity)
#6 clusters
#cluster 2 is combination of standing sitting and laying
#cluster 1 is walking up
#cluster 6 is walking down
#cluster 5 is walking

#slide 14/18
#if you try again you'll get a different answer
set.seed(2022)
k_clust <- kmeans(sub1[-c(562,563)], centers = 6, nstart=1)
table(k_clust$cluster, sub1$activity)
#cluster 1 is a combination of standing, sitting and laying

#slide 15/18
#now try nstart=100
set.seed(2022)
k_clust <- kmeans(sub1[-c(562,563)], centers = 6, nstart=100)
table(k_clust$cluster, sub1$activity)
#with 100 starts, the solution looks identical to Roger's 
#takes the most optimal solution of 100 tries
#things separate out a bit better this time
#cluster 2 is laying
#cluster 6 is walking
#cluster 4 is walking down


# So you can see kind of, can see where the kind of cluster centers are. And the
# idea is that each of the clusters Has a mean value or a center in a, in this
# 500 dimensional space. And so we can see kind of which features of these 500
# features seem to drive the location of the center for that given cluster. And
# then, that will help us, help give us some idea of you know what features.
# Seem to be important for classifying people in that cluster, or classifying
# observations in that cluster.


# Cluster 2 variable centers (laying)
#slide 17/18

#plot the first 10 centers of 561 features for cluster 2
plot(k_clust$center[2, 1:10], pch=19, ylab="Cluster Center", xlab="")
#so the first cluster has high values on the first 3 features (mean body acceleration)
# and low values for features 4 - 10

# cluster 6 variable centers (walking)
#slide 18/18

#plot the first 10 centers of 561 features for cluster 6
plot(k_clust$center[6, 1:10], pch=19, ylab="Cluster Center", xlab="")
#walking has a different set of cluster centers for first 10 of 561 features compared to laying

#by examining profile of centers, you can get a hint as to which features will
#be most useful for predicting different activities


