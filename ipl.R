#importing libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forcats)      #fct_reorder() function

#importing dataset
ipl.df <- read.csv("D:/roshan/ipl_case/ipl_player_performance.csv")
head(ipl.df)
str(ipl.df)

#checking if there are missing data
colSums(is.na(ipl.df))
##It seems there is no missing data in whole dataset

#Removing <a0> from the player names
ipl.df$Player <- gsub("<a0>", " ", ipl.df$Player)
head(ipl.df)

#Creating a new categorical variable from Avg variable
ipl.df1 <- ipl.df %>%
  mutate(Avg.run.category = case_when(Avg < 20 ~ "0-20",
                                      Avg < 40 & Avg <= 20 ~ "20-40",
                                      Avg < 60 & Avg <= 40 ~ "40-60",
                                      Avg < 80 & Avg <= 60 ~ "60-80",
                                      TRUE ~ "80-100"))

#summary statistics
summary(ipl.df$Avg)
summary(ipl.df$Runs)
summary(ipl.df$Salary)
boxplot(ipl.df$Avg)

#number of players with avg run rate above mean
players.no <- ipl.df %>%
  filter(Avg > 28.58) %>%
  count(Player)

#plotting the players avg run
ipl.df %>%
  mutate(player.reordered = fct_reorder(Player, Avg)) %>%
  ggplot(aes(player.reordered, Avg)) +
  geom_col() +
  geom_text(aes(label = Avg), hjust = 0, size = 3.2) +
  labs(x = "Average runs by Player",
       y = "",
       title = "Players Average run") +
  theme_bw() +
  coord_flip()

#plotting the players salary
ipl.df %>%
  mutate(player.reordered = fct_reorder(Player, Salary)) %>%
  ggplot(aes(player.reordered, Salary)) +
  geom_col() +
  labs(x = "Salary in USD million",
       y = "",
       title = "Salary earned by players in IPL") +
  geom_text(aes(label = Salary), hjust = 0, size = 3.2) +
  theme_bw() +
  coord_flip()
  
#scatter plot of salary vs avg runs
ipl.df1 %>%
  ggplot(aes(Avg, Salary)) +
  geom_point(aes(color = Avg.run.category)) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Average run by players",
       y = "Salary in USD million",
       title = "Scatter plot of Salary vs Average runs") +
  theme_bw() 

         