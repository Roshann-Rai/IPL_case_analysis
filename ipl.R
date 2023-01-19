#importing libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forcats)      #fct_reorder() function

#importing dataset
ipl.df <- read.csv("https://raw.githubusercontent.com/Roshann-Rai/IPL_case_analysis/main/ipl_player_performance.csv")
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
  mutate(Avg.run.category = case_when(Avg < 20 ~ "Less than 20",
                                      Avg < 40 & Avg >= 20 ~ "20-40",
                                      Avg < 60 & Avg >= 40 ~ "40-60",
                                      Avg < 80 & Avg >= 60 ~ "60-80",
                                      TRUE ~ "Above 80"),
         SR.category = case_when(SR < 75 ~ "Less than 75",
                                 SR < 150 & SR >= 75 ~ "75-150",
                                 SR < 225 & SR >= 150 ~ "150-225",
                                 TRUE ~ "Above 225"))

#summary statistics
summary(ipl.df$Avg)
summary(ipl.df$SR)
summary(ipl.df$Salary)
boxplot(ipl.df$Avg)        #It looks like there are two outliers in this variable.
boxplot(ipl.df$Salary)
boxplot(ipl.df$SR)         #seems like 4 outliers in this variable.

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

#Scatter plot of salary vs Strike rate
ipl.df1 %>%
  ggplot(aes(SR, Salary)) +
  geom_point(aes(color = SR.category)) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Strike Rate by players",
       y = "Salary in USD million",
       title = "Scatter plot of Salary vs strike rate") +
  theme_bw() 

#Karl pearson correlation coefficients
cor.df <- ipl.df %>%
  select(Salary, SR, Avg) 
  
cor(cor.df)

#linear regression model of player salary
model.salary <- lm(Salary ~ Avg + SR, data = ipl.df)
summary(model.salary)         
