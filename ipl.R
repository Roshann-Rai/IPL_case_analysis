#importing libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forcats)      #fct_reorder() function
library(plotly)

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

#summary statistics
summary(ipl.df$Avg)
summary(ipl.df$SR)
summary(ipl.df$Salary)

#boxplot
boxplot(ipl.df$SR)
title("Strike Rate of Players")

boxplot(ipl.df$Salary)
title("Salary of Players")

boxplot(ipl.df$Avg)
title("Average runs of players")
#It looks like there are two outliers in this variable.
#seems like 4 outliers in this variable.

#Players with outstanding performance
players.no <- ipl.df %>%
  filter(Avg > 35.16 & SR > 148.47)

players.no

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
                                 TRUE ~ "Above 225"),
         player.category = case_when(Avg >= 28.58 & SR >= 133.12 ~ "Good Player Performance",
                                     TRUE ~ "Bad Player Performance"))
setwd("D:/roshan/IPL_case_analysis")
#plotting the players avg run
p1 <- ipl.df %>%
  mutate(player.reordered = fct_reorder(Player, Avg)) %>%
  ggplot(aes(player.reordered, Avg)) +
  geom_col() +
  geom_text(aes(label = Avg), hjust = 0, size = 3.2) +
  labs(x = "",
       y = "Batting average of Player",
       title = "Players Batting average") +
  theme_bw() +
  coord_flip()
p1

#plotting the players salary
p2 <- ipl.df %>%
  mutate(player.reordered = fct_reorder(Player, Salary)) %>%
  ggplot(aes(player.reordered, Salary)) +
  geom_col() +
  labs(x = "",
       y = "Salary in USD million",
       title = "Salary earned by players in IPL") +
  geom_text(aes(label = Salary), hjust = 0, size = 3.2) +
  theme_bw() +
  coord_flip()
p2  

#scatter plot of salary vs avg runs
p3 <- ipl.df1 %>%
  ggplot(aes(Avg, Salary)) +
  geom_point(aes(color = Avg.run.category)) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Average run by players",
       y = "Salary in USD million",
       title = "Scatter plot of Salary vs Average runs") +
  theme_bw() 
ggplotly(p3)

#Scatter plot of salary vs Strike rate
p4 <- ipl.df1 %>%
  ggplot(aes(SR, Salary)) +
  geom_point(aes(color = SR.category)) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Strike Rate by players",
       y = "Salary in USD million",
       title = "Scatter plot of Salary vs strike rate") +
  theme_bw()
ggplotly(p4)

#Karl pearson correlation coefficients
cor.df <- ipl.df %>%
  select(Salary, SR, Avg) 
  
cor(cor.df)

#Two sample t test
t.test.df <- ipl.df1 %>%
  select(player.category, Salary) %>%
  group_by(player.category) %>%
  summarise(Mean = mean(Salary),
            SD = sd(Salary),
            samp_size = n())
t.test.df
t.test(Salary ~ player.category, ipl.df1, alternative = "less")
#At 5% significance level, p-value is less than 0.05 so, alternative hypothesis is accepted.
#i.e. Salary of players with good performance is greater than that of player with bad performance.

#linear regression model of player average run
#Avg runs has been assumed as the performance indicator.
model.salary <- lm(Salary ~ Avg + SR, data = ipl.df)
summary(model.salary)
#Salary is significantly affected Average run (at 5% level of significance) rate but not by strike rate.
#The linear model is salary = 0.6421 + 0.014Avg + Error
#Adjusted R-square = 0.059 i.e., salary is only determined 5% by Avg variable and rest by the variables not present in this linear regression model.
#It suggests that we need to identify more variables to determine the salary of player.

model.salary1 <- lm(Salary ~ Avg + SR + Fifties + Hundreds, data = ipl.df)
summary(model.salary1)
