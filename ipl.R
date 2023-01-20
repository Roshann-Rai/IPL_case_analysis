install.packages("forcats")
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

#Two sample t test
t.test.df <- ipl.df1 %>%
  select(player.category, Salary) %>%
  group_by(player.category) %>%
  summarise(Mean = mean(Salary),
            SD = sd(Salary),
            samp_size = n())

# ggplot(ipl.df1, aes(x = Salary)) + 
#   geom_histogram(binwidth = 0.5) + 
#   facet_wrap(~player.category, ncol = 1)

#Alternative hypothesis --> Income of good players is greater than bad players.
t.test(Salary ~ player.category, ipl.df1, alternative = "less")

#linear regression model of player average run
#Avg runs has been assumed as the performance indicator.
model.salary <- lm(Salary ~ Avg + SR, data = ipl.df)
summary(model.salary)


#It looks like 
?t.test()
