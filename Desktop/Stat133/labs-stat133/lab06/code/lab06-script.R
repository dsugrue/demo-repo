# ==============================================================================
# Title: Lab 6
# Description:
#   This script works with a raw data file and filters it according to variable 
#   values. It exports some of these subsetted data tables into csv files. It
#   also exports text files which contain summaries and structures of the data
#   frames we made. It also makes graphs with and without ggplot of some of the
#   variables and exports these as well. Lastly it practices filtering and
#   selecting, amongst other functions, from data frames using piping techniques.
# Input(s): data file 'nba2018-players.csv'
# Output(s): data files 'warriors.csv' and 'lakers.csv', text files 
#            'data-structure.txt', 'summary-warriors.txt', and
#            'summary-lakers.txt', png files 'scatterplot-height-weight.png'
#            and 'highres-scatterplot-height-weight.png', jpeg file
#            'histogram-age.jpeg', and pdf files 'histogram-age.pdf',
#            'points_salary.pdf', 'height_weight_by_position.pdf'
# Author: Danielle Sugrue
# Date: 10-3-2018
# ==============================================================================

# Required packages
library(readr)    # importing data
library(dplyr)    # data wrangling
library(ggplot2)  # graphics

# Exporting some data tables
#setwd("~/Desktop/Stat133/labs-stat133/lab06/code")

data <- read_csv("../data/nba2018-players.csv", col_names = TRUE, 
                 col_types = "ccciiiicdiiiiii")

warriors <- filter(data, team == 'GSW')
warriors <- arrange(warriors,salary)

write.csv(warriors, "../data/warriors.csv", row.names = FALSE)

lakers <- filter(data, team == 'LAL')
lakers <- arrange(lakers,desc(experience))

write_csv(lakers, "../data/lakers.csv")

# Exporting some R output

sink(file = "../output/data-structure.txt")
str(data)
sink()

sink(file = "../output/summary-warriors.txt")
summary(warriors)
sink()

sink(file = "../output/summary-lakers.txt")
summary(lakers)
sink()

# Exporting some "base" graphs

help("png")

png(filename = "../images/scatterplot-height-weight.png")
plot(data$height, data$weight, pch = 20, xlab = 'Height (inches)', 
     ylab = 'Weight (pounds)')
dev.off()

png(filename = "../images/highres-scatterplot-height-weight.png",  res = 150)
plot(data$height, data$weight, pch = 20, cex = .5, xlab = 'Height (inches)', 
     ylab = 'Weight (pounds)')
dev.off()

jpeg(filename = "../images/histogram-age.jpeg", width = 600, height = 400, 
     units = "px")
hist(data$age, main = "Histogram of Ages", xlab = 'Age (years)',
     col="darkmagenta", xlim = c(15,44))
dev.off()

pdf(file = "../images/histogram-age.pdf", width = 7, height = 5)
hist(data$age, main = "Histogram of Ages", xlab = 'Age (years)', 
     col="darkmagenta", xlim = c(15,44))
dev.off()

## Exporting some ggplots

gg_pts_salary <- ggplot(data, aes(x=points, y=salary)) +
  geom_point(size=2, shape=20, col = "dodgerblue2") + 
  labs(title = "Scatterplot of Points and Salary", y = "Salary (dollars)", 
       x = "Points Scored")
ggsave("../images/points_salary.pdf", width = 7, height = 5, units = "in")

gg_ht_wt_positions <- ggplot(data, aes(x = height, y = weight)) + 
  geom_point(aes(color = position), size = .8, alpha = 0.5) + 
  facet_wrap(~ position)
ggsave("../images/height_weight_by_position.pdf", width = 6, height = 4, 
       units = "in")

# More "dplyr"
## Piping

#display the player names of Lakers 'LAL'.
lakers %>% select(player)

#display the name and salary of GSW point guards 'PG'.
warriors %>% filter(position == 'PG') %>% select(player, salary)

#display the name, age, and team, of players with more than 10 years of 
#experience, making 10 million dollars or less.
data %>% 
  filter(experience > 10 & salary <= 10000000) %>% 
  select(player, age, team)

#select the name, team, height, and weight, of rookie players, 20 years old, 
#displaying only the first five occurrences (i.e. rows).
data %>% 
  filter(age == 20 & experience == 0) %>% 
  select(player, team, height, weight) %>% 
  head(5)

#create a data frame gsw_mpg of GSW players, that contains variables for player 
#name, experience, and min_per_game (minutes per game), sorted by min_per_game 
#(in descending order).
warriors$min_per_game <- warriors$minutes/warriors$games %>% 
  as.numeric()
gsw_mpg <- warriors %>% 
  select(player, experience, min_per_game) %>% 
  arrange(desc(min_per_game)) %>%
  data.frame()

#display the average triple points by team, in ascending order, of the bottom-5 
#teams (worst 3pointer teams).
group_by(data, team) %>% 
  summarize(avg_points3 = mean(points3)) %>% 
  arrange(avg_points3) %>% 
  head(5)

#obtain the mean and standard deviation of age, for Power Forwards, with 5 and 
#10 years (including) of experience.
filter(data, position == 'PF' & 5 <= experience & experience <= 10) %>% 
  summarize(meanX = mean(age), sdX = sd(age))
