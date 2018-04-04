###########################
"SIMULATED FALLS"
###########################

# install.packages("tibbletime")
# install.packages("gridExtra")

library(tibbletime)
library(tidyverse)
library(lubridate)
library(gridExtra)

# Read data
# The following will work provided the csv is in your project dir:
df <- read_csv("My_Simulated_Falls_Data.csv")

# Examine the dataset

# Also use:
glimpse(df)


df <- df %>% arrange(ward_no, date_of_fall)


# monthly counts of falls in each ward:


mdf <-  df %>%
  # use help to understand following functions 
  # (also see tibbletime github page: https://github.com/business-science/tibbletime )
  as_tbl_time (date_of_fall) %>%
  group_by(ward_no) %>%
  collapse_by("monthly", side = "start") %>% # collapse_by is like group_by, but for dates
  group_by(date_of_fall, add = T) %>%
  summarise(no_of_falls = sum(no_of_falls)) %>%
  mutate(cl = median(no_of_falls))



p1 <- ggplot(mdf, aes(date_of_fall, no_of_falls)) +
  geom_point() +
  geom_line () +
  # horizontal line:
  geom_hline(aes(yintercept=cl), col = 'red') +
  ylim(0, as.integer(1.2*max(mdf$no_of_falls))) +
  scale_x_date(date_labels = "%Y-%m",
               date_breaks = "2 months",
               expand = c(0.05, 0.01)
               ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # rotate labels
  labs(x = "Year-Month",
       y = "Number of Falls") +
  facet_wrap(~ ward_no) +
  theme_bw()

p1

# number of falls by day of week  
wdf <- df %>%
  as_tbl_time (date_of_fall) %>%
  group_by(ward_no) %>%
  group_by(dow = wday(date_of_fall, T, T), add = T) %>%
  summarise(no_of_falls = sum(no_of_falls))   


p2 <- ggplot(wdf, aes(x = dow, y = no_of_falls)) +
  geom_boxplot (outlier.shape = NA) + 
  geom_jitter(aes(color = factor(ward_no)), width = 0.2) +
  labs(x = "Year-Month",y = "Number of Falls")

p2

grid.arrange(p1,p2, ncol = 2)



# plot 1 ward per pdf page...
# setwd("C:/Qsync/UoBradford/Projects/NHS_R_Community/Workshop1 Intro/Plot")

for (i in 1:length(unique(mdf$ward_no))) {
  pdf(paste("ward_no_", i, ".pdf", sep=''))
  plot1 <- ggplot(mdf[mdf$ward_no == i, ], aes(date_of_fall, no_of_falls)) +
    geom_hline(aes(yintercept = cl), col = 'red') +
    geom_point() +
    geom_line () +
    ylim(0, as.integer(1.2*max(mdf$no_of_falls))) +
    labs(title = paste("ward_no", i), x = "Year-Month", y = "Number of Falls") +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months", expand = c(0.05, 0.01)) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  print(plot1)
  dev.off()
}	
