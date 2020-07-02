library(tidyverse)
library(data.table)
library(gganimate); library(transformr)


# Andrew's script from 
#  https://statmodeling.stat.columbia.edu/2020/07/02/no-i-dont-believe-that-claim-based-on-regression-discontinuity-analysis-that/#comments
df_rdd <- fread("longevity.csv")

death_date <- sapply(df_rdd[,"death_date_imp"], as.character)
living <- df_rdd[,"living"] == "yes"
death_date[living] <- "2020-01-01"
election_year <- as.vector(unlist(df_rdd[,"year"]))
election_date <- paste(election_year, "-11-05", sep="")
more_days <- as.vector(as.Date(death_date) - as.Date(election_date))
more_years <- more_days/365.24
age <- as.vector(unlist(df_rdd[,"living_day_imp_pre"]))/365.24
n <- nrow(df_rdd)
name <- paste(unlist(df_rdd[,"cand_last"]), unlist(df_rdd[,"cand_first"]), unlist(df_rdd[,"cand_middle"]))
first_race <- c(TRUE, name[2:n] != name[1:(n-1)])
margin <- as.vector(unlist(df_rdd[,"margin_pct_1"]))
won <- ifelse(margin > 0, 1, 0)
lifetime <- age + more_years
decades_since_1950 <- (election_year - 1950)/10
data <- data.frame(margin, won, election_year, age, more_years, living, lifetime, decades_since_1950)
subset <- first_race & election_year >= 1945 & election_year <= 2012 & abs(margin) < 10 & !living





# my version, as facets
data %>%
  filter(first_race & election_year >= 1945 & 
           election_year <= 2012 & 
           abs(margin) < 10 & 
           !living) %>%
  uncount(19, .id = "split") %>%
  mutate(split = split - 10,
         split_label = paste("split at", split) %>%
           fct_reorder(split)) %>%
  mutate(side = if_else(margin < split, "below cutoff", "above cutoff")) %>%
  ggplot(aes(margin, more_years, color = side)) +
  geom_point(alpha = 0.3, size = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~split_label) +
  theme_minimal()


# my version as animation
animate(
data %>%   # Using Andrew's "data"
  filter(first_race & election_year >= 1945 & 
           election_year <= 2012 & 
           abs(margin) < 10 & 
           !living) %>%
  mutate(point_id = row_number()) %>%
  uncount(40, .id = "split") %>%
  mutate(split = split/2 - 10,
         split_label = paste("split at", split) %>%
           fct_reorder(split)) %>%
  mutate(side = if_else(margin < split, "below cutoff", "above cutoff")) %>%
  ggplot(aes(margin, more_years, color = side)) +
  geom_point(alpha = 0.5, size = 0.5, aes(group = point_id)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "{closest_state }") +
  transition_states(split_label, state_length = 0.1) +
  theme_minimal(),
nframes = 200, fps = 25, type = "cairo" )
  
