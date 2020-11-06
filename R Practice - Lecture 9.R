hofstede <- read.csv(
  "https://geerthofstede.com/wp-content/uploads/2016/08/6-dimensions-for-website-2015-08-16.csv",
  stringsAsFactors = FALSE, sep = ";"
  )

library(dplyr)

idv_df <- select(hofstede, country, idv)
idv_numeric_df <- mutate(idv_df, idv = as.numeric(idv))
idv_not_nas_df <- filter(idv_numeric_df, !is.na(idv))

idv_sorted_df <- arrange(idv_not_nas_df, idv) #sorts by idv num low->high

idv_summary_df <- summarize(idv_sorted_df,
                  avg_idv = mean(idv),
                  min_idv = min(idv),
                  max_idv = max(idv))

#find min and max individualism countries
max_row <- filter(idv_sorted_df, idv == max(idv))
max_country <- pull(max_row, country)
min_row <- filter(idv_sorted_df, idv == min(idv))
min_country <- pull(min_row, country)


library("pscl")

votes_2008 <- filter(presidentialElections, year == 2008)
most_dem_votes <- filter(votes_2008, demVote == max(demVote))
most_dem_state <- select(most_dem_votes, state)
pull(most_dem_votes, state)

# The Pipe Operator
most_dem_state <- presidentialElections %>% # data frame to start with
  filter(year == 2008) %>% # 1. filter down to only 2008 votes
  filter(demVote == max(demVote)) %>% # 2. filter down to the highest "demVote"
  select(state) # 3. select name of the state

#Hofstede example with Chaining (Pipe Operator)
idv_data_df <- hofstede %>%
  select(country, idv) %>%
  mutate(idv = as.numeric(idv)) %>%
  filter(!is.na(idv)) %>%
  arrange(idv)

idv_summary2_df <- idv_data_df %>%
  summarize(avg_idv = mean(idv),
            min_idv = min(idv),
            max_idv = max(idv))

max_country2 <- idv_data_df %>%
  filter(idv == max(idv)) %>%
  pull(country)

min_country2 <- idv_data_df %>%
  filter(idv == min(idv)) %>%
  pull(country)