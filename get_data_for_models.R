library(httr)
library(jsonlite)
library(elo)
library(dplyr)
library(sjmisc)
library(glue)
library(rvest)
library(stringr)
library(loo)
library(xgboost)
library(rstan)
options(mc.cores = parallel::detectCores())

column_classes <- function(x) {
  sapply(x, class)
}

scale_column <- function(x, na.rm = TRUE) {
  (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
}

map_scores <- function(win_margin, did_win) {
  mapped_scores <- numeric(length(win_margin))
  for (i in 1:length(win_margin)) {
    if(win_margin[i] >= 35) {
      mapped_scores[i] <- 1
    } else if (win_margin[i] <= -5) {
      mapped_scores[i] <- .75
    } else {
      mapped_scores[i] <- (win_margin[i] + 5) / 40
    }
    if(!did_win){
      mapped_scores[i] <- mapped_scores[i]*-1
    }
  }
  return(pmax(mapped_scores, .25))
}

get_api_response <- function(call, flatten = T){
  get_response_details <- GET(url = call)
  response_text <- content(get_response_details, 'text', encoding = 'UTF-8')
  response_data_frame <- as.data.frame(fromJSON(response_text, flatten = flatten))
  return(response_data_frame)
}

## First, to get everything to calculate the Elo ##
# This first block collects all the recent pro games, up to the first DPC game of interest, 
# using open dota api

games_test <- data.frame()
call <- 'https://api.opendota.com/api/proMatches'
get_games_test_details <- GET(url = call)
games_test_text <- content(get_games_test_details, 'text', encoding = 'UTF-8')
games_test <- rbind(games_test, fromJSON(games_test_text, flatten = T))

i = 1
while(min(games_test$match_id) >= 6953791169){ # 6953791169 is the match id of the first DPC game this season
  min_match_id <- min(games_test$match_id)
  call <- glue('https://api.opendota.com/api/proMatches?less_than_match_id={min_match_id}')
  get_games_test_details <- GET(url = call)
  sc <- status_code(get_games_test_details)
  if(sc != 200){
    # Catch the error if the status code is not successful
    cat(paste0("\033[0;", 41, "m",
               glue('Ran into error on iteration {i} with status code = {sc}'),
               "\033[0m","\n")) # Print the unsuccessful gets in a red background
    i = i + 1
    next
  }
  print(glue('status code: {sc}. the iteration is: {i}'))
  games_test_text <- content(get_games_test_details, 'text', encoding = 'UTF-8')
  games_test <- rbind(games_test, fromJSON(games_test_text, flatten = T))
  Sys.sleep(2)
  i = i + 1
}

## Next, we need to work on calculating the elo values for each of these ##
# These are the k-values that will be used to calculate the elo later. 
# They are based on the patch information: games on the most recent patch are the most important

k_vals <- c('7.32d' = 8, '7.32e' = 10, '7.33' = 16, '7.33b' = 20, '7.33c' = 23, '7.33d' = 26)

# This data frame finds only the games that occurred in the dota pro circuit
# It also creates the mapped win margins, but I don't think I'm actually going to use that.

bali_major_games <- games_test %>%
  filter(grepl('The Bali Major', league_name)) %>%
  mutate(# Make sure each variable that has numbers is the correct class
    radiant_team_id = as.character(radiant_team_id),
    match_id = as.character(match_id),
    dire_team_id = as.character(dire_team_id),
    leagueid = as.character(leagueid),
    series_id = as.character(series_id),
    series_type = as.character(series_type),
    # Turn the league types into dummy variables
    league_type = case_when(grepl('Major', league_name) ~ 'Div_1',
                            grepl('\\bDivision I\\b', league_name) ~ 'Div_1',
                            grepl('\\bDivision II\\b', league_name) ~ 'Div_2',
                            grepl('\\bQualifiers\\b', league_name) ~ 'Qual',
                            .default = 'Qual'),
    to_dummy(league_type, suffix = 'label'),
    across(starts_with("league_type_"), as.numeric),
    # Include the patch versions of each game
    patch_version = case_when(start_time <= 1678086000 ~ '7.32d',
                              start_time <= 1681970400 ~ '7.32e',
                              start_time <= 1682402400 ~ '7.33' ,
                              start_time <= 1683957600 ~ '7.33b',
                              start_time <= 1686808800 ~ '7.33c',
                              start_time >  1686808800 ~ '7.33d'),
    # Include k value to make calculation of elo more specific
    k_val = if_else(grepl('Major', league_name), 
                    k_vals[patch_version]+2,
                    k_vals[patch_version])) %>%
  arrange(start_time)

DPC_games <- games_test %>%
  # Grab only the rows that are DPC or Major games
  filter(grepl("\\bDPC\\b", league_name) | grepl('Major', league_name)) %>%
  filter(leagueid != '15438') %>%
  mutate(# Make sure each variable that has numbers is the correct class
         radiant_team_id = as.character(radiant_team_id),
         match_id = as.character(match_id),
         dire_team_id = as.character(dire_team_id),
         leagueid = as.character(leagueid),
         series_id = as.character(series_id),
         series_type = as.character(series_type),
         # Turn the league types into dummy variables
         league_type = case_when(grepl('Major', league_name) ~ 'Div_1',
                                  grepl('\\bDivision I\\b', league_name) ~ 'Div_1',
                                  grepl('\\bDivision II\\b', league_name) ~ 'Div_2',
                                  grepl('\\bQualifiers\\b', league_name) ~ 'Qual',
                                  .default = 'Qual'),
         to_dummy(league_type, suffix = 'label'),
         across(starts_with("league_type_"), as.numeric),
         # Include the patch versions of each game
         patch_version = case_when(start_time <= 1678086000 ~ '7.32d',
                                   start_time <= 1681970400 ~ '7.32e',
                                   start_time <= 1682402400 ~ '7.33' ,
                                   start_time <= 1683957600 ~ '7.33b',
                                   start_time <= 1686808800 ~ '7.33c',
                                   start_time >  1686808800 ~ '7.33d'),
         # Include k value to make calculation of elo more specific
         k_val = if_else(grepl('Major', league_name), 
                         k_vals[patch_version]+2,
                         k_vals[patch_version])) %>%
  arrange(start_time)

#Calculate running elo scores

elo.data <- elo.run(radiant_win ~ radiant_name + dire_name + k(k_val),
                    data = DPC_games, initial.elos = 1000)
elos <- as.list(final.elos(elo.data))
sorted_elos <- elos[order(unlist(elos))]

# Let's put all the teams with their id and their elo into a single dataframe 
# to make it easier to manage later

df_elos <- data.frame(name = names(elos), elo = unname(unlist(elos))) %>%
  left_join(DPC_games %>% select(dire_team_id, dire_name), by = c('name' = 'dire_name'), multiple = 'first') %>%
  rename('team_id' = 'dire_team_id',
         'team_name' = 'name') %>%
  select(team_id, team_name, elo) %>%
  mutate(team_id = as.character(team_id))

## Get information on the teams to help calculate win probability on non-elo metrics ##
team_ids <- unique(c(DPC_games$radiant_team_id, DPC_games$dire_team_id))

# Get all the active players for each team
# This information is scraped from dotabuff.com

active_players <- data.frame()
for(i in get_team_to_start(team_ids, active_players):length(team_ids)){
  team_id = team_ids[i]
  url = glue('https://www.dotabuff.com/esports/teams/{team_id}')
  print(glue('Scraping team {team_id}.'))
  player_roster = read_html(url) %>%
    html_element('section.player-roster') %>%
    html_element('table.table') %>%
    html_table()
  colnames(player_roster) <- player_roster[1,]
  player_roster <- player_roster[-1, -1]
  player_roster <- player_roster %>% mutate(team_id = team_id)
  # Check if there are 5 players maked as active. If not, then just grab the first 5 rows
  if(nrow(player_roster[which(grepl('(Active)', player_roster$Player)), ]) == 5){
    player_roster <- player_roster[which(grepl('(Active)', player_roster$Player)), ]
  } else {
    player_roster <- player_roster[1:5,]
  }
  player_roster$Player <- sub('2023.*', '', player_roster$Player)
  active_players <- rbind(active_players, player_roster)
  Sys.sleep(runif(1, min = 10, max = 15))
}

# Let's clean the active_players up so that it has only the best information
active_players_cleaned <- active_players %>%
  filter(
    # Fix teams that had 6 active players. 
    # This was done manually; I had to look up the official rosters.
    Player != 'Liquid.Jabbz',
    Player != 'MATUOUBA',
    Player != 'BOOM.Mushi',
    Player != 'TSM FTX.MoonMeand…',
    Player != ')'
  ) %>%
  mutate(
    GPM = gsub(',', '', GPM),
    XPM = gsub(',', '', XPM),
    team_id = as.character(team_id)
  )

# Now, lets put all the team stats into one place
team_stats <- active_players_cleaned %>%
  group_by(team_id) %>%
  filter(n() == 5) %>%
  summarize(team_GPM = sum(as.numeric(GPM)),
            team_XPM = sum(as.numeric(XPM)),
            pos1_GPM = max(as.numeric(GPM)),
            pos1_XPM = max(as.numeric(XPM)),
            pos1_KDA = max(as.numeric(KDA)),
            pos1_LH  = max(as.numeric(LH )),
            pos5_GPM = min(as.numeric(GPM)),
            pos5_XPM = min(as.numeric(XPM)),
            pos5_KDA = min(as.numeric(KDA)),
            pos5_LH  = min(as.numeric(LH )),
            GPM_range= max(as.numeric(GPM))-min(as.numeric(GPM)),
            XPM_range= max(as.numeric(XPM))-min(as.numeric(XPM)),
            LH_range = max(as.numeric(LH ))-min(as.numeric(LH))) %>%
  left_join(df_elos, by = c('team_id' = 'team_id'))

# And let's scale the factors so that they are more interpretable by the logistic regression

team_stats_scaled <- team_stats %>%
  mutate(across(where(is.numeric), scale_column),
         elo = if_else(is.na(elo), 0, elo)) %>% # Replace the missing elo values with 0: the average
  select(team_id, team_name, everything())
  
## Let's now put the team stats onto the DPC_games df to build the model. It's called 'DPC_games_with_team_stats'

merged_data_rad <- merge(DPC_games, team_stats_scaled, by.x = "radiant_team_id", by.y = "team_id", all.x = TRUE) %>%
  rename_with(~ paste0("radiant_", .), (ncol(DPC_games)+1):last_col()) %>%
  select(-radiant_team_name)
merged_data_dire <- merge(DPC_games, team_stats_scaled, by.x = "dire_team_id", by.y = "team_id", all.x = TRUE) %>%
  rename_with(~ paste0("dire_", .), (ncol(DPC_games)+1):last_col()) %>%
  select(-dire_team_name, -(names(DPC_games)[names(DPC_games) != 'match_id']))
clean_DPC_data <- left_join(merged_data_rad, merged_data_dire, by = 'match_id')

## The data is now ready to be fed into the models ##