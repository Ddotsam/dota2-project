library(caret)

## Code to build the knn model ##

# Prepare the data #
knn.dat <- clean_DPC_data %>% 
  select(
    radiant_team_GPM,
    radiant_elo,
    dire_team_GPM,
    dire_elo,
    radiant_win
  ) %>%
  mutate(
    radiant_win = as.factor(radiant_win)
  )

# Build the model #
control <- trainControl(method = "cv",
                        number = 5)
tuning_grid <- expand.grid(k = seq(1, 101, by = 2))


knn.model <- train(form = radiant_win ~ .,
                   data = knn.dat,
                   method = "knn",
                   trControl = control,
                   tuneGrid = tuning_grid)

calc_accuracy_rolling_elo(knn.model)

## This is the function used to test the accuracy for each caret model.
## I wanted to predict game outcomes one by one so that I could update the elo
## of each team after each game.
## The function does this and it does it sequentially so that the games are predicted
## In the order that they occur.
calc_accuracy_rolling_elo <- function(model, return_results = FALSE){
  # Since I used the normalized elo score, I had to recreate the dataframe with all
  # the normalized features so that everything would appear to scale 
  merged_data_rad <- merge(bali_major_games, team_stats_scaled, by.x = "radiant_team_id", by.y = "team_id", all.x = TRUE) %>%
    rename_with(~ paste0("radiant_", .), (ncol(bali_major_games)+1):last_col()) %>%
    select(-radiant_team_name)
  merged_data_dire <- merge(bali_major_games, team_stats_scaled, by.x = "dire_team_id", by.y = "team_id", all.x = TRUE) %>%
    rename_with(~ paste0("dire_", .), (ncol(bali_major_games)+1):last_col()) %>%
    select(-dire_team_name, -(names(bali_major_games)[names(bali_major_games) != 'match_id']))
  clean_bali_major <- left_join(merged_data_rad, merged_data_dire, by = 'match_id') %>%
    arrange(
      start_time
    )
  
  # Create an additional dataframe to hold the rolling elos
  df_elos_updated <- df_elos
  # For each row, I need to first run the prediction, then update the elos
  # Based on the actual game outcome
  for(i in 1:nrow(clean_bali_major)){
    # Scale the elos
    df_elos_updated_scaled <- df_elos_updated %>%
      mutate(
        across(where(is.numeric), scale_column),
        elo = if_else(is.na(elo), 0, elo)
      )
    # Grab the correct match data
    row_to_test <- clean_bali_major[i,] %>%
      select(
        radiant_team_GPM,
        radiant_elo,
        dire_team_GPM,
        dire_elo
      )
    # Grab the correct scaled elos for the teams
    row_to_test$radiant_elo = df_elos_updated_scaled$elo[which(df_elos_updated_scaled$team_id == clean_bali_major$radiant_team_id[i])]
    row_to_test$dire_elo = df_elos_updated_scaled$elo[which(df_elos_updated_scaled$team_id == clean_bali_major$dire_team_id[i])]
    # Run the prediction
    prediction <- predict(model, newdata = row_to_test)
    clean_bali_major$pred_rad_win[i] <- prediction
    # Update the elos
    rad_win <- as.numeric(clean_bali_major$radiant_win[i])
    rad_elo <- df_elos_updated$elo[which(df_elos_updated$team_id == clean_bali_major$radiant_team_id[i])]
    dire_elo <- df_elos_updated$elo[which(df_elos_updated$team_id == clean_bali_major$dire_team_id[i])]
    new_elos <- elo.calc(rad_win, rad_elo, dire_elo, k = 26)
    # Save the new values
    df_elos_updated$elo[which(df_elos_updated$team_id == clean_bali_major$radiant_team_id[1])] <- new_elos[[1]]
    df_elos_updated$elo[which(df_elos_updated$team_id == clean_bali_major$dire_team_id[1])] <- new_elos[[2]]
  }
  
  # Calculate the results
  results <- clean_bali_major %>%
    select(
      radiant_win,
      pred_rad_win
    ) %>%
    mutate(
      pred_rad_win = pred_rad_win - 1,
      correct = if_else(radiant_win == pred_rad_win, TRUE, FALSE)
    )
  
  # Calculate the accuracy
  if(return_results){
    return(results)
  } else {
    return(sum(results$correct)/200)
  }
}
