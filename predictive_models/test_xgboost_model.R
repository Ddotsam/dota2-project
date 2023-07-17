## Code to build the xgboost model ##
xgboost.dat <- clean_DPC_data %>% 
  select(
    radiant_team_GPM,
    radiant_elo,
    dire_team_GPM,
    dire_elo,
    radiant_win
  )

xgboost.model <- xgboost(data = as.matrix(xgboost.dat %>% select(-radiant_win)),
                         label = as.matrix(xgboost.dat %>% select(radiant_win)),
                         nrounds = 100000,
                         early_stopping_rounds = 20,
                         objective = "binary:logistic")

## The code for getting the accuracy of the xgboost model
## is slightly different than the other models, so I couldn't use the 
## function I defined for the other ones
bali_major_games$pred_rad_win <- NA
bali_major_games$league_type_Div_1 <- 1
bali_major_games$league_type_Div_2 <- 0
bali_major_games$league_type_Qual  <- 0

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

df_elos_updated <- df_elos
for(i in 1:nrow(clean_bali_major)){
  df_elos_updated_scaled <- df_elos_updated %>%
    mutate(
      across(where(is.numeric), scale_column),
      elo = if_else(is.na(elo), 0, elo)
    )
  row_to_test <- clean_bali_major[i,] %>%
    select(
      radiant_team_GPM,
      radiant_elo,
      dire_team_GPM,
      dire_elo
    )
  row_to_test$radiant_elo = df_elos_updated_scaled$elo[which(df_elos_updated_scaled$team_id == clean_bali_major$radiant_team_id[i])]
  row_to_test$dire_elo = df_elos_updated_scaled$elo[which(df_elos_updated_scaled$team_id == clean_bali_major$dire_team_id[i])]
  # The type = 'response' part is the only difference :/
  prediction <- round(predict(xgboost.model, as.matrix(row_to_test), type = 'response'))
  clean_bali_major$pred_rad_win[i] <- prediction
  rad_win <- as.numeric(clean_bali_major$radiant_win[i])
  rad_elo <- df_elos_updated$elo[which(df_elos_updated$team_id == clean_bali_major$radiant_team_id[i])]
  dire_elo <- df_elos_updated$elo[which(df_elos_updated$team_id == clean_bali_major$dire_team_id[i])]
  new_elos <- elo.calc(rad_win, rad_elo, dire_elo, k = 26)
  df_elos_updated$elo[which(df_elos_updated$team_id == clean_bali_major$radiant_team_id[1])] <- new_elos[[1]]
  df_elos_updated$elo[which(df_elos_updated$team_id == clean_bali_major$dire_team_id[1])] <- new_elos[[2]]
}

results <- clean_bali_major %>%
  select(
    radiant_win,
    pred_rad_win
  ) %>%
  mutate(
    correct = if_else(radiant_win == pred_rad_win, TRUE, FALSE)
  )

# Code to calculate accuracy #
sum(results$correct)/200

## Code to plot various parts of model ##
importance_matrix = xgb.importance(colnames(xgboost.dat %>% select(-radiant_win)), 
                                   model = xgboost.model)
xgb.plot.importance(importance_matrix[1:7,], main = 'XGBoost Feature Importance', xlab = 'Gain', ylab = '')
