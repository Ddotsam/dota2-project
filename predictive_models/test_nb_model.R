library(caret)

## Code to build the naive bayes model ##

# Prepare the data #
nb.dat <- clean_DPC_data %>% 
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

# Create a train control object
control <- trainControl(method = "cv", number = 5)

# Train the Naive Bayes model
nb.model <- train(form = radiant_win ~ .,
                  data = nb.dat,
                  method = "nb",
                  trControl = control)

nb.model
calc_accuracy_rolling_elo(nb.model)
