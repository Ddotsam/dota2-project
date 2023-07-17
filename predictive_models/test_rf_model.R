library(caret)
library(randomForest)
library(ggplot2)
library(rpart.plot)

## Code to build the random forest model ##

# Prepare the data #
rf.dat <- clean_DPC_data %>% 
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
control <- trainControl(method = "cv", number = 5)


rf.model <- train(form = radiant_win ~ .,
                  data = rf.dat,
                  method = 'rf',
                  trControl = control)
calc_accuracy_rolling_elo(rf.model)

# Lets tune the hyperparameters #
param_grid <- expand.grid(mtry = c(1, 2, 3))

rf.model_tuned <- train(form = radiant_win ~ .,
                        data = rf.dat,
                        method = "rf",
                        trControl = control,
                        tuneGrid = param_grid,
                        importance = TRUE,
                        verbose = TRUE)
rf.model_tuned
results <- calc_accuracy_rolling_elo(rf.model_tuned, return_results = TRUE)
# We end up with an accuracy of 66.5%! Not bad!

## Plot various things, including data from the logistic regression model ##
varImpPlot(rf.model_tuned$finalModel)
imp <- as.data.frame(varImpPlot(rf.model_tuned$finalModel)) %>% arrange(desc(MeanDecreaseGini))
imp$varnames <- rownames(imp)
imp$team <- c('dire', 'radiant', 'radiant', 'dire')

# Plot barplot for feature importance
ggplot(imp, aes(x = reorder(varnames, MeanDecreaseGini), weight = MeanDecreaseGini, fill = as.factor(team))) +
  geom_bar() +
  scale_fill_manual(name = 'Team', values = c("brown3", "olivedrab3")) +
  ylab('Mean Decrease In Gini') +
  xlab('Variable Name') +
  ggtitle('Feature Importance in Random Forest Model') +
  coord_flip()

# Plot alternate barplot for feature importance
ggplot(imp, aes(x = reorder(varnames, MeanDecreaseGini), y = MeanDecreaseGini, color = as.factor(team))) +
  geom_point() +
  geom_segment(aes(x = varnames, xend = varnames, y = 0, yend = MeanDecreaseGini)) +
  ylab('Mean Decrease In Gini') +
  xlab('Variable Name') +
  coord_flip()

# Plot one of the trees from the final model
t <- train(form = radiant_win ~ .,
           data = rf.dat,
           method = 'rpart',
           trControl = control)

tree <- getTree(t$finalModel, k = 1, labelVar = T)
rpart.plot(tree)

pars <- c('intercept', 'b_radiant_team_GPM', 'b_radiant_elo', 'b_dire_team_GPM', 'b_dire_elo')
summary(dota2_self_elo.fit_gpm, pars = pars)$summary
vals <- as.data.frame(extract(dota2_self_elo.fit_gpm, pars = pars))
vals_long <- reshape2::melt(vals)

# Plot stacked density plots from logistic regression model
ggplot(vals_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("intercept" = "brown3", 
                               "b_radiant_team_GPM" = "chocolate", 
                               "b_radiant_elo" = "cadetblue",
                               "b_dire_team_GPM" = "olivedrab3",
                               "b_dire_elo" = "darkorchid1")) +
  labs(x = "Value", y = "Density", title = "Logistic Regression Variable Values")

# Plot violin plots
ggplot(vals_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(alpha = 0.8) +
  scale_fill_manual(values = c("intercept" = "brown3", 
                               "b_radiant_team_GPM" = "chocolate", 
                               "b_radiant_elo" = "cadetblue",
                               "b_dire_team_GPM" = "olivedrab3",
                               "b_dire_elo" = "darkorchid1")) +
  labs(x = "Variable", y = "Value", title = "Logistic Regression Variable Values") +
  coord_flip()