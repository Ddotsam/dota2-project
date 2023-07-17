library(caret)
library(e1071)

## Code to build the svm model ##

# Prepare the data #
svm.dat <- clean_DPC_data %>% 
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

# I want to test multiple smv kernels, I'm setting those up here #

svm_kernels <- c("Linear", "Poly", "Radial", "Sigmoid")
control <- trainControl(method = "cv", number = 5)
svm.models <- list()

for (kernel in svm_kernels) {
  model <- train(form = radiant_win ~ .,
                 data = svm.dat,
                 method = paste0("svm", kernel),
                 trControl = control)
  
  svm.models[[kernel]] <- model
}

# The gaussian kernel is created with the sigmoid = TRUE argument
# I just let the sigmoid kernel return an error and then implemented it here

svm.models[['Sigmoid']] <- train(
  form = radiant_win ~ .,
  data = svm.dat,
  method = 'svmRadial',
  trControl = control,
  sigmoid = TRUE
)

# Test each model using the rolling elo function
accuracy_results <- list()
for(kernel in svm_kernels){
  accuracy_results[[kernel]] <- calc_accuracy_rolling_elo(svm.models[[kernel]])
}

# Let's further tune the best performing models
param_grid_radial <- expand.grid(C = c(.5, 1, 1.5),
                                 sigma = c(.1, .15, .2))

svm.radial_tuned <- train(form = radiant_win ~ .,
                          data = svm.dat,
                          method = "svmRadial",
                          trControl = control,
                          tuneGrid = param_grid_radial)
svm.radial_tuned
calc_accuracy_rolling_elo(svm.radial_tuned)

param_grid_linear <- expand.grid(C = c(0.05, 0.1, 0.15))
svm.linear_tuned <- train(form = radiant_win ~ .,
                          data = svm.dat,
                          method = "svmLinear",
                          trControl = control,
                          tuneGrid = param_grid_linear)
svm.linear_tuned
calc_accuracy_rolling_elo(svm.linear_tuned)

# Turns out the best model was the origional without tuning haha