
# Initial setup -----------------------------------------------------------


library(tensorflow)
library(keras)

# check if working
#sess = tf$Session()
#hello <- tf$constant('Hello, TensorFlow!')
#sess$run(hello)

<<<<<<< HEAD
# use dataframe prepared for h2o.ai
str(df_train$Brand)
str(df_pred)
colnames(df_train)
=======
# # use dataframe prepared for h2o.ai
# str(df_train)
# str(df_pred)
>>>>>>> b2fdf41e0448519d2db18d16c9022cc27b10ed98

# library(caret)
# set.seed(123)
# trainIndex <- createDataPartition(df_train$Variant, p = .8, 
#                                   list = FALSE, 
#                                   times = 1)
# 
# df_split_train = df_train[trainIndex, ]
# df_split_valid = df_train[-trainIndex, ]



# use files split by caret ------------------------------------------------

str(df_split_train)
str(df_split_valid)
str(df_split_test)


# Data Prep ---------------------------------------------------------------

# Prep training data
# split response variable from dataset
train_labels = df_split_train[, 1]

# train_data = data.matrix(df_train[, -1])
# create dummy variable for categorical variables
<<<<<<< HEAD
fct_col = model.matrix( ~ Brand + Model_Grp + Variant +
                          Elect.R + Eng.R + Ext.R + Gearb.R +
                          Int.R + UC.R +Struct.R + Flood.R + Mileage_yr_Grp - 1, 
                        data=df_split_train
                      ) # Note if N/A available, errors may occur, i.e. random 0/1, soln is to code NA to smth else
=======

factor_list = sapply(df_split_train[, -1], is.factor)
Factor_names = names(factor_list[factor_list == TRUE])
NonFactor_names = names(factor_list[factor_list == FALSE])

category_formula = as.character()
for (i in Factor_names){
  category_formula = paste(i, category_formula)
}
category_formula = trimws(category_formula)
category_formula = gsub(" ", "+", category_formula)

Category_col = model.matrix(as.formula(paste("~", category_formula, "-1")), 
  data=df_split_train)[, -1]
NonCategory_col = df_split_train[NonFactor_names]

train_data = cbind(Category_col, NonCategory_col)
typeof(train_data)

typeof(list(train_data, train_labels))
# Note if N/A available, errors may occur, i.e. random 0/1, soln is to code NA to smth else
# fct_col = model.matrix( ~ Brand + Model_Grp + Variant +
#                           Elect.R + Eng.R + Ext.R + Gearb.R +
#                           Int.R + UC.R +Struct.R + Flood.R + Mileage_yr_Grp - 1, 
#                         data=df_split_train)[, -1] 
>>>>>>> b2fdf41e0448519d2db18d16c9022cc27b10ed98

# alternate soln for dummy var
# binom <- data.frame(data=runif(1e5),type=sample(0:4,1e5,TRUE))
#for(t in unique(binom$type)) {
#  binom[paste("type",t,sep="")] <- ifelse(binom$type==t,1,0)
#}
# head(binom)

# problem: col 19:27 why have NAs shud be 0
num_col = as.matrix(sapply(df_split_train[, c(6, 7, 8, 9, 19:27)], as.numeric))
num_col_yr = as.numeric(df_split_train$Manf.Yr) # manf yr prepped as factored

# merge data in matrix form
train_data = cbind(fct_col, num_col, num_col_yr)

# Prep validation data
# split response variable from dataset
test_labels = df_split_valid[, 1]

fct_col_valid = model.matrix( ~ Brand + Model_Grp + Variant +
                          Elect.R + Eng.R + Ext.R + Gearb.R +
                          Int.R + UC.R +Struct.R + Flood.R + Mileage_yr_Grp - 1, 
                        data=df_split_valid)

num_col_valid = as.matrix(sapply(df_split_valid[, c(6, 7, 8, 9, 19:27)], as.numeric))
num_col_yr_valid = as.numeric(df_split_valid$Manf.Yr)

test_data = cbind(fct_col_valid, num_col_valid, num_col_yr_valid) # merge data in matrix form

# Data Pre-Processing -----------------------------------------------------


# Normalize data (suggested by keras rstudio), for faster convergence?
# Test data not used when calculating mean / std
# dimnames(train_data)[[1]] <- NULL; wont work for NULL
rownames(train_data) <- NULL
rownames(test_data) <- NULL

train_data <- scale(train_data) # do we need to scale binary? end result is the same, converted 0/1 to some number x/y

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

colnames(train_data)
ncol(train_data)
ncol(test_data)


# Build Model -------------------------------------------------------------

# create metric using backend tensor functions
metric_rmse <- custom_metric("rmse", 
                                  function(y_true, y_pred) {k_sqrt(k_mean((y_pred-y_true)^2))})

build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 32, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error", metric_rmse)
  )
  
  model
}

model <- build_model()
model %>% summary()

# train model

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 100

# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 500,
  #validation_split = 0.2, # use last 20% of data only
  validation_data = list(test_data, test_labels),
  verbose = 0,
  callbacks = list(print_dot_callback)
)

# visualize model's training progress
library(ggplot2)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(ylim = c(0, 5000))

# valiadation errors increased after ~100 epochs

# set stopping criteria

# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

model <- build_model()

history <- model %>% fit(
  train_data,
  train_labels,
  epochs = 500,
  #validation_split = 0.2,
  validation_data = list(test_data, test_labels),
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 5000))
# model stop at avg error of 1.5k

# check performance vs test set (validation set)

c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 0))
paste0("Mean absolute error on test set: $", sprintf("%.2f", mae))

# predictions for test (validation) set (Nid to update to prediction set)

test_predictions <- model %>% predict(test_data)
test_predictions
str(test_data)
test_labels
colnames(train_data)

test_data$Brand <- names(test_data[1:5])[max.col(test_data[1:5])]

# Try over/under fit models -----------------------------------------------

smaller_model <- 
  keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", 
              input_shape = dim(train_data)[2]) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

smaller_model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_absolute_error")
)

smaller_model %>% summary()

smaller_history <- smaller_model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  #batch_size = 512,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

bigger_model <- 
  keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu",
              input_shape = dim(train_data)[2]) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 1)

bigger_model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_absolute_error")
)

bigger_model %>% summary()

bigger_history <- bigger_model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  #batch_size = 512,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

# plot training & validation loss for different models
library(tibble)
library(tidyr)

compare_cx <- data.frame(
  baseline_train = history$metrics$loss,
  baseline_val = history$metrics$val_loss,
  smaller_train = smaller_history$metrics$loss,
  smaller_val = smaller_history$metrics$val_loss,
  bigger_train = bigger_history$metrics$loss,
  bigger_val = bigger_history$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_cx, aes(x = rowname, y = value, color = type)) +
  geom_line(size=1.5) +
  xlab("epoch") +
  ylab("loss")

# note our validation errors for both base & bigger models is flat, weird? or can fit more?
# typically validation errors shud increase if overfit


# Add weight regularization -----------------------------------------------

l2_model <- 
  keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", 
              input_shape = dim(train_data)[2],
              kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  # every coefficient in the weight matrix of the layer will add 0.001 * weight_coefficient_value to the total loss
  # shud penalize overfitting
  layer_dense(units = 32, activation = "relu",
              kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dense(units = 1)

l2_model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_absolute_error")
)

l2_history <- l2_model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  #batch_size = 512,
  validation_split = 0.2,
  #validation_data = list(test_data, test_labels),
  verbose = 2
)

compare_cx <- data.frame(
  baseline_train = history$metrics$loss,
  baseline_val = history$metrics$val_loss,
  l2_train = l2_history$metrics$loss,
  l2_val = l2_history$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_cx, aes(x = rowname, y = value, color = type)) +
  geom_line(size=1.5) +
  xlab("epoch") +
  ylab("loss")


# Add dropout -------------------------------------------------------------

dropout_model <- 
  keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", 
              input_shape = dim(train_data)[2]) %>%
  layer_dropout(0.6) %>% # fraction of features being zeroed out in layer
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(0.6) %>%
  layer_dense(units = 1)

dropout_model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_absolute_error")
)

dropout_history <- dropout_model %>% fit(
  train_data,
  train_labels,
  epochs = 500,
  #batch_size = 512,
  #validation_split = 0.2,
  validation_data = list(test_data, test_labels),
  verbose = 0,
  callbacks = list(print_dot_callback)
)

compare_cx <- data.frame(
  baseline_train = history$metrics$loss,
  baseline_val = history$metrics$val_loss,
  dropout_train = dropout_history$metrics$loss,
  dropout_val = dropout_history$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_cx, aes(x = rowname, y = value, color = type)) +
  geom_line(size=1.5) +
  xlab("epoch") +
  ylab("loss")


# Save & restore models ---------------------------------------------------

# Ref: https://keras.rstudio.com/articles/tutorial_save_and_restore.html

