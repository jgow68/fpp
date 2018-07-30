# data = read.csv("ConsolAD_Apr18.csv")
data = read.csv("ConsolAD_Jun18.csv")

# Extract and form df -----------------------------------------------------

str(data)

training_data = cbind.data.frame(data$Reserve.Price, data$Auction.Date, data$Auction.Year, 
                                 data$Auction.Month, data$Auction.Quarter,
                                 data$Brand, data$Model.Group,
                                 data$Live.Auction...X.Change,
                                 data$Final.bid.price,
                                 data$Adj..Var., # adj. in Excel
                                 data$Adj..PG, data$Adj..ERP, # need to import from PG
                                 data$Auct.Freq, # adj. in Excel
                                 data$Manufacturing.Year, data$Mileage, data$Vehicle.Age,
                                 data$Auction.Status, data$Vehicle.Location,
                                 # data$MUV.Grade, # first grading scheme
                                 # data$VAS.Exterior.Grade, data$VAS.Interior.Grade, # second grading scheme
                                 # third grading scheme
                                 data$MOS.Electrical.Rating, data$MOS.Engine.Rating, data$MOS.Exterior.Rating,
                                 data$MOS.Gearbox.Rating, data$MOS.Interior.Rating, data$MOS.Undercarriage.Rating,
                                 data$MOS.Structure.Rating, data$MOS.Flood.Rating
                                  ) 
names(training_data) = c("Res.Price", "Date", "AuctY",
                         "AuctM", "AuctQ",
                         "Brand", "Model_Grp", 
                         "Live Auct/X-Chg",
                         "Final.Price",
                         "Variant",
                         "Price.Guide", "ERP",
                         'Auct.Freq',
                         "Manf.Yr", "Mileage", "Age",
                         "Auct.Stat", "Veh Loc",
                         # "Grade",
                         # "Ext", "Int",
                         "Elect.R", "Eng.R", "Ext.R",
                         "Gearb.R", "Int.R", "UC.R",
                         "Struct.R", "Flood.R"
)


training_data$AuctY = factor(training_data$AuctY, ordered = TRUE, levels=c(2013, 2014, 2015, 2016, 2017, 2018))
training_data$AuctM = factor(training_data$AuctM, ordered = TRUE, 
                             levels=c("Jan","Feb","Mar", "Apr","May","Jun", "Jul","Aug","Sep", "Oct","Nov","Dec")
                             )
training_data$AuctQ = factor(training_data$AuctQ, ordered = TRUE, levels=c("Q1", "Q2", "Q3", "Q4"))
levels(training_data$Manf.Yr)


training_data$Manf.Yr = as.numeric(levels(training_data$Manf.Yr))[training_data$Manf.Yr]
training_data = dplyr::filter(training_data, (Manf.Yr >= 2000) & !is.na(Manf.Yr))
training_data$Manf.Yr = factor(training_data$Manf.Yr, ordered = TRUE, 
                            levels=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                                     2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
                            )

training_data$Brand = as.factor(toupper(trimws(training_data$Brand)))
training_data$Model_Grp = as.factor(toupper(trimws(training_data$Model_Grp)))
training_data$Variant = as.factor(toupper(trimws(training_data$Variant)))
training_data$Auct.Stat = as.factor(toupper(trimws(training_data$Auct.Stat)))

training_data$Res.Price = as.numeric(gsub(",", "", levels(training_data$Res.Price)))[training_data$Res.Price]
training_data$Final.Price = as.numeric(gsub(",", "", levels(training_data$Final.Price)))[training_data$Final.Price]
training_data$Mileage = as.numeric(gsub(",", "", levels(training_data$Mileage)))[training_data$Mileage]
training_data$Date = as.Date(training_data$Date)

summary(training_data$Auct.Freq)

training_data$Flood.R = as.character(training_data$Flood.R)
training_data$Flood.R = ifelse(is.na(training_data$Flood.R), 0,
                               ifelse(training_data$Flood.R == "", 0,
                                      training_data$Flood.R)
)
training_data$Flood.R = as.factor(training_data$Flood.R)

training_data = droplevels(training_data)

summary(training_data)
str(training_data)

tail(training_data)

# Filter data

training_data = dplyr::filter(training_data, Auct.Stat == "SOLD")

# sapply(training_data, function(x) sum(is.na(x))) # check no. of NAs in each predictor

training_data = training_data[complete.cases(training_data), ]

# training_data = dplyr::filter(training_data, !is.na(Res.Price) & !is.na(Price.Guide) & 
#                                !is.na(Age) & !is.na(ERP) & !is.na(Mileage))

training_data$Mileage_yr = training_data$Mileage / training_data$Age

training_data = training_data %>% 
  mutate(Mileage_yr_Grp = factor(case_when((Mileage_yr <= 30000) ~ "<= 30k/yr",
                                           (Mileage_yr > 30001 & Mileage_yr < 40000) ~ "30-40k/yr",
                                           (Mileage_yr > 40000) ~ ">40k/yr",
                                           TRUE ~ "N/A")
                                 )
         )

# # # # # # # # # # # # # # # # # # # # # # # 
# # # # consider ordered veh condition # # # #
# # # # # # # # # # # # # # # # # # # # # # # 

# Merge certain conditions in insp. report Phase 3
training_data$Elect.R = as.character(training_data$Elect.R)
unique(training_data$Elect.R)
training_data$Elect.R[training_data$Elect.R %in% c("A", "B")] = "A/B"
training_data$Elect.R = as.factor(training_data$Elect.R)
levels(training_data$Elect.R)

training_data$Eng.R = as.character(training_data$Eng.R)
unique(training_data$Eng.R)
training_data$Eng.R[training_data$Eng.R %in% c("A", "B")] = "A/B"
training_data$Eng.R = as.factor(training_data$Eng.R)
plyr::count(training_data$Eng.R)

training_data$Ext.R = as.character(training_data$Ext.R)
unique(training_data$Ext.R)
training_data$Ext.R[training_data$Ext.R %in% c("A", "B")] = "A/B"
training_data$Ext.R = as.factor(training_data$Ext.R)
plyr::count(training_data$Ext.R)
table(train_p2$Ext.R, train_p2$Variant)

training_data$Gearb.R = as.character(training_data$Gearb.R)
unique(training_data$Gearb.R)
training_data$Gearb.R[training_data$Gearb.R %in% c("A", "B")] = "A/B"
training_data$Gearb.R = as.factor(training_data$Gearb.R)
levels(training_data$Gearb.R)

training_data$Int.R = as.character(training_data$Int.R)
unique(training_data$Int.R)
training_data$Int.R[training_data$Int.R %in% c("A", "B")] = "A/B"
training_data$Int.R = as.factor(training_data$Int.R)
levels(training_data$Int.R)

training_data$UC.R = as.character(training_data$UC.R)
unique(training_data$UC.R)
training_data$UC.R[training_data$UC.R %in% c("A", "B")] = "A/B"
training_data$UC.R = as.factor(training_data$UC.R)
levels(training_data$UC.R)

training_data$Struct.R = as.character(training_data$Struct.R)
unique(training_data$Struct.R)
training_data$Struct.R[training_data$Struct.R %in% c("A", "B")] = "A/B"
training_data$Struct.R = as.factor(training_data$Struct.R)
levels(training_data$Struct.R)



# # # # # # # # # # # # # # # #
# # # # #  Scaling TBD # # # # # 
# # # # # # # # # # # # # # # #

# library("scales")

# scale final price, manf yr, mileage, age
#training_data$Final.Price = rescale(training_data$Final.Price, to=c(0,1))
#training_data$Price.Guide = rescale(training_data$Price.Guide, to=c(0,1))
#training_data$ERP = rescale(training_data$ERP, to=c(0,1))
#training_data$Manf.Yr = rescale(training_data$Manf.Yr, to=c(0,1))
#training_data$Mileage = rescale(training_data$Mileage, to=c(0,1))
#training_data$Age = rescale(training_data$Age, to=c(0,1))


# Perodua -----------------------------------------------------------------

train_p2 = dplyr::filter(training_data, Brand == "PERODUA" & !is.na(Variant) &
                           !(Elect.R %in% c("", "N")) & !(Eng.R %in% c("", "N")) & !(Ext.R %in% c("","N")) & 
                           !(Gearb.R %in% c("", "N")) & !(Int.R %in% c("", "N")) & !(UC.R %in% c("", "N")) &
                           !(Struct.R %in% c("", "N"))
)

train_p2 = droplevels(train_p2)

train_p2 = train_p2 %>% 
  group_by(Variant) %>%
  dplyr::filter(n()>10) %>%
  as.data.frame()

# Drop specific predictors
drops = c("Brand", "Date", "AuctM", "AuctQ", "Live Auct/X-Chg", "Age", "Mileage", "Mileage_yr", "Auct.Stat", "Veh Loc")

train_p2 = train_p2[, !names(train_p2) %in% drops]

colnames(train_p2)
str(train_p2)


# lm - p2 -----------------------------------------------------------------

# # # # # # # # # # # # # # # 
# # # # # # Tricky # # # # # # 
# # # # # # # # # # # # # # # 

# if factor levels dropped before model training, unable to est when factor appear in testing set, 
# esp. for veh conditions
to_drop = as.character()
for (i in 7:(length(colnames(train_p2)))){
  test = plyr::count(train_p2, colnames(train_p2)[i])
  if (length(test$freq) == 1){ 
    # alternative we can add: if length(test$freq) > 1, then test$freq[1]/test$freq[2] < 5% then remove
    to_drop = c(paste(colnames(train_p2)[i]), to_drop)
  }
}
to_drop

drops = c(to_drop, drops)
train_p2 = train_p2[, !names(train_p2) %in% drops]


CV_values_lm_full = vector(length=1)
pred_p2 = vector(length=1)
n=nrow(train_p2)
for(i in 1){
  cvi=0
  for(j in 1:n){
    # k = ((j-1)*floor(n/5)+1):(j*floor(n/5));
    set_model = lm(Res.Price ~ . , data = train_p2[-j,]) 
    yhat = predict(set_model, newdata=train_p2[j,])
    cvi = cvi + sqrt((yhat - train_p2[j, 1])^2) # Note k in train_p2[j, k] must refer to reserve price
    pred_p2[j] = yhat
  }
  CV_values_lm_full[i] = cvi/ n
}

test_model_p2 = lm(Res.Price ~., data=train_p2)
summary(test_model_p2)

# Error message:
# factor Ext.R has new level E
# likely due to sparse data in veh conditions
# predictions not same length with data

CV_values_lm_full
p2_tbl = cbind("Actual Reserve Price" = train_p2$Res.Price, 
               "Actual Final Bid Price" = train_p2$Final.Price, 
               "Est. Reserve Price" = pred_p2)


write.csv(p2_tbl, file = "p2.csv")


# mboost - p2 -----------------------------------------------------------

library(mboost)

str(train_p2)

boost_control(mstop = 200, # initial number of boosting iterations, default: 100
              nu = 0.05, # step length, default 0.1
              trace = TRUE) # default = FALSE

# centering of numerical predictors
train_p2$FP_ctr = scale(train_p2$Final.Price, center = TRUE)
train_p2$PG_ctr = scale(train_p2$Price.Guide, center = TRUE)
train_p2$ERP_ctr = scale(train_p2$ERP, center = TRUE)

drops = c("Brand", "Date", "AuctM", "AuctQ", "Live Auct/X-Chg", "Age", "Mileage", "Mileage_yr", "Auct.Stat", "Veh Loc", 
          'Final.Price', 'Price.Guide', 'ERP')

train_p2 = train_p2[, !names(train_p2) %in% drops]


preds <- names(train_p2[, names(train_p2) != "Res.Price"]) ## names of predictors
fm <- as.formula(paste("Res.Price ~", paste(preds, collapse = "+"))) ## build formula
fm

glmboost_p2 = glmboost(fm, data=train_p2)

coef(glmboost_p2, off2int = TRUE) # which = "" show even unselected predictors
# plot(glmboost_p2, off2int = TRUE)
plot(glmboost_p2, ylim = range(coef(glmboost_p2)[-1])) # plot without intercept

sum(abs(train_p2$Res.Price - fitted(glmboost_p2)))/length(train_p2$Res.Price)
sqrt(sum((train_p2$Res.Price - fitted(glmboost_p2))^2)/length(train_p2$Res.Price))

p2_glmboost_tbl = cbind('Actual Res. Price' = train_p2$Res.Price, 
                        'Est Res. Price - glm' = fitted(glmboost_p2), 
                        'Actual - Est Res. Price (a)' = train_p2$Res.Price - fitted(glmboost_p2),
                        'abs(a)' = abs(train_p2$Res.Price - fitted(glmboost_p2)),
                        'a^2' = ((train_p2$Res.Price - fitted(glmboost_p2))^2)
                        )

write.csv(p2_glmboost_tbl, file = 'p2_glmboost.csv')

gamboost_p2 = gamboost(fm, data=train_p2)

sum(abs(train_p2$Res.Price - fitted(gamboost_p2)))/length(train_p2$Res.Price)
sqrt(sum((train_p2$Res.Price - fitted(gamboost_p2))^2)/length(train_p2$Res.Price))


p2_gamboost_tbl = cbind('Actual Res. Price' = train_p2$Res.Price, 
                        'Est Res. Price - gam' = fitted(gamboost_p2), 
                        'Actual - Est Res. Price (a)' = train_p2$Res.Price - fitted(gamboost_p2),
                        'abs(a)' = abs(train_p2$Res.Price - fitted(gamboost_p2)),
                        'a^2' = ((train_p2$Res.Price - fitted(gamboost_p2))^2)
)
write.csv(p2_gamboost_tbl, file = 'p2_gamboost.csv')

test = gamboost(Res.Price ~ bols(AuctY) + bols(Model_Grp) + bols(Variant) + bols(Manf.Yr) + 
                  bols(Elect.R, Eng.R, Ext.R, Gearb.R, Int.R, UC.R, Struct.R, Flood.R) +
                  bols(Mileage_yr_Grp) + bbs(PG_ctr, by = ERP_ctr) + bbs(FP_ctr) + bbs(ERP_ctr, by = PG_ctr),
                data = train_p2)

sum(abs(train_p2$Res.Price - fitted(test)))/length(train_p2$Res.Price)
sqrt(sum((train_p2$Res.Price - fitted(test))^2)/length(train_p2$Res.Price))


coef(test)
extract(bols(train_p2$Manf.Yr))
length(train_p2$Manf.Yr)
attributes(train_p2$Manf.Yr)
length(train_p2$Manf.Yr)
bols(train_p2$Elect.R, train_p2$Eng.R)

z <- factor(1:3)
extract(bols(z))
attributes(z)


bbs(train_p2$PG_ctr)



# h2o - P2 ---------------------------------------------------------------------

df = train_p2

df$AuctY = factor(df$AuctY, ordered = FALSE)
df$Manf.Yr = factor(df$Manf.Yr, ordered = FALSE)
df$Mileage_yr_Grp = factor(df$Mileage_yr_Grp, ordered = FALSE)

library(h2o)
h2o.init(min_mem_size="2g", max_mem_size = "4g")
h2o.shutdown()

df <- as.h2o(df)

h2o.describe(df)

y <- 'Final.Price'
# y <- 'Res.Price'
x <- setdiff(names(df), y)

splits <- h2o.splitFrame(df, ratios = c(0.7, .15) , seed = 1)
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

aml_p2 <- h2o.automl(x = x,
                     y = y,
                     training_frame = train,
                     nfolds = 5,
                     keep_cross_validation_predictions = TRUE,
                     validation_frame = valid,
                     leaderboard_frame = test,
                     # exclude_algos = "GBM", # exclude_algos = c("GLM", "DeepLearning", "GBM", DRF", "StackedEnsemble"),
                     max_runtime_secs = 60, # max_models
                     seed = 1
                     # project_name = "p2_final_price"
                     )

print(aml_p2@leaderboard)


# get the top 6 models from the leaderboard
for (i in c(1:6)){
  assign(paste('model', i, sep = "_"), h2o.getModel(aml_p2@leaderboard[i, 1]))
}

# try ensemble

# issue 1

ensemble1 <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                validation_frame = valid, 

                                # model_id = "my_ensemble_p2",
                                base_models = list(model_1, model_2), #  model_3, model_4, model_5, model_6),
                                metalearner_algorithm = "gbm" # metalearner_algorithm = "AUTO" or "glm" or "gbm" or 'drf' or 'deeplearning'
                                # metalearner_params = list(ntrees = 100, max_depth = 2)
)


# h2o.getFrame(model_3@parameters$training_frame)

# Eval performance on a test set
perf_leader <- h2o.performance(model_1, newdata = test)
perf_ensemble <- h2o.performance(ensemble1, newdata = test)

# Compare performance
mrd_leader <- h2o.mean_residual_deviance(perf_leader)
mrd_ensemble <- h2o.mean_residual_deviance(perf_ensemble)

mrd_ensemble <= mrd_leader # ensemble performed worst

print(sprintf("Best Base-learner Perf:  %s", mrd_leader))
print(sprintf("Ensemble Perf:  %s", mrd_ensemble))

# Predictions

pred <- h2o.predict(ensemble1, df)
p2_h2o_est = as.vector(pred)

# predict reserve price
p2_h2o_tbl = cbind('Actual Res. Price' = train_p2$Res.Price, 
                   'Est Res. Price' = p2_h2o_est, 
                   'Final Bid Price' = train_p2$Final.Price,
                   'Actual - Est Res. Price (a)' = train_p2$Res.Price - p2_h2o_est,
                   'abs(a)' = abs(train_p2$Res.Price - p2_h2o_est),
                   'a^2' = ((train_p2$Res.Price - p2_h2o_est))^2
                   )

# predict final bid price
p2_h2o_tbl = cbind('Actual Res. Price' = train_p2$Res.Price, 
                   'Est Final Price' = p2_h2o_est, 
                   'Final Bid Price' = train_p2$Final.Price,
                   'Actual - Final Res. Price (a)' = train_p2$Final.Price - p2_h2o_est,
                   'abs(a)' = abs(train_p2$Final.Price - p2_h2o_est),
                   'a^2' = ((train_p2$Final.Price - p2_h2o_est))^2
)


write.csv(p2_h2o_tbl, file = 'p2_h2o.csv')


# save the model
# model_path_1 <- h2o.saveModel(object=aml_p2@leader, path=getwd(), force=TRUE)

# print(model_path_1)
# "C:\\Users\\70062559\\Documents\\Github Folder\\fpp\\GBM_grid_0_AutoML_20180724_101959_model_0"

# load the model
# saved_model_1 <- h2o.loadModel(model_path_1)
# saved_model_1@parameters

h2o.shutdown()


# h2o stacked ensemble - p2 -----------------------------------------------

nfolds <- 5

# 1. Generate a 2-model ensemble (GBM + RF)

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  distribution = "gaussian",
                  ntrees = 55,
                  max_depth = 6,
                  min_rows = 1,
                  learn_rate = 0.2,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)


# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          ntrees = 50,
                          nfolds = nfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

# Train a stacked ensemble using the GBM and RF above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "my_ensemble_binomial",
                                base_models = list(my_gbm, my_rf)
                                # metalearner_algorithm = "AUTO" or "glm" or "gbm" or 'drf' or 'deeplearning'
                                # metalearner_params = list(ntrees = 100, max_depth = 2)
                                )

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)
h2o.mean_residual_deviance(perf)
# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test)
perf_rf_test <- h2o.performance(my_rf, newdata = test)

baselearner_best_mrd_test <- min(h2o.mean_residual_deviance(perf_gbm_test), h2o.mean_residual_deviance(perf_rf_test))
ensemble_mrd_test <- h2o.mean_residual_deviance(perf)

print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_mrd_test)) # ensemble performed worst

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = test)
pred

# 2. Generate a random grid of models and stack them together

# GBM Hyperparamters

# sort by importance
ntrees_opts = seq(50, 1000, 25) # the more the better
learn_rate_opts <- seq(0.001, 0.01, 0.001) # the lower the better, but require more trees, 
# learn_rate_annealing=0.995 (reduction of learning rate with each additional tree)
max_depth_opts <- seq(1,20) # depths > 10, take longer time to train

sample_rate_opts <- seq(0.3, 1, 0.05) # usually 70-80%
col_sample_rate_opts <- seq(0.3, 1, 0.05)

# sample_rate_per_class for highly imbalanced classification datasets
min_rows_opts <- c(1,5,10,20,50,100)
col_sample_rate_per_tree_opts = seq(0.3, 1, 0.05)
# nbins_cats_opts = seq(100, 10000, 100) # for categorical predictors
# nbins for continuous / integer predictors

hyper_params <- list(ntrees = ntrees_opts,
                     learn_rate = learn_rate_opts,
                     max_depth = max_depth_opt,
                     min_rows = min_rows_opts,
                     sample_rate = sample_rate_opts,
                     col_sample_rate = col_sample_rate_opts,
                     col_sample_rate_per_tree = col_sample_rate_opts
                     # nbins_cats = nbins_cats_opts
                     )

search_criteria <- list(strategy = "RandomDiscrete",
                        max_runtime_secs = 600,
                        max_models = 100,
                        stopping_metric = "AUTO",
                        stoppping_tolerance = 0.00001,
                        stopping_rounds = 5,
                        seed = 1)

gbm_grid <- h2o.grid(algorithm = "gbm",
                     grid_id = "gbm_grid_binomial",
                     x = x,
                     y = y,
                     training_frame = train,
                     nfolds = nfolds,
                     validation_frame = test, # nfolds = 0
                     
                     # Gaussian is best for MSE loss, but can try 
                     # other distributions ("laplace", "quantile"):
                     # distribution="gaussian",
                     
                     # stop as soon as mse doesn't improve by 
                     # more than 0.1% on the validation set, 
                     # for 2 consecutive scoring events:
                     stopping_rounds = 2,
                     stopping_tolerance = 1e-3,
                     stopping_metric = "MSE",
                     
                     # how often to score (affects early stopping):
                     score_tree_interval = 100,
                     
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

gbm_sorted_grid <- h2o.getGrid(grid_id = "gbm_grid_binomial", sort_by = "mse")
print(gbm_sorted_grid)

best_model <- h2o.getModel(gbm_sorted_grid@model_ids[[1]])
summary(best_model)

# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "ensemble_gbm_grid_binomial",
                                base_models = gbm_grid@model_ids)


# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test)

# Compare to base learner performance on the test set
.getmrd <- function(mm) h2o.mean_residual_deviance(h2o.performance(h2o.getModel(mm), newdata = test))
baselearner_mrds <- sapply(gbm_grid@model_ids, .getmrd)

baselearner_best_mrd_test <- max(baselearner_mrds)
ensemble_mrd_test <- h2o.mean_residual_deviance(perf)

print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_mrd_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_mrd_test))

# Generate predictions on a test set (if neccessary)
pred <- h2o.predict(ensemble, newdata = df) # still poorer than AutoML GBM best model

p2_est_gbmgs = as.vector(pred)

p2_tbl_gbmgs = cbind('Actual Res. Price' = train_p2$Res.Price, 
                   'Est Res. Price - gam' = p2_est_gbmgs, 
                   'Final Bid Price' = train_p2$Final.Price,
                   'Actual - Est Res. Price (a)' = train_p2$Res.Price - p2_est_gbmgs,
                   'abs(a)' = abs(train_p2$Res.Price - p2_est_gbmgs),
                   'a^2' = ((train_p2$Res.Price - p2_est_gbmgs))^2
)

write.csv(p2_tbl_gbmgs, file = 'p2_gbmgs.csv')

# 3. Train several grids of models



# Filter Honda City by inspection period ----------------------------------------

train_city = dplyr::filter(training_data, Model_Grp=="CITY")
sapply(train_city, function(x) sum(is.na(x))) # check no. of NAs in each predictor

train_city$Variant = droplevels(train_city$Variant)
levels(train_city$Variant)
plyr::count(train_city, "Variant") # alot of variants un-identified

# 3 different grading scheme over the different time period
# split data based on 3 inspection grading periodss
train_city1 = dplyr::filter(train_city, Grade != "")
train_city2 = dplyr::filter(train_city, Ext != "", Int != "")
train_city3 = dplyr::filter(train_city, Elect.R != "", Eng.R != "", Ext.R != "", Gearb.R != "", Int.R != "",
                            UC.R != "", Struct.R != "")

dim(train_city1) # 19 obs
dim(train_city2) # 572 obs
dim(train_city3) # 223 obs

plyr::count(train_city2, "Variant") # totally not categorized by variants yet
plyr::count(train_city3, "Variant") # some obs not categeorized due to missing variants >> no Price Guide / ERP

str(train_city3)

# # # NOT REQUIRED (TRIM BLANKS in Excel) # # # 
train_city3$Variant = as.character(train_city3$Variant)
unique(train_city3$Variant)
train_city3$Variant[train_city3$Variant %in% c("E ", "E  ")] = "E"
train_city3$Variant = as.factor(train_city3$Variant)
levels(train_city3$Variant)
# # # # # # # # # # # # # # # # # #



# Combine Grading Scheme --------------------------------------------------

# Phase 1 insp. report
train_city1$Grade = as.character(train_city1$Grade)
train_city1$Grade[train_city1$Grade %in% c("A", "B", "B ", "A+")] = "A/B"
train_city1$Grade[train_city1$Grade %in% c("C", "C ","C+")] = "C"
train_city1$Grade = as.factor(train_city1$Grade)
levels(train_city1$Grade)

# Phase 2 insp. report
train_city2$Ext = as.character(train_city2$Ext)
unique(train_city2$Ext)
train_city2$Ext[train_city2$Ext %in% c("A", "B")] = "A/B"
train_city2$Ext[train_city2$Ext %in% c("C+", "C", 'C+.W2')] = "C"
train_city2$Ext = as.factor(train_city2$Ext)

train_city2$Int = as.character(train_city2$Int)
unique(train_city2$Int)
train_city2$Int[train_city2$Int %in% c("A", "B")] = "A/B"
train_city2$Int = as.factor(train_city2$Int)

levels(train_city2$Ext)
levels(train_city2$Int)

# Phase 3 insp. report 
train_city3$Elect.R = as.character(train_city3$Elect.R)
unique(train_city3$Elect.R)
train_city3$Elect.R[train_city3$Elect.R %in% c("A", "B")] = "A/B"
train_city3$Elect.R = as.factor(train_city3$Elect.R)
levels(train_city3$Elect.R)

train_city3$Eng.R = as.character(train_city3$Eng.R)
unique(train_city3$Eng.R)
train_city3$Eng.R[train_city3$Eng.R %in% c("A", "B")] = "A/B"
train_city3$Eng.R = as.factor(train_city3$Eng.R)
levels(train_city3$Eng.R)

train_city3$Ext.R = as.character(train_city3$Ext.R)
unique(train_city3$Ext.R)
train_city3$Ext.R = as.factor(train_city3$Ext.R)

train_city3$Gearb.R = as.character(train_city3$Gearb.R)
unique(train_city3$Gearb.R)
train_city3$Gearb.R[train_city3$Gearb.R %in% c("A", "B")] = "A/B"
train_city3$Gearb.R = as.factor(train_city3$Gearb.R)
levels(train_city3$Gearb.R)

train_city3$Int.R = as.character(train_city3$Int.R)
unique(train_city3$Int.R)
train_city3$Int.R = as.factor(train_city3$Int.R)

train_city3$UC.R = as.character(train_city3$UC.R)
unique(train_city3$UC.R)
train_city3$UC.R[train_city3$UC.R %in% c("A", "B")] = "A/B"
train_city3$UC.R = as.factor(train_city3$UC.R)
levels(train_city3$UC.R)

train_city3$Struct.R = as.character(train_city3$Struct.R)
unique(train_city3$Struct.R)
train_city3$Struct.R[train_city3$Struct.R %in% c("A", "B")] = "A/B"
train_city3$Struct.R = as.factor(train_city3$Struct.R)
levels(train_city3$Struct.R)

# flood rating ignored



# Filter out City model variants for model testing ------------------------------

City_E = dplyr::filter(train_city3, Variant == 'E')
summary(City_E)



# Case: Honda City - Plots -----------------------------------------------------------
library(ggplot2)

# plot Reserve Price vs Date of Auction
# Conclusion: veh reserve price does not fluctuate over years?? visually?
ggplot(train_city, aes(Date, Res.Price)) +
  geom_point(aes(color = Age)) +
  scale_color_gradientn(colours = terrain.colors(10))


# 1st phase ignore due to sparse obs

# 2nd phase - 3d plot
plyr::count(train_city2, "Ext")
plyr::count(train_city2, "Int")

library(plotly)

p = plot_ly(train_city2, x = ~Int, y = ~Ext, z = ~Res.Price,
            marker = list(color = ~Age, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(categoryorder = "array",
                                   categoryarray = c("A/B", "C", "D", "E"),
                                   title = "Interior"),
                      yaxis = list(categoryorder = "array",
                                   categoryarray = c("A/B", "C", "D"),
                                   title = "Exterior"),
                      zaxis = list(title = "Reserve Price")),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Vehicle Age',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
           ))

p

# 3rd phase
library(GGally)
plot3 = ggpairs(train_city3[, -c(1:4, 6, 10:13, 20)])
plot3

str(City_E)
ggpairs(City_E[, -c(1:4, 10:13, 20)])



# PCA / FA ----------------------------------------------------------------

# Ref: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/

# no. of predictors if not alot -> maybe not required to do PCA/FA
# test for multicollinearity first

# do PCA for quantitative data then do MCA (multiple correspondence analysis) for categorical data
# PCA not recommended for categorical data (where levels > 2)
# our categorical data is ordinal (grade A/B, C, D)

# can consider convert factor levels to dummy variable then test as numerical
# head(model.matrix(~ Ext + Int - 1, data=train_city2))
# Ref: https://stats.stackexchange.com/questions/18084/collinearity-between-categorical-variables

# use FAMD (factor analysis on mixed data) for the mix of quant and qual data

# install.packages("FactoMineR")
# install.packages("factoextra")
library(FactoMineR)
library(factoextra)

str(train_city3)
train_city3_active = train_city3[ , c(16:22)]
head(train_city3_active)
mca1 = MCA(train_city3_active)
mca1$eig


# not sure if the response variable (reserve price) should be incorporated in the factor analysis
train_city2_active = train_city2[, c(6:9, 12:13)] # set active variables to be used in MCA
head(train_city2_active)

# MCA can only be applied to categorical variables, here Interior and Exterior Grade (maybe useful in reducing the no. of levels)
levels(train_city2_active$Int) # 3 levels
levels(train_city2_active$Int) # 4 levels

res.mca = MCA(train_city2_active[,c(5:6)])
print(res.mca)
get_eigenvalue(res.mca)
res.mca$var

# do FAMD
res.famd = FAMD(train_city2_active, graph=FALSE)
print(res.famd)
res.famd$eig

get_eigenvalue(res.famd)
fviz_screeplot(res.famd)
var.famd = get_famd_var(res.famd)$coord
fviz_famd_var(res.famd)

# how to apply transformed dim to regresssion?






# Linear regression -------------------------------------------------------

# Issue: final price n/a for some data
# Option 1: Filter for vehicles sold only then re-run model
# Option 2: Do not use final price as predictor

City_E = dplyr::filter(train_city3, Variant == 'E') # Final.Price != 'NA'
str(City_E)
summary(City_E)

plot(Res.Price ~ Mileage, data=City_E)
plot(Res.Price ~ Age, data=City_E)
plot(Res.Price ~ Price.Guide, data=City_E)
plot(Res.Price ~ ERP, data=City_E)

# test for multicollinearity
cor(City_E[,c(7:11)]) # manf yr highly corr with age (Remove 1), price guide ~ ERP (might have issues)
# might be multicollinearity in vehicle conditions -->> try ridge / lasso regression??

City_E_model_full = lm(Res.Price ~ Price.Guide + ERP + Mileage + Age + 
                    Elect.R + Eng.R + Ext.R + Gearb.R + Int.R + UC.R + Struct.R ,data = City_E
)

alias(City_E_model)
summary(City_E_model) # singularity?? # uc rating for D has n/a coefficients? perfect collineaerity?
plot(resid(City_E_model))
# vif(City_E_model)

# Try remove UC rating
City_E_model_rem_UC = lm(Res.Price ~ Price.Guide + ERP + Mileage + Age + 
                    Elect.R + Eng.R + Ext.R + Gearb.R + Int.R + Struct.R ,data = City_E
)
summary(City_E_model_rem_UC)

# fit model without price guide / ERP
City_E_model_rem_price = lm(Res.Price ~ Mileage + Age + 
                           Elect.R + Eng.R + Ext.R + Gearb.R + Int.R + UC.R + Struct.R ,data = City_E
)
summary(City_E_model_rem_price)


# prediction from a rank-deficient fit may be misleading
length(City_E_model$coefficients) > City_E_model$rank


pred_City_E_full = predict(City_E_model_full, newdata=City_E)
sqrt(sum((pred_City_E_full-City_E$Res.Price)^2) / length(pred_City_E_full)) # average s.e. is 764.83

pred_City_E_rem_UC = predict(City_E_model_rem_UC, newdata=City_E)
sqrt(sum((pred_City_E_rem_UC-City_E$Res.Price)^2) / length(pred_City_E_rem_UC)) # average s.e. is 786.79

pred_City_E_rem_price = predict(City_E_model_rem_price, newdata=City_E)
sqrt(sum((pred_City_E_rem_price-City_E$Res.Price)^2) / length(pred_City_E_rem_price)) # average s.e. is 783.36




# for output purposes
cbind.data.frame(pred_City_E_full, City_E$Res.Price, as.integer(sqrt((pred_City_E_full-City_E$Res.Price)^2)))

cbind.data.frame(pred_City_E_rem_price, City_E$Res.Price, as.integer(sqrt((pred_City_E_rem_price-City_E$Res.Price)^2)))



# Linear Regression CV ----------------------------------------------------------------------

CV_values_lm_full = vector(length=1)
n=nrow(City_E)
for(i in 1){
  cvi=0
  for(j in 1:n){
    # k = ((j-1)*floor(n/5)+1):(j*floor(n/5));
    set_model = lm(Res.Price ~ Price.Guide + ERP + Mileage + Age +
                     Elect.R + Eng.R + Ext.R + Gearb.R + Int.R + UC.R + Struct.R,
                   data = City_E[-j, ]) 
    yhat = predict(set_model, newdata=City_E[j,])
    cvi = cvi + sqrt((yhat - City_E[j, 5 ])^2)
  }
  CV_values_lm_full[i] = cvi/ n
}

CV_values_lm_full

CV_values_lm_rem_price = vector(length=1)
n=nrow(City_E)
for(i in 1){
  cvi=0
  for(j in 1:n){
    # k = ((j-1)*floor(n/5)+1):(j*floor(n/5));
    set_model = lm(Res.Price ~Mileage + Age +
                     Elect.R + Eng.R + Ext.R + Gearb.R + Int.R + UC.R + Struct.R,
                   data = City_E[-j, ]) 
    yhat = predict(set_model, newdata=City_E[j,])
    cvi = cvi + sqrt((yhat - City_E[j, 5 ])^2)
  }
  CV_values_lm_rem_price[i] = cvi/ n
}

CV_values_lm_rem_price





# Ridge / Lasso Regression ------------------------------------------------
library(glmnet)
str(City_E)
head(City_E[, c(7:10, 16:22)])

x = model.matrix(~., data=City_E[, c(7:10, 16:22)])
y = City_E$Res.Price
cv.out = cv.glmnet(x,y, alpha=1)
plot(cv.out)
lambda_min = cv.out$lambda.min
lambda_lse = cv.out$lambda.1se
coef(cv.out, s=lambda_lse)

x_test = model.matrix(~., data=City_E[, c(7:10, 16:22)])
lasso_pred = predict(cv.out, newx= x_test, s=lambda_lse, type='response')

error_mean = sqrt(sum((y - lasso_pred)^2)/length(lasso_pred)) # rough gauge of s.e. ~1,400
mean(y) # 38,479
error_mean / mean(y) # 3.65%

# for output
cbind.data.frame(lasso_pred, City_E$Res.Price, as.integer(sqrt((lasso_pred-City_E$Res.Price)^2)))

# Time series regression --------------------------------------------------

library(fpp)

ts_city = ts(train_city)

ts_model_age = tslm(Res.Price ~ Age, data=ts_city)
summary(ts_model_age)

res1 = ts(resid(ts_model_age))
plot.ts(res1)
Acf(res1) # presence of autocorrelation?

