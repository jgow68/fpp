# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                       NOTE:                         #
# create file by copying table from Power BI          #
# Append PG & ERP to training data set before running #
#                                                     #
# training data & prediction data have different      #
# format                                              #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #


# data = read.csv("ConsolAD_Apr18.csv")
# data = read.csv("ConsolAD_Jun18.csv")
# data = read.csv('ConsolAD_Jul18.csv', encoding = 'UTF-8')
# testing fork

rm(data)

data = read.csv('ConsolAD_17Oct_2018.csv', encoding = 'UTF-8')
verify_data = read.csv("DisposalTemplate_31Oct.csv")

# Load libraries ----------------------------------------------------------

library(dplyr)
library(stringr)
library(ggplot2)
library(h2o)
library(quanteda)
library(caret)

# Extract and form df (Note: testing rmks) -----------------------------------------------------

training_data = cbind.data.frame(
  data$Reserve.Price, data$Final.bid.price,
  data$Auction.Date, data$Auction.Year, # note utf-8 file start format changed for Auction Date
  data$Auction.Month, data$Auction.Quarter,
  data$Brand, data$Model.Group,
  data$Auction.Platform, 
  # data$Live.Auction...X.Change,
  data$Variants.Standardization, 
  #data$Adj..Var., # adj. in Excel
  data$PG, data$ERP,
  #data$Adj..PG, data$Adj..ERP, # need to import from PG
  data$Auction.Freq..by.Auction.Session.,
  #data$Auct.Freq, # adj. in Excel
  data$Manufacturing.Year, data$Mileage, #data$Vehicle.Age,
  data$Auction.Status, data$Vehicle.Location,
  data$Capacity,
  # data$MUV.Grade, # first grading scheme
  # data$VAS.Exterior.Grade, data$VAS.Interior.Grade, # second grading scheme
  # third grading scheme
  data$MOS.Electrical.Rating, data$MOS.Engine.Rating, data$MOS.Exterior.Rating,
  data$MOS.Gearbox.Rating, data$MOS.Interior.Rating, data$MOS.Undercarriage.Rating,
  data$MOS.Structure.Rating, data$MOS.Flood.Rating,
  data$CommentsElec, data$CommentsEng, data$CommentsExt,
  data$CommentsGear, data$CommentsInt, data$CommentsUnderCarr, data$CommentsVehHistory)
  
  # data$Electrical_Rmks, data$Engine_Rmks, data$Exterior_Rmks,
  # data$Gearbox_Rmks, data$Interior_Rmks, data$UC_Rmks, data$VehHist_Rmks)

names(training_data) = c(
  "Res.Price", "Final.Price",
  "Date", "AuctY",
  "AuctM", "AuctQ",
  "Brand", "Model_Grp", 
  "Live Auct/X-Chg",
  "Variant",
  "Price.Guide", "ERP",
  'Auct.Freq',
  "Manf.Yr", "Mileage", # "Age",
  "Auct.Stat", "Veh Loc",
  # "Grade",
  # "Ext", "Int",
  'CC',
  "Elect.R", "Eng.R", "Ext.R",
  "Gearb.R", "Int.R", "UC.R",
  "Struct.R", "Flood.R",
  'Elec_rmk', 'Eng_rmk', 'Ext_rmk',
  'Gear_rmk', 'Int_rmk', 'UC_rmk', 'VH_rmk')

test_data = cbind.data.frame(
  verify_data$Proposed.Reserve.Price..RM., verify_data$Reg..No., as.factor(format(Sys.Date(), "%Y")),
  verify_data$Make, verify_data$Model,
  verify_data$Variants.Standardization,
  verify_data$Price.Guide..RM., verify_data$Approved.Final.ERP,
  verify_data$No..of.times.in.auction,
  verify_data$Year.Make, verify_data$Mileage,
  verify_data$Electrical, verify_data$Engine, verify_data$Ext.Grade,
  verify_data$Gearbox, verify_data$Interior, verify_data$Undercarriage,
  verify_data$Structure, verify_data$Flooded.Rating,
  verify_data$Electrical.comments, verify_data$Engine.comments, verify_data$Exterior.comments,
  verify_data$Gearbox.comments, verify_data$Interior.comments, verify_data$Undercarriage.comments, verify_data$vehicle.history)

names(test_data) = c(
  "Res.Price", "Reg.No.", 'AuctY',
  "Brand", "Model_Grp", 
  "Variant", 
  "Price.Guide", "ERP",
  'Auct.Freq', 
  "Manf.Yr", "Mileage",
  "Elect.R", "Eng.R", "Ext.R",
  "Gearb.R", "Int.R", "UC.R",
  "Struct.R", "Flood.R",
  'Elec_rmk', 'Eng_rmk', 'Ext_rmk',
  'Gear_rmk', 'Int_rmk', 'UC_rmk', 'VH_rmk')


# Filter for Sold units & Clean Data -----------------------------------------------------------
#library(dplyr)

dim(training_data)

name_columns = c('Brand', 'Model_Grp', 'Variant', 'Veh Loc', 'Auct.Stat')
training_data = training_data[complete.cases(training_data[name_columns]), ]


training_data[name_columns] = sapply(training_data[name_columns], function(x) toupper(trimws(x)))
training_data[name_columns] = lapply(training_data[name_columns], factor)

training_data = dplyr::filter(training_data, Auct.Stat == "SOLD")

# categorize Make_Popular into: "HOT", "COLD_PREMIUM", "COLD_NONPREMIUM"
training_data$Make_Popular = as.factor(
  dplyr::case_when(
    training_data$Brand %in% c("HONDA", "NISSAN", "TOYOTA", "PROTON", "PERODUA") ~ "HOT",
    training_data$Brand %in% c("ALFA ROMEO", "BMW", "MERCEDES BENZ", "VOLVO", "PORSCHE", "ROVER", 
                               "LAND ROVER", "MG ROVER", "MINI. UNITED KINGDOM", "AUDI", "JAGUAR", 
                               "INFINITI","LEXUS") ~ "COLD_PREMIUM",
    TRUE ~ "COLD_NONPREMIUM"))



num_columns = c("Res.Price", "Final.Price", 'AuctY', 'Manf.Yr', 'Price.Guide', 'ERP', 'Mileage', 'CC', 'Auct.Freq')

toNumFunction <- function (x) {
 tryCatch({
    if(class(x) == 'factor'){x = as.numeric(gsub(",", "", levels(x)))[x]} 
    else {x}},
  error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

training_data[colnames(training_data) %in% num_columns] <- sapply(
  training_data[colnames(training_data) %in% num_columns], toNumFunction)

training_data = dplyr::filter(training_data, (Manf.Yr >= 2000) & !is.na(Manf.Yr))

training_data$Date = as.Date(training_data$Date, format='%m/%d/%Y')

training_data$Age = as.integer(format(Sys.Date(), "%Y")) - training_data$Manf.Yr
training_data$Mileage_yr = training_data$Mileage / training_data$Age

training_data = training_data %>% 
  dplyr::mutate(Mileage_yr_Grp = factor(
    dplyr::case_when(
      (Mileage_yr <= 30000) ~ "<= 30k/yr",
      (Mileage_yr > 30001 & Mileage_yr < 40000) ~ "30-40k/yr",
      (Mileage_yr > 40000) ~ ">40k/yr", 
      TRUE ~ "N/A"), 
    ordered = TRUE, levels = c("<= 30k/yr", "30-40k/yr",">40k/yr")))

training_data = training_data[complete.cases(training_data[num_columns]), ]

# create ordered categorical variables (for dates)

training_data$AuctY = factor(training_data$AuctY, 
                             ordered = TRUE, levels=c(2013, 2014, 2015, 2016, 2017, 2018))

training_data$AuctM = factor(training_data$AuctM, 
                             ordered = TRUE, 
                             levels=c("Jan","Feb","Mar", "Apr","May","Jun", 
                                      "Jul","Aug","Sep", "Oct","Nov","Dec"))

training_data$AuctQ = factor(training_data$AuctQ, 
                             ordered = TRUE, 
                             levels=c("Q1", "Q2", "Q3", "Q4"))


training_data$Manf.Yr = factor(training_data$Manf.Yr, 
                               ordered = TRUE, 
                               levels=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                                        2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))



training_data = droplevels(training_data)
dim(training_data)

# Clean for test data & align factor levels -------------------------------

sapply(test_data, function(x) sum(is.na(x))) # check no. of NAs in each predictor
# test_data = test_data[complete.cases(test_data[, -1]), ] # filter for NAs except for reserve price col.


test_data[colnames(test_data) %in% name_columns] = sapply(
  test_data[colnames(test_data) %in% name_columns], function(x) toupper(trimws(x)))

test_data$Brand = factor(test_data$Brand, levels =  levels(training_data$Brand))
test_data$Model_Grp = factor(test_data$Model_Grp, levels =  levels(training_data$Model_Grp))
test_data$Variant = factor(test_data$Variant, levels =  levels(training_data$Variant))

# for (j in name_toupper){
#   print(j)
#   tryCatch({
#     training_data[, paste0(j)] = as.factor(toupper(trimws(training_data[, paste0(j)])))
#     test_data[, paste0(j)] = as.factor(toupper(trimws(test_data[, paste0(j)])))},
#   error = function(e){cat("ERROR :",conditionMessage(e), "\n")})}
# check if can work for unavail columns for test data


test_data[colnames(test_data) %in% num_columns] <- sapply(
  test_data[colnames(test_data) %in% num_columns], toNumFunction)

## Alternate method using for loop
# for (j in numerical_columns){
#   print(j)
#   tryCatch({
#     if(class(test_data[, paste0(j)]) == 'factor'){
#       test_data[, paste0(j)] = as.numeric(
#         gsub(",", "", levels(test_data[, paste0(j)])))[test_data[, paste0(j)]]} 
#     else{NULL}}, 
#   error = function(e){cat("ERROR :",conditionMessage(e), "\n")})}

test_data$AuctY = factor(test_data$AuctY, levels = levels(training_data$AuctY), ordered = TRUE)

test_data$Age = as.integer(format(Sys.Date(), "%Y")) - test_data$Manf.Yr
test_data$Auct.Freq = test_data$Auct.Freq + 1 # note template auction freq is before listing
test_data$Mileage_yr = test_data$Mileage / test_data$Age

test_data = test_data %>% 
  dplyr::mutate(Mileage_yr_Grp = dplyr::case_when(
    (Mileage_yr <= 30000) ~ "<= 30k/yr",
    (Mileage_yr > 30001 & Mileage_yr < 40000) ~ "30-40k/yr",
    (Mileage_yr > 40000) ~ ">40k/yr",
    TRUE ~ "N/A"))
test_data$Mileage_yr_Grp = factor(test_data$Mileage_yr_Grp, 
  levels = levels(training_data$Mileage_yr_Grp), ordered = TRUE)

test_data$Make_Popular = dplyr::case_when(
  test_data$Brand %in% c("HONDA", "NISSAN", "TOYOTA", "PROTON", "PERODUA") ~ "HOT",
  test_data$Brand %in% c("ALFA ROMEO", "BMW", "MERCEDES BENZ", "VOLVO", "PORSCHE", "ROVER", 
                         "LAND ROVER", "MG ROVER", "MINI. UNITED KINGDOM", "AUDI", "JAGUAR", 
                         "INFINITI","LEXUS") ~ "COLD_PREMIUM",
  TRUE ~ "COLD_NONPREMIUM")
test_data$Make_Popular = factor(test_data$Make_Popular, levels = levels(training_data$Make_Popular))

test_data$Manf.Yr = factor(test_data$Manf.Yr, levels = levels(training_data$Manf.Yr), ordered = TRUE)

# Clean & Filter vehicle condition rating (potentially removed in future)  --------

training_data[condition_rating] = sapply(training_data[condition_rating], function(x) as.character(x))
test_data[condition_rating] = sapply(test_data[condition_rating], function(x) as.character(x))

condition_rating = colnames(training_data)[grep('\\.[R]', colnames(training_data))]

# for (j in condition_rating){ # convert veh ratings to character
#   print(j)
#   training_data[, paste0(j)] = as.character(training_data[, paste0(j)])
#   test_data[, paste0(j)] = as.character(test_data[, paste0(j)])}

training_data = dplyr::filter(training_data,
  !is.na(Variant) &
  !(Elect.R %in% c(0, "", "N")) & !(Eng.R %in% c(0, "", "N")) & !(Ext.R %in% c(0, "","N")) & 
  !(Gearb.R %in% c(0, "", "N")) & !(Int.R %in% c(0, "", "N")) & !(UC.R %in% c(0, "", "N")) &
  !(Struct.R %in% c(0, "", "N")))


cat_ABFun <- function (x) {
  if_else(condition = x %in% c("A", "B"), true = "A/B", false = x)}
      
cat_columns = condition_rating[!(condition_rating %in% c('Flood.R'))]

training_data[cat_columns] <- sapply(training_data[cat_columns], cat_ABFun)
training_data[cat_columns] <- lapply(
  training_data[cat_columns], function (x) {factor(x, levels = c("A/B", 'C','D','E'))})

test_data[cat_columns] <- sapply(test_data[cat_columns], cat_ABFun)
test_data[cat_columns] <- lapply(
  test_data[cat_columns], function (x) {factor(x, levels = c("A/B", 'C','D','E'))})

# for (j in condition_rating[condition_rating != c("Flood.R")]){ # categorize A & B into A/B
#   print(j)
#   training_data[ , paste0(j)][training_data[, paste0(j)] %in% c("A", "B")] = "A/B"
#   training_data[ , paste0(j)] = as.factor(training_data[ , paste0(j)])
#   test_data[ , paste0(j)][test_data[, paste0(j)] %in% c("A", "B")] = "A/B"
#   test_data[ , paste0(j)] = as.factor(test_data[ , paste0(j)])}

# Clean Flood Rating: alot of NAs and 0
training_data$Flood.R = ifelse(is.na(training_data$Flood.R), 0,
                               ifelse(training_data$Flood.R == "", 0,
                                      training_data$Flood.R))
training_data$Flood.R = as.factor(training_data$Flood.R)

test_data$Flood.R = ifelse(is.na(test_data$Flood.R), 0,
                               ifelse(test_data$Flood.R == "", 0,
                                      test_data$Flood.R))
test_data$Flood.R = factor(test_data$Flood.R, levels = levels(training_data$Flood.R))

# Note: 
# consider ordered vehicle conditions


# clean vehicle condition comments

for (j in condition_comments){ # convert veh comments to character
  print(j)
  training_data[, paste0(j)] = as.character(training_data[, paste0(j)])
  test_data[, paste0(j)] = as.character(test_data[, paste0(j)])}

condition_comments = colnames(training_data)[grep('\\_[rmk]', colnames(training_data))]

dim(training_data)
dim(test_data)

str(training_data)
str(test_data)


# Text: Features Selection ------------------------------------------------

# library(quanteda)
quanteda_options("threads" = 4)

for (j in condition_comments){ # create corpus
  print(j)
  assign(paste0('corpus', j, 'train'), corpus(as.character(training_data[, paste0(j)])))
  assign(paste0('corpus', j, 'test'), corpus(as.character(test_data[, paste0(j)])))}

for (j in condition_comments){ # create doc feature matrix
  print(j)
  assign(paste0('dfm', j, 'train'), dfm(get(paste0('corpus', j, 'train')), 
    ngrams = 1, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE, stem = FALSE))
  
  assign(paste0('dfm', j, 'test'), dfm(get(paste0('corpus', j, 'test')), 
    ngrams = 1, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE, stem = FALSE))}

# summary(corpus1)
# kwic(corpus1, 'faulty') # key words in corpus

# do one hot encoding on top 10 features (initial top 10 features concatenated, later try do each 10)
keywords = names(topfeatures(dfmElec_rmktrain, n = 10, decreasing=TRUE))
# library(stringr)

for (j in condition_comments){ # 1 hot encoding for all keywords under major category
  print(j)
  keywords <- names(topfeatures(get(paste0('dfm', j, 'train')), n = 10, decreasing=TRUE))
  keywords <- keywords[nchar(keywords) >= 3] # incl keywords with more than 3 chars
  training_data[paste0(j, 'Bin')] <- lapply(
    training_data[paste0(j)], function (x) (str_detect(x, paste(keywords, collapse = '|')))*1)
  test_data[paste0(j, 'Bin')] <- lapply(
    test_data[paste0(j)], function (x) (str_detect(x, paste(keywords, collapse = '|')))*1)
  }


# Issue: 
# 6 columns of remarks, consider merging some?
# sparse matrices due to one-hot encoding and text categorization
# h2o.ai: skip tree-based algo, i.e. GBM, RF
# sparse multi-layer perceptron (MLP)

# oversampling techniques for classification prob:
# SMOTE (synthetic minorioty over-sampling technique) ??
# ADASYN (adaptive sysnthetic sampling approach)

# undersampling technique: custer, toek links


# Scaling -----------------------------------------------------------------

# scale PG, ERP & Auct.Freq
scaled_col = c('Price.Guide', 'ERP', 'Auct.Freq')

scaled_data = scale(training_data[, scaled_col])

training_data[, scaled_col] = scaled_data
# center_ref = attr(scaled_data, 'scaled:center')
# scaled_ref = attr(scaled_data, 'scaled:scale')
test_data[, scaled_col] = scale(test_data[, scaled_col], 
  center = attr(scaled_data, 'scaled:center'), scale = attr(scaled_data, 'scaled:scale'))

# library("scales"), function rescale()

# Prep files for h2o AutoML ----------------------------------------------------------

drops = c('Res.Price', 'Date', "AuctM", "AuctQ", 'Reg.No',
          "Live Auct/X-Chg", "Mileage", 'Age', "Auct.Stat", "Veh Loc", 'CC',
          'Elect.R', 'Eng.R', 'Ext.R', 'Gearb.R', 'Int.R', 'UC.R', 
          'Elec_rmk', 'Eng_rmk', 'Ext_rmk', 'Gear_rmk', 'Int_rmk', 'UC_rmk',
          "Mileage_yr", "VH_rmk", "Make_Popular") # drop Make_Popular when filtered Jap-Top3 & Natl

df_train = training_data[, !names(training_data) %in% drops]
df_test = test_data[, !names(test_data) %in% drops]

dim(df_train)
dim(df_test)

paste(sum(names(df_train) == names(df_test)), "columns matched out of", dim(df_train)[2], sep = " ")


# h2o can't process ordered factors
ordered_factors = c('AuctY', 'Manf.Yr', 'Mileage_yr_Grp')
for (j in ordered_factors){
  print(j)
  df_train[, paste0(j)] <- factor(df_train[, paste0(j)], ordered = FALSE)
  df_test[, paste0(j)] <- factor(df_test[, paste0(j)], ordered = FALSE, levels = levels(df_train[, paste0(j)]))
}

str(df_train)
str(df_test)

# Split data for train, valid, test ---------------------------------------

# library(caret)
set.seed(123)
trainIndex <- createDataPartition(df_train$Final.Price, p = .7, 
                                  list = FALSE, 
                                  times = 1)

df_split_train = df_train[trainIndex, ]
df_split_nonTrain = df_train[-trainIndex, ]

validIndex <- createDataPartition(df_split_nonTrain$Final.Price, p = .5, 
                                  list = FALSE, 
                                  times = 1)

df_split_valid = df_split_nonTrain[validIndex, ]
df_split_test = df_split_nonTrain[-validIndex, ]

dim(df_split_train)
dim(df_split_valid)
dim(df_split_test)
dim(df_test)

# Init files for h2o ------------------------------------------------------

# library(h2o)
h2o.init(min_mem_size="4g", max_mem_size = "8g")
h2o.shutdown()

df_h2o_train <- as.h2o(df_split_train)
df_h2o_valid <- as.h2o(df_split_valid)
df_h2o_test <- as.h2o(df_split_test)
df_pred <- as.h2o(df_test)

# h2o.describe(df_h2o_train)
# h2o.describe(df_h2o_test)

y <- 'Final.Price'
# y <- 'Res.Price'
x <- setdiff(names(df_h2o_train), y)

# x <- x[!(x %in% c('Price.Guide'))] # remove and observe price guide impact

# splits <- h2o.splitFrame(df_h2o_train, ratios = c(0.7, .15) , seed = 1)
# train <- splits[[1]]
# valid <- splits[[2]]
# test <- splits[[3]]
# h2o AUTOML: training model for final price prediction --------------------------------------------------------------

# aml_selected <- h2o.automl(x = x,
#   y = y,
#   training_frame = df_h2o_train,
#   nfolds = 5,
#   keep_cross_validation_predictions = TRUE,
#   validation_frame = df_h2o_valid,
#   leaderboard_frame = df_h2o_test,
#   # exclude_algos = c("GLM", "DeepLearning", "GBM", DRF", "StackedEnsemble"),
#   max_runtime_secs = 60, # max_models
#   seed = 1,
#   project_name = "selected_final_price")

# h2o.removeAll() # reset settings NOT WORKING

# print(aml_selected@leaderboard)
# h2o.rmse(aml_selected@leader, valid = TRUE)
# aml_selected@leader@parameters
# model_path <- h2o.saveModel(object = h2o.getModel(aml_selected@leader@model_id), path = getwd(), force = TRUE)
h2o_best_model <- h2o.loadModel(model_path)


# all models, without rating except for structural & flood, + remarks of major ctg
# 1 min, gbm, valid error 2.5k
# 10 mins, gbm, valid error 2.3k

# (Jap-Top3 & Natl, filtered model)
# 1 min, gbm model validation error: 2.3k 
# 10 mins, stacked ensemble, validation error 2.26k
# 1 hr, stacked ensemble, validation error 2.22k

# validation error: 2.2k (Jap-Top3 & Natl, filtered model)

# Note all previous metrics are training errors
# ~2k with Make_Popular tag, 1,177 after removed
# training RMSE: 859 (added vehicle history); 859 (with condition remarks & Brand); 965

# aml_all <- h2o.automl(x = x, # note incl all variants
#                       y = y,
#                       training_frame = train,
#                       nfolds = 5,
#                       keep_cross_validation_predictions = TRUE,
#                       validation_frame = valid,
#                       leaderboard_frame = test,
#                       # exclude_algos = c("GLM", "DeepLearning", "GBM", DRF", "StackedEnsemble"),
#                       max_runtime_secs = 600, # max_models
#                       seed = 1,
#                       project_name = "final_price_all"
# )
# 
# h2o.rmse(aml_all@leader, valid=TRUE) # 2,291 (training error), valid error 2.4k


# Make predictions for upcoming listings --------------------------------------------------------

# predict prices of upcoming auction
pred_automl <- h2o.predict(h2o_best_model, df_pred[, -1])
pred_prices = as.vector(pred_automl)

# output in table format
output_tbl = cbind.data.frame(
  'Model' = as.character(df_test$Model_Grp),
  'Variant' = as.character(df_test$Variant),
  'Yr Make' = as.character(df_test$Manf.Yr),
  'Reg. No.' = as.character(df_test$Reg.No.),
  #'Actual Res. Price' = df_test$Res.Price, 
  'Est Final Price' = pred_prices
)

write.csv(output_tbl, file = 'pred_prices.csv')

# i <- sapply(df_test, is.factor) # convert factor cols to characters
# df_output = test_data
# df_output[i] <- lapply(df_output[i], as.character)



# Next Steps: improvements

# more specific indicators for vehicle conditions
# handle sparse data
# convert remaining factors to numericals? i.e. 

# Text Feature Eng Ref ----------------------------------------------------


# clean text remarks, e.g. do for electrical remarks only
#training_data$Elec_len = str_length(training_data$Elec_rmk)
#training_data$Elec_len[training_data$Elec_len == 2] = -1
#training_data['ElecUP'] = sapply(gregexpr("[A-Z]", training_data$Elec_rmk), length)
#training_data$ElecUP[training_data$Elec_rmk == -1] = -1
#training_data['ElecNC'] = sapply(gregexpr("[0-9]", training_data$Elec_rmk), length)
#training_data$ElecNC[training_data$Elec_rmk == -1] = -1
#training_data['ElecUD'] = (training_data$ElecUP/training_data$Elec_len)
#training_data['ElecND'] = (training_data$ElecNC/training_data$Elec_len)

#library(tm)
#corpus = VCorpus(VectorSource(training_data$Elec_rmk)) # corpus = Corpus(VectorSource(training_data$Elec_rmk))
#inspect(corpus)

#corpus = tm_map(corpus, stripWhitespace) # remove extra whitespace
#corpus = tm_map(corpus, content_transformer(tolower))
#corpus = tm_map(corpus, removePunctuation) # remove punctuation
#corpus = tm_map(corpus, removeWords, stopwords('en'))
# corpus = tm_map(corpus, stemDocument) # stemming, i.e. is, are, was, were -> be

#dtm = DocumentTermMatrix(corpus 
#                         # control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)) # default is weightTf
#)
#inspect(dtm)

#findFreqTerms(dtm, 50) # find terms that occur at least n times
#findAssocs(dtm, 'window', 0.5) # find terms that correlate with 0.x correlation

#removeSparseTerms(dtm, sparse = 0.999) # our current sparsity is very high

#library(NLP)
#BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))

#inspect(removeSparseTerms(tdm, 0.9999))
#findFreqTerms(tdm, 25)

#dataframe <- data.frame(text=sapply(corpus, identity),stringsAsFactors=F)
#training_data$Elec_rmk = dataframe$text

#training_data['Elec_rmk'] = ifelse(training_data$Elec_len == 0,-1,training_data$Elec_rmk) # separate those with descriptions and w/o  
#training_data['descWC'] = sapply(gregexpr("\\W+", training_data$Elec_rmk), length) + 1
#training_data$descWC[training_data$Elec_rmk == -1] = -1
#training_data['descWD1'] = (training_data$Elec_len/training_data$descWC)
#training_data$descWD1[training_data$Elec_rmk == -1] = -1
#training_data['descWD2'] = tan(training_data$descWD1)
#training_data$descWD2[training_data$Elec_rmk == -1] = -1

# split data into 2 (below / above median price)
# train1 = subset(train, price <= 2.833)
# train2 = subset(train, price > 2.833)



# Visualize Predictors -----------------------------------------------------

library(GGally)

pred_vis = colnames(training_data)[(colnames(training_data) %in% 
  c('Res.Price', 'Final.Price', 'Brand', 'Model_Grp'))]



ggpairs(training_data[, paste0(pred_vis)], 
  cardinality_threshold = 500) # consider simplifying the categories


str(training_data)

# training_data %>% group_by(Brand) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))




# Manual comparison of valid/test errors (SKIP for Production) ----------------------------------

h2o_best_model <- h2o.loadModel(model_path)

h2o.varimp(h2o_best_model) # price guide dominate the var importance

# analyze results by model group, shud compare valid/test set only

pred_full_h2o = as.vector(h2o.predict(h2o_best_model, df_h2o_train))
df_train_est_h2o = cbind.data.frame('est.Final.Price' = pred_full_h2o, df_split_train)

df_train_est_h2o %>% # compare mae of model groups + any bias towards upper / lower of final price
  dplyr::group_by(Model_Grp) %>%
  dplyr::summarize(
    MAE = mean(abs(est.Final.Price - Final.Price), na.rm=TRUE),
    RMSE = sqrt(mean((est.Final.Price - Final.Price)^2, na.rm=TRUE)),
    est_lesser = length(Model_Grp[est.Final.Price - Final.Price < 0]),
    est_greater = length(Model_Grp[est.Final.Price - Final.Price > 0]),
    no_error = length(Model_Grp[est.Final.Price - Final.Price == 0]),
    totalc = n()
  ) %>%
  dplyr::arrange(desc(MAE)) %>%
  dplyr::mutate(lesser_pct = est_lesser / totalc,
                greater_pct = est_greater / totalc) %>%
  View

# pred_all = as.vector(h2o.predict(aml_all, df_h2o_train))
# sqrt(mean((pred_all-df_train[,1])^2)) # 2,346 (full mode, full dataset)

# filter out Jap-Top 3 & National make to compare with below, re-run data prep
# pred_selected_full_model = as.vector(h2o.predict(aml_all, df_h2o_train))
# 
# sqrt(mean((pred_selected_full_model-df_train[,1])^2)) # 2,291 (full model, Jap-Top3 & National dataset)
# sqrt(mean((pred_full_h2o-df_train[, 1])^2)) # 1,330 (Jap-Top3 & National)


# Japan Top 3 & 2 National Make (SKIP) -------------------------------------------

train_pop = dplyr::filter(training_data, Brand %in% c("PERODUA",'PROTON', 'NISSAN', 'TOYOTA', 'HONDA') & 
                            !is.na(Variant) &
                            !(Elect.R %in% c("", "N")) & !(Eng.R %in% c("", "N")) & !(Ext.R %in% c("","N")) & 
                            !(Gearb.R %in% c("", "N")) & !(Int.R %in% c("", "N")) & !(UC.R %in% c("", "N")) &
                            !(Struct.R %in% c("", "N"))
)

test_pop = dplyr::filter(test_data, Brand %in% c("PERODUA",'PROTON', 'NISSAN', 'TOYOTA', 'HONDA') & 
                           !is.na(Variant) &
                           !(Elect.R %in% c("", "N")) & !(Eng.R %in% c("", "N")) & !(Ext.R %in% c("","N")) & 
                           !(Gearb.R %in% c("", "N")) & !(Int.R %in% c("", "N")) & !(UC.R %in% c("", "N")) &
                           !(Struct.R %in% c("", "N"))
)


# train_pop = train_pop %>% 
#   dplyr::group_by(Variant) %>%
#   dplyr::filter(n()>10) %>%
#   as.data.frame()


drops = c('Date', "AuctM", "AuctQ",
          "Live Auct/X-Chg", "Mileage", 'Age', "Auct.Stat", "Veh Loc",
          'Elec_rmk', 'Eng_rmk', 'Ext_rmk', 'Gear_rmk', 'Int_rmk', 'UC_rmk',
          "Mileage_yr", "VH_rmk", "Make_Popular" # drop Make_Popular when filtered Jap-Top3 & Natl
)

train_pop = train_pop[, !names(train_pop) %in% drops]
test_pop = test_pop[, !names(test_pop) %in% drops]

train_pop = droplevels(train_pop) # any errors??
test_pop = droplevels(test_pop) # any errors??

dim(train_pop)
dim(test_pop)



# h2o Update (Uncomment to update) --------------------------------------------------------------


# Update to latest h2o stable release, weird some packages are deleted
# # The following two commands remove any previously installed H2O packages for R.
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 
# # Next, we download packages that H2O depends on.
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# 
# # # Now we download, install and initialize the H2O package for R.
# install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wright/9/R")


# Wordcloud Plot (Visualization only)----------------------------------------------------------

# quanteda plot
set.seed(100)
textplot_wordcloud(dfm_elec, min_count = 50, random_order = FALSE,
                   rotation = .25, min_size = 0.5, max_size = 5, 
                   color = RColorBrewer::brewer.pal(8,"Dark2")
)

#df_wc = quanteda::convert(dfm1, to = 'matrix')
#df_wc_cs = colSums(df_wc)
#ap.d <- data.frame(word = names(df_wc_cs), freq = df_wc_cs)
#table(ap.d$freq)

#library(wordcloud)
#pal2 <- brewer.pal(8,"Dark2")
# png("test1.png", width=480, height=480, units = 'px')
#wordcloud(words = ap.d$word, freq = ap.d$freq, scale=c(3, .2),min.freq = 50,
#          max.words=200, random.order=FALSE, rot.per=.15, colors=pal2
#)
# dev.off() # to reset graphics device and output graphs


termFrame = data.frame(term = names(tf1), freq = unname(tf1))
#termFrame1 = data.frame(term = names(tf2), freq = unname(tf2))
#termFrame = termFrame %>%
#  left_join(termFrame1, by = 'term')
#termFrame$ratio = termFrame$freq.x/(termFrame$freq.x+termFrame$freq.y)
termFrame$ratio = termFrame$freq/sum(termFrame$freq)
summary(termFrame$ratio)

termFrame1 = subset(termFrame, ratio > 0.03)
options(repr.plot.width=8, repr.plot.height=5)

library(ggplot2)
ggplot(termFrame1, aes(x=reorder(term,ratio), y=ratio))+
  geom_bar(stat='identity')+
  scale_y_continuous(limits=c(0, 0.5))+
  ggtitle('Term Frequency Ratio - Above Median Pricing - item_description')+
  geom_abline(intercept = 0.25, slope = 0,color="red")+
  xlab('Term')

#termFrame2 = subset(termFrame, ratio > 0.53)
#ggplot(termFrame2, aes(x=reorder(term,-ratio), y=ratio))+
#  geom_bar(stat='identity')+
#  scale_y_continuous(limits=c(0,1))+
#  ggtitle('Term Frequency Ratio - Below Median Pricing - item_description')+
#  geom_abline(intercept = 0.5, slope = 0,color="red")+
#  xlab('Term')

# h2o GBM grid search -----------------------------------------------------

hyper_params = list(
  
  max_depth = seq(1,20,2), # usual ~ 10, use c(4,6,8,12,16,20) faster for larger datasets
  #max_depth = seq(minDepth, maxDepth, 1),
  learn_rate = seq(0.05, 0.1, 0.01), # smaller learning rate is better
  learn_rate_annealing = 0.99, # learning_rate shrinks by 1% after every tree, use 1.00 to disable
  
  sample_rate = seq(0.2, 1, 0.01), # sample % of rows per tree
  col_sample_rate = seq(0.2, 1, 0.01), # sample % of columns per split
  col_sample_rate_per_tree = seq(0.2, 1, 0.01),
  col_sample_rate_change_per_level = seq(0.9, 1.1, 0.01), # col sampling / split as a function of split depth
  min_rows = 2^seq(0, log2(nrow(train))-1, 1), # number of min rows in terminal node
  nbins = 2^seq(4, 10, 1), # no. of bins for split-finding for cont/int cols
  nbins_cats = 2^seq(4, 12, 1), # for cat col
  min_split_improvement = c(0, 1e-8, 1e-6, 1e-4), # min relative error improvement thresholds for a split to occur
  histogram_type = c("UniformAddptive", 'QuantilesGlobal', 'RoundRobin') # QG, RR good for num col with outliers
)

search_criteria <- list(strategy = "RandomDiscrete", # 'Cartesian'
                        max_runtime_secs = 600,
                        # max_models = 5,
                        stopping_rounds = 5,
                        stopping_metric = 'AUTO',
                        stopping_tolerance = 1e-3
)

system.time(grid <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  algorithm="gbm",
  grid_id = 'multi_params',
  # grid_id="depth_grid", # search for depth only
  
  x = x, 
  y = y, 
  training_frame = train, 
  validation_frame = valid,
  
  ntrees = 10000, ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  
  seed = 1, 
  # learn_rate = 0.01, disabled if set in hyper_params
  score_tree_interval = 10 # score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  )
) # 450.89s, 8 min (depth search only)

grid@summary_table

## sort the grid models by preferred metric
sortedGrid <- h2o.getGrid(grid@grid_id, sort_by="rmse")
sortedGrid
h2o.rmse(h2o.performance(h2o.getModel(sortedGrid@model_ids[[1]]))) # rmse = 804 (depth search), 680 (after tuning)

## find the range of max_depth for the top 5 models - can be used to set for further tuning
topDepths = sortedGrid@summary_table$max_depth[1:5]
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
minDepth
maxDepth
# redo grid search with specified depth range

pred_gbm <- h2o.predict(h2o.getModel(sortedGrid@model_ids[[1]]), df_h2o_test)

pred_gbm = as.data.frame(pred_gbm)

str(test_pop)
# output in table format
pop_tbl_gbm = cbind.data.frame(
  'Model' = as.character(test_pop$Model_Grp),
  'Variant' = as.character(test_pop$Variant),
  'Reg. No.' = as.character(test_pop$Reg.No.),
  'Actual Res. Price' = test_pop$Res.Price, 
  'Est Final Price' = pred_gbm
)

write.csv(pop_tbl_gbm, file = 'pop_gbm.csv')





# XGBoost -----------------------------------------------------------------

# # # # # # # # # # # # # # # # # 
# Matrix format can preserve ordered factors!!
# Consider use original dataset, dunno how it affects the test set
#
#
# # # # # # # # # # # # # # # # # 

str(df_train)
str(df_pred)

library(xgboost)

library(caret)
set.seed(1234)
trainIndex <- createDataPartition(
  df_train$Final.Price, p = 0.7,
  list = FALSE, 
  times = 1
)
validtest_set = df_train[-trainIndex, ]
validtestIndex <- createDataPartition(
  validtest_set$Final.Price, p = 0.5,
  list = FALSE, 
  times = 1
)

train_data_xgb = data.matrix(df_train[trainIndex, ])
valid_data_xgb= data.matrix(validtest_set[validtestIndex, ])
test_data_xgb= data.matrix(validtest_set[-validtestIndex, ])

dim(train_data_xgb)
dim(valid_data_xgb)
dim(test_data_xgb)

dtrain <- xgb.DMatrix(data = train_data_xgb[, -1], label = train_data_xgb[, 1])
dvalid <- xgb.DMatrix(data = valid_data_xgb[, -1], label = valid_data_xgb[, 1])
dtest <- xgb.DMatrix(data = test_data_xgb[, -1], label = test_data_xgb[, 1])

#bstSparse <- xgboost( # simple implementation
#  data = dtrain, max.depth = 2, eta = 1, nthread = 3,
#  nrounds = 2, objective = 'reg:linear',
#  verbose = 2
#)

xgPrm = list(
  boost='gbtree',objective='reg:linear',colsample_bytree=1,
  eta=0.11,max_depth=9,min_child_weight=1,alpha=0.3,
  lambda=0.4,gamma=0.2,subsample=0.8,seed=5,silent=TRUE
)

xgbModel = xgb.train(
  xgPrm, dtrain, nrounds=300, 
  watchlist=list(train=dtrain, test=dvalid)
)

pred <- predict(xgbModel, dtest)

err <- sqrt(mean((pred-test_data_xgb[, 1])^2))
err # 2233 (with train, valid, test)

# analyze results by model group
full_data = data.matrix(df_train)
dfull <- xgb.DMatrix(data = full_data[, -1], label = full_data[, 1])
pred_full_xgb <- predict(xgbModel, dfull)
df_train_est_xgb = cbind.data.frame('est.Final.Price' = pred_full_xgb, df_train)

# use this to benchmark with h2o predictions
sqrt(mean((pred_full_xgb-df_train[, 1])^2)) # 1239


df_train_est_xgb %>%
  dplyr::group_by(Model_Grp) %>%
  dplyr::summarize(
    MAE = mean(abs(est.Final.Price - Final.Price), na.rm=TRUE),
    RMSE = sqrt(mean((est.Final.Price - Final.Price)^2, na.rm=TRUE)),
    est_lesser = length(Model_Grp[est.Final.Price - Final.Price < 0]),
    est_greater = length(Model_Grp[est.Final.Price - Final.Price > 0]),
    totalc = n()
  ) %>%
  dplyr::arrange(desc(MAE)) %>%
  View



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

