# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                       NOTE:                         #
# Create file by copying table from Power BI          #
# Append PG & ERP to training data set before running #
#                                                     #
# training data & prediction data have different      #
# layout & format                                     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Read data from databases ------------------------------------------------


# data = read.csv("ConsolAD_Apr18.csv")
# data = read.csv("ConsolAD_Jun18.csv")
# data = read.csv('ConsolAD_Jul18.csv', encoding = 'UTF-8')
data = read.csv('ConsolAD_17Oct_2018.csv', encoding = 'UTF-8')
verify_data = read.csv("Disposal Listing Template.csv")

# Load libraries ----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, stringr, ggplot2, h2o, quanteda, caret)

# Extract and form df (Note: testing rmks) -----------------------------------------------------

training_data = cbind.data.frame(
  "Res.Price" = data$Reserve.Price, "Final.Price" = data$Final.bid.price,
  "Date" = data$Auction.Date, "AuctY" = data$Auction.Year, # note utf-8 file start format changed for Auction Date
  "AuctM" = data$Auction.Month, "AuctQ" = data$Auction.Quarter,
  "Brand" = data$Brand, "Model_Grp" = data$Model.Group,
  "AuctPf" = data$Auction.Platform, 
  "Variant" = data$Variants.Standardization, 
  "Price.Guide" = data$PG, "ERP" = data$ERP,
  "Auct.Freq" = data$Auction.Freq..by.Auction.Session.,
  "Manf.Yr" = data$Manufacturing.Year, "Mileage" = data$Mileage,
  "Auct.Stat" = data$Auction.Status, "Veh.Loc" = data$Vehicle.Location,
  "CC" = data$Capacity,
  # data$MUV.Grade, # first grading scheme
  # data$VAS.Exterior.Grade, data$VAS.Interior.Grade, # second grading scheme
  # third grading scheme
  "Elect.R" = data$MOS.Electrical.Rating, "Eng.R" = data$MOS.Engine.Rating, "Ext.R" = data$MOS.Exterior.Rating,
  "Gearb.R" = data$MOS.Gearbox.Rating, "Int.R" = data$MOS.Interior.Rating, "UC.R" = data$MOS.Undercarriage.Rating,
  "Struct.R" = data$MOS.Structure.Rating, "Flood.R" = data$MOS.Flood.Rating,
  "Elec_rmk" = data$CommentsElec, "Eng_rmk" = data$CommentsEng, "Ext_rmk" = data$CommentsExt,
  "Gear_rmk" = data$CommentsGear, "Int_rmk" = data$CommentsInt, 
  "UC_rmk" = data$CommentsUnderCarr, "VH_rmk" = data$CommentsVehHistory)
  
  # data$Electrical_Rmks, data$Engine_Rmks, data$Exterior_Rmks,
  # data$Gearbox_Rmks, data$Interior_Rmks, data$UC_Rmks, data$VehHist_Rmks)

test_data = cbind.data.frame(
  "Res.Price" = verify_data$Proposed.Reserve.Price..RM., "Reg.No." = verify_data$Reg..No., 
  "AuctY" = as.factor(format(Sys.Date(), "%Y")),
  "Brand" = verify_data$Make, "Model_Grp" = verify_data$Model,
  "Variant" = verify_data$Variants.Standardization,
  "Price.Guide" = verify_data$Price.Guide..RM., "ERP" = verify_data$Approved.Final.ERP,
  "Auct.Freq" = verify_data$No..of.times.in.auction,
  "Manf.Yr" = verify_data$Year.Make, "Mileage" = verify_data$Mileage,
  "Elect.R" = verify_data$Electrical, "Eng.R" = verify_data$Engine, "Ext.R" = verify_data$Ext.Grade,
  "Gearb.R" = verify_data$Gearbox, "Int.R" = verify_data$Interior, "UC.R" = verify_data$Undercarriage,
  "Struct.R" = verify_data$Structure, "Flood.R" = verify_data$Flooded.Rating,
  "Elec_rmk" = verify_data$Electrical.comments, "Eng_rmk" = verify_data$Engine.comments, "Ext_rmk" = verify_data$Exterior.comments,
  "Gear_rmk" = verify_data$Gearbox.comments, "Int_rmk" = verify_data$Interior.comments, 
  "UC_rmk" = verify_data$Undercarriage.comments, "VH_rmk" = verify_data$vehicle.history)


# Clean data & apply filters -----------------------------------------------------------

dim(training_data)

# Clean character based predictors excl vehicle remarks -------


name_columns = c('Brand', 'Model_Grp', 'Variant', 'Veh.Loc', 'Auct.Stat')
training_data = training_data[complete.cases(training_data[name_columns]), ]

training_data[name_columns] = sapply(training_data[name_columns], function(x) toupper(trimws(x)))
training_data[name_columns] = lapply(training_data[name_columns], factor)

training_data = dplyr::filter(training_data, Auct.Stat == "SOLD")

training_data = droplevels(training_data)

sapply(test_data, function(x) sum(is.na(x))) # check no. of NAs in each predictor
# test_data = test_data[complete.cases(test_data[, -1]), ] # filter for NAs except for reserve price col.

test_data[colnames(test_data) %in% name_columns] = sapply(
  test_data[colnames(test_data) %in% name_columns], function(x) toupper(trimws(x)))

test_data$Brand = factor(test_data$Brand, levels =  levels(training_data$Brand))
test_data$Model_Grp = factor(test_data$Model_Grp, levels =  levels(training_data$Model_Grp))
test_data$Variant = factor(test_data$Variant, levels =  levels(training_data$Variant))

# Categorize Make_Popular into: "HOT", "COLD_PREMIUM", "COLD_NONPREMIUM"
training_data$Make_Popular = as.factor(
  dplyr::case_when(
    training_data$Brand %in% c("HONDA", "NISSAN", "TOYOTA", "PROTON", "PERODUA") ~ "HOT",
    training_data$Brand %in% c("ALFA ROMEO", "BMW", "MERCEDES BENZ", "VOLVO", "PORSCHE", "ROVER", 
                               "LAND ROVER", "MG ROVER", "MINI. UNITED KINGDOM", "AUDI", "JAGUAR", 
                               "INFINITI","LEXUS") ~ "COLD_PREMIUM",
    TRUE ~ "COLD_NONPREMIUM"))

test_data$Make_Popular = dplyr::case_when(
  test_data$Brand %in% c("HONDA", "NISSAN", "TOYOTA", "PROTON", "PERODUA") ~ "HOT",
  test_data$Brand %in% c("ALFA ROMEO", "BMW", "MERCEDES BENZ", "VOLVO", "PORSCHE", "ROVER", 
                         "LAND ROVER", "MG ROVER", "MINI. UNITED KINGDOM", "AUDI", "JAGUAR", 
                         "INFINITI","LEXUS") ~ "COLD_PREMIUM",
  TRUE ~ "COLD_NONPREMIUM")
test_data$Make_Popular = factor(test_data$Make_Popular, levels = levels(training_data$Make_Popular))


# Clean numerical based predictors----------------------------

num_columns = c("Res.Price", "Final.Price", 'AuctY', 'Manf.Yr', 'Price.Guide', 'ERP', 'Mileage', 'CC', 'Auct.Freq')

toNumFunction <- function (x) {
 tryCatch({
    if(class(x) == 'factor'){x = as.numeric(gsub(",", "", levels(x)))[x]} 
    else {x}},
  error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

training_data[colnames(training_data) %in% num_columns] <- sapply(
  training_data[colnames(training_data) %in% num_columns], toNumFunction)

test_data[colnames(test_data) %in% num_columns] <- sapply(
  test_data[colnames(test_data) %in% num_columns], toNumFunction)


# Clean time related predictors -------------------------------------------

training_data = dplyr::filter(training_data, (Manf.Yr >= 2000) & !is.na(Manf.Yr))

training_data$Date = as.Date(training_data$Date, format='%m/%d/%Y')

training_data$Age = as.integer(format(Sys.Date(), "%Y")) - training_data$Manf.Yr
training_data$Mileage_yr = training_data$Mileage / training_data$Age

test_data$Age = as.integer(format(Sys.Date(), "%Y")) - test_data$Manf.Yr
test_data$Auct.Freq = test_data$Auct.Freq + 1 # note template auction freq is before listing
test_data$Mileage_yr = test_data$Mileage / test_data$Age

ctg_mileage_yr <- function (x){
  x  %>% 
  dplyr::mutate(Mileage_yr_Grp = factor(
    dplyr::case_when(
      (Mileage_yr <= 30000) ~ "<= 30k/yr",
      (Mileage_yr > 30001 & Mileage_yr < 40000) ~ "30-40k/yr",
      (Mileage_yr > 40000) ~ ">40k/yr", 
      TRUE ~ "N/A"), 
    ordered = TRUE, 
    levels = c("<= 30k/yr", "30-40k/yr",">40k/yr")))}

training_data = ctg_mileage_yr(training_data)
test_data = ctg_mileage_yr(test_data)

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

test_data$Manf.Yr = factor(test_data$Manf.Yr, 
  levels = levels(training_data$Manf.Yr), ordered = TRUE)

dim(training_data)
dim(test_data)


# For Ref:
# for (j in name_toupper){
#   print(j)
#   tryCatch({
#     training_data[, paste0(j)] = as.factor(toupper(trimws(training_data[, paste0(j)])))
#     test_data[, paste0(j)] = as.factor(toupper(trimws(test_data[, paste0(j)])))},
#   error = function(e){cat("ERROR :",conditionMessage(e), "\n")})}
# check if can work for unavail columns for test data

## Alternate method using for loop
# for (j in numerical_columns){
#   print(j)
#   tryCatch({
#     if(class(test_data[, paste0(j)]) == 'factor'){
#       test_data[, paste0(j)] = as.numeric(
#         gsub(",", "", levels(test_data[, paste0(j)])))[test_data[, paste0(j)]]} 
#     else{NULL}}, 
#   error = function(e){cat("ERROR :",conditionMessage(e), "\n")})}


# Clean & Filter vehicle condition rating (potentially removed in future)  --------

training_data[condition_rating] = sapply(training_data[condition_rating], 
  function(x) as.character(x))
test_data[condition_rating] = sapply(test_data[condition_rating], 
  function(x) as.character(x))

condition_rating = colnames(training_data)[grep('\\.[R]', 
  colnames(training_data))]

training_data = dplyr::filter(training_data,
  !is.na(Variant) &
  !(Elect.R %in% c(0, "", "N")) & !(Eng.R %in% c(0, "", "N")) & !(Ext.R %in% c(0, "","N")) & 
  !(Gearb.R %in% c(0, "", "N")) & !(Int.R %in% c(0, "", "N")) & !(UC.R %in% c(0, "", "N")) &
  !(Struct.R %in% c(0, "", "N")))


cat_ABFun <- function (x) {
  if_else(condition = x %in% c("A", "B"), true = "A/B", false = x)}
      
# excl flood rating (different rating parameters than other ratings)
cat_columns = condition_rating[!(condition_rating %in% c('Flood.R'))]

training_data[cat_columns] <- sapply(training_data[cat_columns], cat_ABFun)
training_data[cat_columns] <- lapply(
  training_data[cat_columns], function (x) {factor(x, levels = c("A/B", 'C','D','E'))})

test_data[cat_columns] <- sapply(test_data[cat_columns], cat_ABFun)
test_data[cat_columns] <- lapply(
  test_data[cat_columns], function (x) {factor(x, levels = c("A/B", 'C','D','E'))})

# Clean Flood Rating: alot of NAs and 0
training_data$Flood.R = ifelse(
  is.na(training_data$Flood.R), 0,
    ifelse(training_data$Flood.R == "", 0,
      training_data$Flood.R))
training_data$Flood.R = as.factor(training_data$Flood.R)

test_data$Flood.R = ifelse(
  is.na(test_data$Flood.R), 0,
    ifelse(test_data$Flood.R == "", 0,
      test_data$Flood.R))
test_data$Flood.R = factor(test_data$Flood.R, levels = levels(training_data$Flood.R))

# Note: 
# consider ordered vehicle conditions



# Clean vehicle condition remarks ----------------------------------------

condition_comments = colnames(training_data)[grep('\\_[rmk]', 
  colnames(training_data))]

for (j in condition_comments){ # convert veh comments to character
  print(j)
  training_data[, paste0(j)] = as.character(training_data[, paste0(j)])
  test_data[, paste0(j)] = as.character(test_data[, paste0(j)])}

dim(training_data)
dim(test_data)

str(training_data)
str(test_data)


# Text: Features Selection for vehicle condition remarks ------------------------------------------------

quanteda_options("threads" = 4)

for (j in condition_comments){ # create corpus from vehicle condition remarks
  print(j)
  assign(paste0('corpus', j, 'train'), corpus(as.character(training_data[, paste0(j)])))
  assign(paste0('corpus', j, 'test'), corpus(as.character(test_data[, paste0(j)])))}

for (j in condition_comments){ # create doc feature matrix from corpus
  print(j)
  assign(paste0('dfm', j, 'train'), dfm(get(paste0('corpus', j, 'train')), 
    ngrams = 1, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE, stem = FALSE))
  
  assign(paste0('dfm', j, 'test'), dfm(get(paste0('corpus', j, 'test')), 
    ngrams = 1, remove = stopwords("english"), remove_punct = TRUE, remove_numbers = TRUE, stem = FALSE))}

# summary(corpus1)
# kwic(corpus1, 'faulty') # key words in corpus

# do one hot encoding on top 10 features (initial top 10 features concatenated, later try do each 10)

for (j in condition_comments){ # 1 hot encoding for all keywords under major category
  print(j)
  keywords <- names(topfeatures(get(paste0('dfm', j, 'train')), 
    n = 10, decreasing=TRUE))
  keywords <- keywords[nchar(keywords) >= 3] # incl keywords with more than 3 chars
  
  training_data[paste0(j, 'Bin')] <- lapply(
    training_data[paste0(j)], 
    function (x) (str_detect(x, paste(keywords, collapse = '|')))*1)
  
  test_data[paste0(j, 'Bin')] <- lapply(
    test_data[paste0(j)], 
    function (x) (str_detect(x, paste(keywords, collapse = '|')))*1)}


# Issue: 
# 6 columns of remarks, consider merging some?
# sparse matrices due to one-hot encoding and text categorization
# h2o.ai: skip tree-based algo, i.e. GBM, RF
# sparse multi-layer perceptron (MLP)

# oversampling techniques for classification prob:
# SMOTE (synthetic minorioty over-sampling technique) ??
# ADASYN (adaptive sysnthetic sampling approach)

# undersampling technique: custer, toek links


# Scaling of selected numerical predictors -----------------------------------------------------------------

# Scale PG, ERP & Auct.Freq
scaled_col = c('Price.Guide', 'ERP', 'Auct.Freq')

# saved scaled data for use in test_data
scaled_data = scale(training_data[, scaled_col])

training_data[, scaled_col] = scaled_data

test_data[, scaled_col] = scale(test_data[, scaled_col], 
  center = attr(scaled_data, 'scaled:center'), scale = attr(scaled_data, 'scaled:scale'))

# library("scales"); function rescale()

# Prep files for h2o AutoML ----------------------------------------------------------

# select predictors to keep

cols_to_keep = c('Final.Price', 'Reg.No.', 'AuctY', 'Brand', "Model_Grp", "Variant", "Price.Guide", "ERP", "Auct.Freq", "Manf.Yr",
                 "Struct.R", "Flood.R", "Mileage_yr_Grp", "Elec_rmkBin", "Eng_rmkBin", "Ext_rmkBin", "Gear_rmkBin", "Int_rmkBin", 
                 "UC_rmkBin", "VH_rmkBin")

df_train = training_data[, names(training_data) %in% cols_to_keep]
df_pred = test_data[, names(test_data) %in% cols_to_keep]

paste(sum(names(df_train) == names(df_pred)), "columns matched out of", dim(df_train)[2], sep = " ")

# h2o can't process ordered factors, so revert ordered factors to factors
ordered_factors = c('AuctY', 'Manf.Yr', 'Mileage_yr_Grp')
for (j in ordered_factors){
  print(j)
  df_train[, paste0(j)] <- factor(df_train[, paste0(j)], ordered = FALSE)
  df_pred[, paste0(j)] <- factor(df_pred[, paste0(j)], ordered = FALSE, levels = levels(df_train[, paste0(j)]))}

str(df_train)
str(df_pred)

dim(df_train)
dim(df_pred)

# Split training data into 3 sets: train, valid, test ---------------------------------------

set.seed(123) # set seed to maintain changes
trainIndex <- createDataPartition(df_train$Final.Price, p = .7, list = FALSE, times = 1)

df_split_train = df_train[trainIndex, ]
df_split_nonTrain = df_train[-trainIndex, ]

validIndex <- createDataPartition(df_split_nonTrain$Final.Price, p = .5, list = FALSE, times = 1)

df_split_valid = df_split_nonTrain[validIndex, ]
df_split_test = df_split_nonTrain[-validIndex, ]

dim(df_split_train)
dim(df_split_valid)
dim(df_split_test)
dim(df_pred)

# Init files for h2o ------------------------------------------------------

h2o.init() # options if installed jdk 64 bit: min_mem_size="4g", max_mem_size = "8g"
h2o.shutdown()

df_h2o_train <- as.h2o(df_split_train)
df_h2o_valid <- as.h2o(df_split_valid)
df_h2o_test <- as.h2o(df_split_test)
df_h2o_pred <- as.h2o(df_pred)

# h2o.describe(df_h2o_train)
# h2o.describe(df_h2o_test)

y <- 'Final.Price'
x <- setdiff(names(df_h2o_train), y)

# h2o AUTOML: training model for final price prediction --------------------------------------------------------------

AutoML_model <- h2o.automl(
  x = x,
  y = y,
  training_frame = df_h2o_train,
  nfolds = 5,
  keep_cross_validation_predictions = TRUE,
  validation_frame = df_h2o_valid,
  leaderboard_frame = df_h2o_test,
  max_runtime_secs = 60, # max_models
  seed = 1,
  project_name = "est_final_price")

# print(aml_selected@leaderboard)
# h2o.rmse(aml_selected@leader, valid = TRUE)
# aml_selected@leader@parameters
model_path <- h2o.saveModel(object = h2o.getModel(AutoML_model@leader@model_id), path = getwd(), force = TRUE)
h2o_best_model <- h2o.loadModel(model_path)

# Make predictions for disposal listing  --------------------------------------------------------

# predict prices of upcoming auction
pred_automl <- h2o.predict(h2o_best_model, df_h2o_pred[, -1])
pred_prices = as.vector(pred_automl)

# output in table format
output_tbl = cbind.data.frame(
  'Model' = as.character(df_pred$Model_Grp),
  'Variant' = as.character(df_pred$Variant),
  'Yr Make' = as.character(df_pred$Manf.Yr),
  'Reg. No.' = as.character(df_pred$Reg.No.),
  'Est Final Price' = pred_prices)

write.csv(output_tbl, file = 'pred_prices.csv')

# i <- sapply(df_pred, is.factor) # convert factor cols to characters
# df_output = test_data
# df_output[i] <- lapply(df_output[i], as.character)



# Next Steps / Improvements:

# more binary indicators for each keyword in vehicle condition remarks
# sparse data handling?


# Ref: Text Feature Engineering ----------------------------------------------------


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



# Ref: Visualize Predictors -----------------------------------------------------

library(GGally)

pred_vis = colnames(training_data)[(colnames(training_data) %in% 
  c('Res.Price', 'Final.Price', 'Brand', 'Model_Grp'))]



ggpairs(training_data[, paste0(pred_vis)], 
  cardinality_threshold = 500) # consider simplifying the categories


str(training_data)

# training_data %>% group_by(Brand) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))




# Ref: Manual comparison of valid/test errors (SKIP for Production) ----------------------------------

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

# Ref: to update h2o (Uncomment to update) --------------------------------------------------------------


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


# Ref: Wordcloud Plot (Visualization only)----------------------------------------------------------

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

# Ref: h2o GBM grid search -----------------------------------------------------

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





# Ref: XGBoost -----------------------------------------------------------------

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


# Ref: h2o stacked ensemble - p2 -----------------------------------------------

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


