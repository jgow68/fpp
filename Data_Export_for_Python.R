
# Output cleaned file for Google Colab ------------------------------------

drops = c('Res.Price', 'Date', "AuctM", "AuctQ", # 'Reg.No',
          "Live Auct/X-Chg", "Mileage", 'Age', "Auct.Stat", "Veh Loc", 'CC',
          'Elect.R', 'Eng.R', 'Ext.R', 'Gearb.R', 'Int.R', 'UC.R', 
          'Elec_rmk', 'Eng_rmk', 'Ext_rmk', 'Gear_rmk', 'Int_rmk', 'UC_rmk',
          "Mileage_yr", "VH_rmk", "Make_Popular") # drop Make_Popular when filtered Jap-Top3 & Natl

GG_train = training_data[, !names(training_data) %in% drops]
GG_test = test_data[, !names(test_data) %in% drops]
GG_test$Reg.No. = as.character(GG_test$Reg.No.)

dim(GG_train)
dim(GG_test)

str(GG_train)
str(GG_test)

paste(sum(names(GG_train) == names(GG_test)), "columns matched out of", dim(GG_train)[2], sep = " ")

extract_names <- function(x){
  # df <- get(paste0(x), envir = .GlobalEnv)
  Factor_list <- sapply(x, is.factor)
  Factor_names <- names(Factor_list[Factor_list == TRUE])
  NonFactor_names <- names(Factor_list[Factor_list == FALSE])
  Output_list <- list('Factors' = Factor_names, 'NonFactors' = NonFactor_names)
  return(Output_list)}

GG_train_names = extract_names(GG_train)
GG_test_names = extract_names(GG_test)

ordered_list = sapply(GG_train, is.ordered)
ordered_names = names(ordered_list[ordered_list == TRUE])

for (j in ordered_names){
  print(j)
  GG_train[, paste0(j)] <- factor(GG_train[, paste0(j)], ordered = FALSE)
  GG_test[, paste0(j)] <- factor(GG_test[, paste0(j)], ordered = FALSE, 
    levels = levels(GG_train[, paste0(j)]))}

extract_ctg_col <- function(x){
  category_formula = as.character()
  for (i in x){
    category_formula = trimws(paste(i, category_formula))}
  return(gsub(" ", "+", category_formula))}

ctg_formula = extract_ctg_col(GG_train_names$Factors)
ctg_formula_pred = extract_ctg_col(GG_test_names$Factors)

ctg_formula_pred

Category_col = model.matrix(as.formula(paste("~", ctg_formula, "-1")),
  data=GG_train)[, -1]
Category_col_pred = model.matrix(as.formula(paste("~", ctg_formula_pred, "-1")),
  data=GG_test)[, -1]

NonCategory_col = GG_train[GG_train_names$NonFactors]
NonCategory_col_pred = GG_test[GG_test_names$NonFactors]

str(GG_test)
GG_test_names$NonFactors
output_data = cbind(NonCategory_col, Category_col)
output_data_pred = cbind(NonCategory_col_pred, Category_col_pred)

str(output_data)
str(output_data_pred)

write.csv(output_data, "data_Google_Colab.csv", row.names = FALSE)
write.csv(output_data_pred, "pred_data_Google_Colab.csv", row.names = FALSE)

