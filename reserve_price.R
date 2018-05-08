data = read.csv("ConsolAD_Apr18.csv")


# Extract and form df -----------------------------------------------------

str(data)

training_data = cbind.data.frame(data$Auction.Date, data$Brand, data$Model.Group,
                                 data$Adj..Var.,
                                 data$Reserve.Price, data$Final.bid.price,
                                 data$Adj..PG, data$Adj..ERP,
                                 data$Manufacturing.Year, data$Mileage,data$Vehicle.Age,
                                 data$Auction.Status,
                                 data$MUV.Grade, # first grading scheme
                                 data$VAS.Exterior.Grade, data$VAS.Interior.Grade, # second grading scheme
                                 # third grading scheme
                                 data$MOS.Electrical.Rating, data$MOS.Engine.Rating, data$MOS.Exterior.Rating,
                                 data$MOS.Gearbox.Rating, data$MOS.Interior.Rating, data$MOS.Undercarriage.Rating,
                                 data$MOS.Structure.Rating, data$MOS.Flood.Rating
                                  ) 
names(training_data) = c("Date", "Brand", "Model_Grp", 
                         "Variant",
                         "Res.Price", "Final.Price",
                         "Price.Guide", "ERP",
                         "Manf.Yr", "Mileage", "Age",
                         "Auct.Stat",
                         "Grade",
                         "Ext", "Int",
                         "Elect.R", "Eng.R", "Ext.R",
                         "Gearb.R", "Int.R", "UC.R",
                         "Struct.R", "Flood.R"
)

str(training_data)

training_data = dplyr::filter(training_data, Auct.Stat!="Withdrawn") # filtered out fields without reserve price
sum(is.na(training_data$Res.Price)) # check N/As for reserve price

training_data$Res.Price = as.numeric(levels(training_data$Res.Price))[training_data$Res.Price]
training_data$Mileage = as.numeric(levels(training_data$Mileage))[training_data$Mileage]
training_data$Date = as.Date(training_data$Date)

sapply(training_data, function(x) sum(is.na(x))) # check no. of NAs in each predictor
# Note: final price, price guide, ERP, mileage, flood rating has NAs

str(training_data)
library("scales")

# scale final price, manf yr, mileage, age
training_data$Final.Price = rescale(training_data$Final.Price, to=c(0,1))
training_data$Price.Guide = rescale(training_data$Price.Guide, to=c(0,1))
training_data$ERP = rescale(training_data$ERP, to=c(0,1))
training_data$Manf.Yr = rescale(training_data$Manf.Yr, to=c(0,1))
training_data$Mileage = rescale(training_data$Mileage, to=c(0,1))
training_data$Age = rescale(training_data$Age, to=c(0,1))




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

