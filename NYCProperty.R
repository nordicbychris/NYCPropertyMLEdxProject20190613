library(tidyverse)
library(caret)
library(e1071) # for skewness
library(zoo) # for na.aggregate which replaces N/A with the mean for the respective group
library(randomForest)
library(rpart)
library(elasticnet) # for lasso and ridge and elasticnet regression
library(pls) # for principle component analysis
library(fastICA) # for Independent Component regression
library(monomvn) # for Bayesian ridge regression 

#Read in CSV file from my hard drive
#Data available in my GitHub at https://github.com/nordicbychris/NYCPropertyMLEdxProject20190613.git
nycproperties <- read_csv("C:/RCoding/nyc-property-sales/nyc-rolling-sales.csv")
colnames(nycproperties)
dim(nycproperties)

# Pre-processing and wrangling
# ----------------------------
#
# Convert to numeric or date for graphing and analysis
nycproperties$`SALE PRICE` <- as.numeric(as.character((nycproperties$`SALE PRICE`)))
class(nycproperties$`SALE PRICE`)
nycproperties$`LAND SQUARE FEET` <- as.numeric(as.character((nycproperties$`LAND SQUARE FEET`)))
class(nycproperties$`LAND SQUARE FEET`)
nycproperties$`GROSS SQUARE FEET` <- as.numeric(as.character((nycproperties$`GROSS SQUARE FEET`)))
class(nycproperties$`GROSS SQUARE FEET`)
nycproperties$`SALE DATE` <- as.Date(as.character((nycproperties$`SALE DATE`)))
class(nycproperties$`SALE DATE`)

# Create new column for BuildingAge and fill with data
nycproperties[c("BuildingAge")] <- 2019 - nycproperties$`YEAR BUILT`

# Replace NA values with the mean for the column/group for the numeric columns
checknumeric <- sapply(nycproperties, is.numeric)
nycproperties[checknumeric] <- lapply(nycproperties[checknumeric], na.aggregate)

# Make a new data frame of all the living spaces (not condos) since these have complete data for multiple variables
livingspaces <- nycproperties %>%
  filter(str_detect(`BUILDING CLASS AT TIME OF SALE` ,"A") | 
         str_detect(`BUILDING CLASS AT TIME OF SALE` ,"B") |
         str_detect(`BUILDING CLASS AT TIME OF SALE` ,"C") |
         str_detect(`BUILDING CLASS AT TIME OF SALE` ,"D") |
         str_detect(`BUILDING CLASS AT TIME OF SALE` ,"L"))

# Remove zero values and/or extreme outliers         
livingspaces <- livingspaces %>%
  filter((`SALE PRICE` > 20 & `SALE PRICE` < 100000000) &
           BuildingAge > 0 & BuildingAge < 500 & 
           `TOTAL UNITS` > 0 & `TOTAL UNITS`< 500 &
           `GROSS SQUARE FEET`> 0 & `LAND SQUARE FEET` > 0 )

# Data exploration and visualization
# ----------------------------------

# Histogram to show price distribution
hist(livingspaces$`SALE PRICE`)
# Repeat histogram with a smaller part of the data set
livingspaces %>%
  filter(`SALE PRICE` > 100000 & `SALE PRICE` < 5000000) %>%
  ggplot(aes(`SALE PRICE`)) +
  geom_histogram(binwidth = 100000, fill = "lightsteelblue")
# Note the peak at the mean value due to replacement of NA with the mean

# Histograms of the predictors
livingspaces %>%
  ggplot(aes(BuildingAge)) +
  geom_histogram(fill = "lightsteelblue")
livingspaces %>%
  ggplot(aes(`GROSS SQUARE FEET`)) +
  geom_histogram(fill = "lightsteelblue")
livingspaces %>%
  ggplot(aes(`LAND SQUARE FEET`)) +
  geom_histogram(fill = "lightsteelblue")
livingspaces %>%
  ggplot(aes(`TOTAL UNITS`)) +
  geom_histogram(fill = "lightsteelblue")

# Measure how skewed the sale price data is
skewness(livingspaces$`SALE PRICE`, type = 1)
# Skewnewss of predictors
skewness(livingspaces$BuildingAge, type = 1)
skewness(livingspaces$`GROSS SQUARE FEET`, type = 1)
skewness(livingspaces$`LAND SQUARE FEET`, type = 1)
skewness(livingspaces$`TOTAL UNITS`, type = 1)
# All skewed except for BuildingAge. Something to be considered in the analysis later.

# Dot plot of all sale prices divided into the category borough
livingspaces %>%
  ggplot(aes(BOROUGH, `SALE PRICE`)) +
  geom_point(color = "lightblue") +
  ggtitle("Sale prices by NYC borough") +
  coord_flip()

# Plot of the frequency in each borough  
livingspaces %>%
  ggplot(aes(BOROUGH)) +
  geom_bar(fill = "lightsteelblue") +
  ylab("Property count") +
  ggtitle("Frequency by NYC borough")
# Note that the number of points for Manhattan has dropped

# The mean sale price sorted by Borough
livingspaces %>%
  group_by(BOROUGH) %>%
  summarise(n = n(), avg = mean(`SALE PRICE`), se = sd(`SALE PRICE`)/sqrt(n())) %>%
  ggplot(aes(x = BOROUGH, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  xlab("Borough") +
  ylab("Mean sale price") +
  ggtitle("Sale prices by NYC borough")
# Note that Manhattan prices are well above the average for the remaining boroughs

# Does the sale price vary according to sale date? i.e. are prices rising?
livingspaces %>%
  ggplot(aes(`SALE DATE`,`SALE PRICE`)) +
  geom_point() +
  geom_smooth(na.rm = TRUE, color = "red", size = 0.1, method = lm) +
  ggtitle("Sale prices by date of sale")
# No major change in price over time, so can be excluded from model

# Does the sale price vary according to building age?
livingspaces %>%
  ggplot(aes(BuildingAge,`SALE PRICE`)) +
  geom_point() +
  geom_smooth(color = "red", size = 0.1, method = lm) +
  xlab("Building Age (years)") +
  ylab("Sale Price") +
  ggtitle("Sale prices by age of building")
# Buildings around 100 years old tend to have higher sale price

# Does the sale price vary according to number of units?
livingspaces %>%
  ggplot(aes(`TOTAL UNITS`,`SALE PRICE`)) +
  geom_point() +
  geom_smooth(color = "red", size = 0.1, method = lm) +
  xlab("Number of Units") +
  ylab("Sale Price") +
  ggtitle("Sale prices by number of units")
# Relationship looks good for higher sale prices (i.e. some prices are probably invalid)

# Does the sale price vary according to the lot area of the property?
livingspaces %>%
  ggplot(aes(`LAND SQUARE FEET`,`SALE PRICE`)) +
  geom_point() +
  geom_smooth(color = "red", size = 0.1, method = lm) +
  xlab("Land Square Feet") +
  ylab("Sale Price") +
  ggtitle("Sale prices by area of lot")
# For reasonable-sized lots the relationship looks good

# Does the sale price vary according to the actual size of the living space (i.e gross square feet)?
livingspaces %>%
  ggplot(aes(`GROSS SQUARE FEET`,`SALE PRICE`)) +
  geom_point() +
  geom_smooth(color = "red", size = 0.1, method = lm) +
  xlab("Gross Square Feet") +
  ylab("Sale Price") +
  ggtitle("Sale prices by living area")
# Same as for lot size, but again the sale prices close to zero seem to affect the relationship.
# Consider these outliers for the model


# Data partitioning and further pre-processing
# --------------------------------------------

# Remove unnecessary columns
livingspaces <- livingspaces[, !(colnames(livingspaces) %in% c("X1", "BLOCK", "NEIGHBORHOOD", "BUILDING CLASS CATEGORY",
                                                               "TAX CLASS AT PRESENT", "LOT", "EASE-MENT", "BUILDING CLASS AT PRESENT",
                                                               "ADDRESS", "APARTMENT NUMBER", "ZIP CODE", "RESIDENTIAL UNITS",
                                                               "COMMERCIAL UNITS", "TAX CLASS AT TIME OF SALE", "YEAR BUILT", 
                                                               "BUILDING CLASS AT TIME OF SALE", "SALE DATE"))]
# Create data partition with 10% in the test/validation set
set.seed(1)
test_index <- createDataPartition(y = livingspaces$`SALE PRICE`, times = 1, p = 0.1, list = FALSE)
livingtrain <- livingspaces[-test_index,] 
livingtest <- livingspaces[test_index,]
rm(test_index)

# Training the models and using them to predict sale price in the test set
# ------------------------------------------------------------------------

# Definition of RMSE
RMSE <- function(true_price, predicted_price){
  sqrt(mean((true_price - predicted_price)^2))
}
# Note that due to the skewed price data the RMSE comes out very large
# These data should be log transformed but the model fits do not work on log transformed data
# RMSE does decrease by using models other than linear regression

# Model 1: Just predict that the sale price will be the mean
mu_hat <- mean(livingtrain$`SALE PRICE`)
meanRMSE <- RMSE(livingtest$`SALE PRICE`, mu_hat)

rmse_results <- data_frame(Model = "Mean sale price", RMSE = meanRMSE)
rmse_results

# Model 2: Use linear regression of only the Gross Square Feet
lmGross <- train(`SALE PRICE` ~ `GROSS SQUARE FEET`, data=livingtrain, method = "lm")
lmGross
predictedmodel2 <- predict(lmGross, livingtest)
lmGrossRMSE <- RMSE(livingtest$`SALE PRICE`, predictedmodel2)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Linreg Gross Area",
                                     RMSE = lmGrossRMSE ))
rmse_results %>% knitr::kable()

# Model 3: Use linear regression with all predictors
lmAll <- train(`SALE PRICE` ~ ., data=livingtrain, method = "lm")
lmAll
predictedmodel3 <- predict(lmAll, livingtest)
lmAllRMSE <- RMSE(livingtest$`SALE PRICE`, predictedmodel3)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Linreg All Predictors",
                                     RMSE = lmAllRMSE ))
rmse_results %>% knitr::kable()

# Model 4: Use linear regression but exclude the predictor 'Borough'
lmnoBorough <- train(`SALE PRICE` ~ BuildingAge + `TOTAL UNITS` + `LAND SQUARE FEET` + `GROSS SQUARE FEET`, data=livingtrain, method = "lm")
lmnoBorough
predictedmodel4 <- predict(lmnoBorough, livingtest)
lmnoBoroughRMSE <- RMSE(livingtest$`SALE PRICE`, predictedmodel4)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Linreg without Borough",
                                     RMSE = lmnoBoroughRMSE ))
rmse_results %>% knitr::kable()

# Model 5: Use lasso regression
# Tune for fraction (default uses 0.1, 0.5, 0.9)
fitLasso <- train(`SALE PRICE` ~ ., data=livingtrain, method = "lasso", 
                  tuneGrid = data.frame(fraction = seq(0.1, 1, 0.1)))
fitLasso
predictedmodel5 <- predict(fitLasso, livingtest)
lassoRMSE <- RMSE(livingtest$`SALE PRICE`, predictedmodel5)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Lasso regression",
                                     RMSE = lassoRMSE ))
rmse_results %>% knitr::kable()

# Model 6: Use principal component analysis
# Tune for ncomp (default uses 1, 2, 3)
fitPCR <- train(`SALE PRICE` ~ ., data=livingtrain, method = "pcr",
                tuneGrid = data.frame(ncomp = seq(1, 5, 0.5)))
fitPCR
predictedmodel6 <- predict(fitPCR, livingtest)
PCRRMSE <- RMSE(livingtest$`SALE PRICE`, predictedmodel6)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Principal component analysis",
                                     RMSE = PCRRMSE ))
rmse_results %>% knitr::kable()

# Model 7: Elasticnet regression
fitEnet <- train(`SALE PRICE` ~ ., data=livingtrain, method = "enet")
fitEnet
predictedmodel7 <- predict(fitEnet, livingtest)
EnetRMSE <- RMSE(livingtest$`SALE PRICE`, predictedmodel7)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Elasticnet regression",
                                     RMSE = EnetRMSE ))
rmse_results %>% knitr::kable()

# Model 8: Ridge regression
fitRidge <- train(`SALE PRICE` ~ ., data=livingtrain, method = "ridge",
                  tuneGrid = data.frame(lambda = seq(0, 0.1, 0.01)))
fitRidge
predictedmodel8 <- predict(fitRidge, livingtest)
RidgeRMSE <- RMSE(livingtest$`SALE PRICE`, predictedmodel8)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Ridge regression",
                                     RMSE = RidgeRMSE ))
rmse_results %>% knitr::kable()

# Model 9: Independent Component Regression
fitICR <- train(`SALE PRICE` ~ ., data=livingtrain, method = "icr",
                tuneGrid = data.frame(n.comp = seq(1, 5, 0.5)))
fitICR
predictedmodel9 <- predict(fitICR, livingtest)
ICRRMSE <- RMSE(livingtest$`SALE PRICE`, predictedmodel9)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="ICR regression",
                                     RMSE = ICRRMSE ))
rmse_results %>% knitr::kable()

# Model 10: Bayesian ridge regression
# Note: Longer running time and outputs iteration of t and m in the R Console
fitBridge <- train(`SALE PRICE` ~ ., data=livingtrain, method = "bridge")
fitBridge
predictedmodel10 <- predict(fitBridge, livingtest)
BridgeRMSE <- RMSE(livingtest$`SALE PRICE`, predictedmodel10)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Bayesian Ridge regression",
                                     RMSE = BridgeRMSE ))

# The final list of models sorted by increasing RMSE
rmse_results %>% 
  arrange(RMSE) %>%
  knitr::kable()

