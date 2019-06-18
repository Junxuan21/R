#-----------------------------------------------------------------------------
# Supervised Machine Learning - Classification
#-----------------------------------------------------------------------------

# getwd()
# [1] "C:/Users/Laurel/Desktop/R"

# setwd <- ("/Users/Laurel/Desktop/R")
# getwd()
# [1] "C:/Users/Laurel/Desktop/R"

# loading dataset
housing <- read.csv("housing.csv")

str(housing)
# 'data.frame':	20640 obs. of  10 variables:
# $ longitude         : num  -122 -122 -122 -122 -122 ...
# $ latitude          : num  37.9 37.9 37.9 37.9 37.9 ...
# $ housing_median_age: num  41 21 52 52 52 52 52 52 42 52 ...
# $ total_rooms       : num  880 7099 1467 1274 1627 ...
# $ total_bedrooms    : num  129 1106 190 235 280 ...
# $ population        : num  322 2401 496 558 565 ...
# $ households        : num  126 1138 177 219 259 ...
# $ median_income     : num  8.33 8.3 7.26 5.64 3.85 ...
# $ median_house_value: num  452600 358500 352100 341300 342200 ...
# $ ocean_proximity   : Factor w/ 5 levels "<1H OCEAN","INLAND",..: 4 4 4 4 4 4 4 4 4 4 ...

head(housing)
#   longitude latitude housing_median_age total_rooms total_bedrooms population
# 1   -122.23    37.88                 41         880            129        322
# 2   -122.22    37.86                 21        7099           1106       2401
# 3   -122.24    37.85                 52        1467            190        496
# 4   -122.25    37.85                 52        1274            235        558
# 5   -122.25    37.85                 52        1627            280        565
# 6   -122.25    37.85                 52         919            213        413
#   households median_income median_house_value ocean_proximity
# 1        126        8.3252             452600        NEAR BAY
# 2       1138        8.3014             358500        NEAR BAY
# 3        177        7.2574             352100        NEAR BAY
# 4        219        5.6431             341300        NEAR BAY
# 5        259        3.8462             342200        NEAR BAY
# 6        193        4.0368             269700        NEAR BAY

summary(housing)
#   longitude         latitude     housing_median_age  total_rooms   
# Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2  
# 1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448  
# Median :-118.5   Median :34.26   Median :29.00      Median : 2127  
# Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636  
# 3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148  
# Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320  

#  total_bedrooms     population      households     median_income    
# Min.   :   1.0   Min.   :    3   Min.   :   1.0   Min.   : 0.4999  
# 1st Qu.: 296.0   1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5634  
# Median : 435.0   Median : 1166   Median : 409.0   Median : 3.5348  
# Mean   : 537.9   Mean   : 1425   Mean   : 499.5   Mean   : 3.8707  
# 3rd Qu.: 647.0   3rd Qu.: 1725   3rd Qu.: 605.0   3rd Qu.: 4.7432  
# Max.   :6445.0   Max.   :35682   Max.   :6082.0   Max.   :15.0001  
# NA's   :207                                                        
#  median_house_value   ocean_proximity
# Min.   : 14999     <1H OCEAN :9136  
# 1st Qu.:119600     INLAND    :6551  
# Median :179700     ISLAND    :   5  
# Mean   :206856     NEAR BAY  :2290  
# 3rd Qu.:264725     NEAR OCEAN:2658  
# Max.   :500001                      


# exploratory data analysis
# using histograms to get a sense for the distribution of numeric varibles
long_hist <- hist(housing$longitude, freq = TRUE, col = 'Red')

lat_hist <- hist(housing$latitude, freq = TRUE, col = 'Blue')

median_age_hist <- hist(housing$housing_median_age, freq = TRUE, col = 'Yellow')

rooms_hist <- hist(housing$total_rooms, freq = TRUE, col = 'Red')

bedrm_hist <- hist(housing$total_bedrooms, freq = TRUE, col = 'Blue')

pop_hist <- hist(housing$population, freq = TRUE, col = 'Green')

households_hist <- hist(housing$households, freq = TRUE, col = 'Purple')

median_income_hist <- hist(housing$median_income, freq = TRUE, col = 'Yellow')

median_hsvalue_hist <- hist(housing$median_house_value, freq = TRUE, col = 'Green')


# summary

# From the above graphs, we can see that the longitude, latitude and household mdeian age are well-distributed,
# while the rest numeric variables such as the number of total rooms and bedrooms and population have many outliers,
# the population and the households show similar distribution tendency yet still both have many outliers,
# likewise, the median household income shows the similar distribution tendency as the median house value.


#-----------------------------------------------------------------------------
# data munging
#-----------------------------------------------------------------------------

# impute the missing data values in the total_bedrooms variable
# using the median instead of mean in order to be less influenced by outliers

colSums(is.na(housing))
#  longitude           latitude housing_median_age        total_rooms 
#         0                  0                  0                  0 
#  total_bedrooms         population         households      median_income 
#       207                  0                  0                  0 
#  median_house_value    ocean_proximity 
#         0                  0 
 
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median(housing$total_bedrooms, na.rm=TRUE)

colSums(is.na(housing))
#  longitude           latitude housing_median_age        total_rooms 
#         0                  0                  0                  0 
#  total_bedrooms         population         households      median_income 
#         0                  0                  0                  0 
#  median_house_value    ocean_proximity 
#         0                  0 


# split the ocean_proximity variable into a number of binary categorical variables
NEAR_BAY <- data.frame(NEAR_BAY = (housing$ocean_proximity=='NEAR BAY'))

ONEHR_OCEAN <- data.frame(ONEHR_OCEAN = (housing$ocean_proximity=='<1H OCEAN'))

INLAND <- data.frame(INLAND = (housing$ocean_proximity=='INLAND'))

NEAR_OCEAN <- data.frame(NEAR_OCEAN = (housing$ocean_proximity=='NEAR OCEAN'))

ISLAND <- data.frame(ISLAND = (housing$ocean_proximity=='ISLAND'))

housing1 <- cbind(housing,NEAR_BAY,ONEHR_OCEAN,INLAND,NEAR_OCEAN,ISLAND)

head(housing1,3)
#  longitude latitude housing_median_age total_rooms total_bedrooms population
#1   -122.23    37.88                 41         880            129        322
#2   -122.22    37.86                 21        7099           1106       2401
#3   -122.24    37.85                 52        1467            190        496
#  households median_income median_house_value ocean_proximity NEAR_BAY
#1        126        8.3252             452600        NEAR BAY     TRUE
#2       1138        8.3014             358500        NEAR BAY     TRUE
#3        177        7.2574             352100        NEAR BAY     TRUE
#  ONEHR_OCEAN INLAND NEAR_OCEAN ISLAND
#1       FALSE  FALSE      FALSE  FALSE
#2       FALSE  FALSE      FALSE  FALSE
#3       FALSE  FALSE      FALSE  FALSE

housing2 <- housing1[, -10 ]

head(housing2,3)
#  longitude latitude housing_median_age total_rooms total_bedrooms population
#1   -122.23    37.88                 41         880            129        322
#2   -122.22    37.86                 21        7099           1106       2401
#3   -122.24    37.85                 52        1467            190        496
#  households median_income median_house_value NEAR_BAY ONEHR_OCEAN INLAND
#1        126        8.3252             452600     TRUE       FALSE  FALSE
#2       1138        8.3014             358500     TRUE       FALSE  FALSE
#3        177        7.2574             352100     TRUE       FALSE  FALSE
#  NEAR_OCEAN ISLAND
#1      FALSE  FALSE
#2      FALSE  FALSE
#3      FALSE  FALSE


# using mean instead of total to present bedrooms and rooms number variables

mean_bedrooms <- housing2$total_bedrooms/housing2$households

mean_rooms <- housing2$total_rooms/housing2$households

housing3 <- cbind(mean_rooms, mean_bedrooms, housing2)

housing3 <- housing3[, -6:-7 ]

# head(housing3,3)

# perform feature scaling on all numeric predictor variables so that
# the coefficients are given equal weight in ML models

# ?scale

scaled_housing <- scale(housing3[, 1:8])

head(scaled_housing, 3)

cleaned_housing <- cbind(housing3[, 10:14], scaled_housing, housing3[9])

head(cleaned_housing, 3)


#-----------------------------------------------------------------------------
# supervised machine learning - Classification
#-----------------------------------------------------------------------------

library(randomForest)

set.seed(210)

# create training and test sets

n <- nrow(cleaned_housing)
ntrain <- round(n*0.8)

tindex <- sample(n, ntrain)   

train_housing <- cleaned_housing[tindex,]  
test_housing <- cleaned_housing[-tindex,] 

# split data into X and y
train_X <- train_housing[, 1:13]
train_y <- train_housing[, 14]

rf = randomForest(train_X, y = train_y,
                  ntree = 500, importance = TRUE)

names(rf)
# [1] "call"            "type"            "predicted"       "mse"            
# [5] "rsq"             "oob.times"       "importance"      "importanceSD"   
# [9] "localImportance" "proximity"       "ntree"           "mtry"           
# [13] "forest"          "coefs"           "y"               "test"           
# [17] "inbag"   

rf$importance
#                   %IncMSE IncNodePurity
# NEAR_BAY            477415822  1.529511e+12
# ONEHR_OCEAN        1434977379  4.006863e+12
# INLAND             3853718420  2.984808e+13
# NEAR_OCEAN          487225276  2.202301e+12
# ISLAND                1350321  7.167922e+10
# mean_rooms         1851308773  2.111690e+13
# mean_bedrooms       454879308  7.765181e+12
# longitude          6943011030  2.543130e+13
# latitude           5527424019  2.193270e+13
# housing_median_age 1129618608  1.003958e+13
# population         1038235838  7.434902e+12
# households         1169424082  7.853248e+12
# median_income      8393878546  7.419552e+13


# from the randomForest algorithm, we can use MSE(Mean Squared Error) to measure a feature's importance,
# A higher number indicates a more important predictor.
# Here, we can see that median_income variable is the most important, followed by location variables
# when predicting a house's value.


#-----------------------------------------------------------------------------
# model performance evaluation
#-----------------------------------------------------------------------------

# with the randomForest algorithm, there is no need for a separate test set to get 
# an unbiased estimate of the test set error as it can be estimaed using out-of-bag (oob) error.

# Not specifying a data source forces OOB predictions
oob_prediction = predict(rf)

# Now compute the training set RMSE
train_mse = mean(as.numeric((oob_prediction -
                               train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse
#[1] 49071.56


# The resulting RMSE is the prediction of median price of a house in a given district
# to within a RMSE delta of the actual median house price.

# Next, using the test data to see how well the model predicts 
# by computing the test set RMSE

test_X <- test_housing[, 1:13]
test_y <- test_housing[, 14]

y_pred = predict(rf , test_X)

# Now compute the test set RSME
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse
#[1] 48464.6
 
# Compare the RMSE for the training and test set, we can see that 
# the model score roughly the same on the training and testing data,
# suggesting that it is not overfit and that it makes good predictions.
