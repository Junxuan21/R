library(xgboost)
library(MASS)
attach(Boston)
train = sample(1 : dim(Boston)[1], round(0.90 * dim(Boston)[1]), replace = FALSE)
test = setdiff(1 : dim(Boston)[1], train)
validate = sample(train, 0.2 * length(train), replace = FALSE)
train = setdiff(train, validate)
# separate training and testing set
predictors_train = as.matrix(subset(Boston[train,], select = -medv))
# group all predictors into a matrix
response_train = Boston$medv[train]
# single out the response
train_Boston = xgb.DMatrix(predictors_train, label = response_train)
# transform training data into xgb.DMatrix form
predictors_validate = as.matrix(subset(Boston[validate,], select = -medv))
response_validate = Boston$medv[validate]
validate_Boston = xgb.DMatrix(predictors_validate, label = response_validate)
predictors_test = as.matrix(subset(Boston[test,], select = -medv))
response_test = Boston$medv[test]
test_Boston = xgb.DMatrix(predictors_test, label = response_test)
# same for validating and testing set.

params = list(booster = 'gbtree',    
              # which model to use (Yes, there are other models apart from gradient tree boosting).
              # default: 'gbtree' for gradient tree boosting
              eta = 1, 
              # shrinkage tuning parameter introduced at section 2.3. Default: 1
              max_depth = 6, 
              # max depth of a tree. Default: 6
              subsample = 0.7, 
              # subsample ratio for observations as in section 2.3. Default: 1
              gamma = 1,
              # penalty on number of leaves in a tree, the gamma in section 2.1. Default: 0
              lambda = 1,
              # penalty on score 2-norm, the lambda in section 2.1. Default: 1
              colsample_bytree = sqrt(dim(predictors_train)[2]) / sqrt(dim(predictors_train)[2]),
              # subsample ratio for features as in section 2.3. Default: 1
              min_child_weight = 1,
              # minimum sum of instance scores in a leaf. 
              # If the sum is smaller than this minimim in some leaf, then the leaf will be dropped. 
              # So the less this value, the smaller this tree is. Default: 1
              objective = 'reg:linear',
              # form of loss function
              # default: 'reg:linear' for linear function
              # other options include: 
              # 'reg:logistic' for logistic function for numeric response at [0,1]
              # 'binary:logistic' for logistic function for binary response
              eval_metric = 'rmse'
              # ways to evaluate the model on validation set, default depends on objective
              # options like: 'rmse'(default for regression), 'error'(default for classification), 'logloss', 'auc'
)
# construct params list
watchlist = list(train = train_Boston, eval = validate_Boston)
# make function print model's evaluation
boston_model = xgb.train(data = train_Boston, params = params, watchlist = watchlist,
                         nrounds = 10,
                         # max number of iterations, i.e. trees grown
                         early_stopping_rounds = 2
                         # if the model performance keep becoming worse after k rounds of iteration, 
                         # the algorithm will automatically stop.
                         # here we set k = 2
)

# train the model
# two other interesting parameters in xgb.train is obj, allowing you to customize your own loss function,
# the other is fevel, allowing you to customize your own way evaluating the model
predict_bostion = predict(boston_model, test_Boston)
# do prediction based on current model
head(Boston$medv[test])
head(predict_bostion)
# see how important of each feature
feature_name = names(Boston)
feature_name = feature_name[-length(feature_name)]
# take out names of features
importance_matrix <- xgb.importance(model = boston_model, feature_names = feature_name)
# calculate the importance of every feature
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
# see how the trees look like
library(DiagrammeR)
xgb.plot.tree(feature_names = feature_name, model = boston_model, n_first_tree = 0, plot_width = 1000, plot_height = 1000, color = 'black')

cv_boston = xgb.cv(params = params, data = train_Boston, nrounds = 10, early_stopping_rounds = 2,
                   nfold = 10,
                   # folds of cv
                   prediction = TRUE
                   # show prediction result
)
