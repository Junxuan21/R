#----------------------------------------------------------------------
# Introduction to Data Science
# Homework 3
# Junxuan Wu
#----------------------------------------------------------------------

# Question 1

# install.packages("ISLR")
library(ISLR)

data(Auto)
# head(Auto)

# plot(Auto$mpg, Auto$horsepower)

model_1 <- lm(Auto$mpg ~ Auto$horsepower)
summary(model_1)

# Call:
#  lm(formula = Auto$mpg ~ Auto$horsepower)

# Residuals:
#    Min       1Q   Median       3Q      Max 
# -13.5710  -3.2592  -0.3435   2.7630  16.9240 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     39.935861   0.717499   55.66   <2e-16 ***
#  Auto$horsepower -0.157845   0.006446  -24.49   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 4.906 on 390 degrees of freedom
# Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049 
# F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16

#----------------------------------------------------------------------

# There is a relationship between the predictor and the response variable,
# as the p value is extremly small and there are three * behind each coefficient.
# the R^2 of 0.6059 indicates a moderate relationship 
# the relationship is negative because the coefficient is negative 

model_1$coeff
# (Intercept) Auto$horsepower 
# 39.9358610      -0.1578447 


pdt_mpg <- model_1$coeff[1] + 98 * model_1$coeff[2]
pdt_mpg 
# (Intercept) 
#    24.46708


plot(Auto$mpg ~ Auto$horsepower, col="blue",pch=20)

abline(coef = coef(model_1),col="red",lwd=3)

# produce diagnostic plots of the model

par(mfrow = c(2,2))
plot(model_1)

# from the first two plots, 
# we can see that the residuals are distributed normally
# as they are close to straight line.


#----------------------------------------------------------------------
# Question 2

mpg01 <- ifelse(Auto$mpg > median(Auto$mpg),mpg01 <-1, mpg01<-0)
# head(mpg01)

new_data <- cbind(Auto, mpg01)
# tail(new_data)


par(mfrow = c(1,1))
plot(new_data)

cor1<- cor(new_data$cylinders, new_data$mpg)
cor2<- cor(new_data$displacement, new_data$mpg)
cor3<- cor(new_data$weight, new_data$mpg)
cor4<- cor(new_data$cylinders, new_data$displacement)
cor5<- cor(new_data$cylinders, new_data$weight)
cor6<- cor(new_data$year, new_data$mpg)

plot(new_data$cylinders, new_data$mpg)
plot(new_data$displacement, new_data$mpg)
plot(new_data$weight, new_data$displacement)
plot(new_data$cylinders, new_data$displacement)
plot(new_data$year, new_data$mpg)

# from the plots and the above correlations, we can see that
# cyclindrers and displacement are negatively correlated with mpg,
# yet cyclinders and displacement are highly correlated;
# weight is also positively correlated with displacement;
# year is positively correlated with mpg.
# so it is better to use one year variable and another one from weight, 
# cylinders or displacement as response variables to predict mpg.

n <- nrow(new_data)
ntrain <- round(n*0.7)

set.seed(210)   

tindex <- sample(n, ntrain)   

train_auto <- new_data[tindex,]  
test_auto <- new_data[-tindex,] 

# using all variables in the initial model and we can see that
# year and weight have the lowest p-value

formula <- mpg01 ~ cylinders + horsepower + 
  displacement + weight + acceleration + 
  year + origin

model_ini <- glm(formula, data=train_auto, family="binomial")
summary(model_ini)


# only use the year and weight to produce a second model

model_2 <- glm(mpg01 ~ year + weight, 
               data=train_auto, family=binomial)

summary(model_2)

# Call:
#  glm(formula = mpg01 ~ year + weight, family = binomial, data = train_auto)

# Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-2.18926  -0.18846  -0.00356   0.26421   2.44667  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.713e+01  5.371e+00  -3.188  0.00143 ** 
#  year         4.225e-01  8.426e-02   5.014 5.32e-07 ***
#  weight      -5.380e-03  7.281e-04  -7.390 1.47e-13 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 379.32  on 273  degrees of freedom
#Residual deviance: 123.76  on 271  degrees of freedom
#AIC: 129.76

#Number of Fisher Scoring iterations: 7


pred <- predict(model_2,  test_auto, type = 'response')

pred <- ifelse(pred > 0.5, 1, 0)

library(caret)

# ?confusionMatrix

pred <- as.factor(pred)
mpg01 <- as.factor(test_auto$mpg01)

cm <- confusionMatrix(data=pred, reference = mpg01)
#cm

accuracy <- round(cm$overall['Accuracy'],2)

test_error <- 1-as.numeric(accuracy)
test_error
# [1] 0.07


#----------------------------------------------------------------------
# Question 3

# head(iris)

set.seed(210)

df_iris <- data.frame(iris$Sepal.Length, iris$Sepal.Width)

# head(df_iris)

kc <- kmeans(df_iris,centers=3)

kc$cluster
#  [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [37] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 2 1 2 1 2 1 2 2 2 2 2 2 1 2 2 2 2 2 2
# [73] 2 2 1 1 1 1 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1 1 1 1 2 1
# [109] 1 1 1 1 1 2 2 1 1 1 1 2 1 2 1 2 1 1 2 2 1 1 1 1 1 2 2 1 1 1 2 1 1 1 2 1
# [145] 1 1 2 1 1 2

kc$centers
#   iris.Sepal.Length iris.Sepal.Width
# 1          6.812766         3.074468
# 2          5.773585         2.692453
# 3          5.006000         3.428000

plot(iris$Sepal.Length, iris$Sepal.Width, col=kc$cluster,pch=20,cex=1)

points(kc$centers,col=1:3,pch=4,cex=2,lwd=3)



