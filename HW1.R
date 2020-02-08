#----------------------------------------------------------------------
# Introduction to Data Science
# Homework 1
# Junxuan Wu
#----------------------------------------------------------------------

# Question 1
# ?matrix

mat <- matrix(1:12, nrow=3, byrow=TRUE)
mat
#     [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]    5    6    7    8
#[3,]    9   10   11   12

mat <- rbind(9, mat)
mat
#     [,1] [,2] [,3] [,4]
#[1,]    9    9    9    9
#[2,]    1    2    3    4
#[3,]    5    6    7    8
#[4,]    9   10   11   12

mat <- cbind(8,mat)
mat
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    8    9    9    9    9
#[2,]    8    1    2    3    4
#[3,]    8    5    6    7    8
#[4,]    8    9   10   11   12

#----------------------------------------------------------------------
# Question 2

lst <- list(student = c('Ellen','Catherine','Stephen'),
            score = c(90, 95, 99), 
            attend = matrix(c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE), 
                            nrow=2, byrow = TRUE))

# display name of all students
lst [[1]]
# [1] "Ellen"     "Catherine" "Stephen

# display Stephen's grade 
lst [[2]][3]
# [1] 99

# display Caterhine's attendance for both sessions
lst [[3]][,2]
# [1]  TRUE FALSE


#----------------------------------------------------------------------
# Question 3

# solution 1: 
gender = c( rep('male',20), rep ("female", 30))
table(gender)
# gender
# female   male 
#     30     20 

# class(gender)
# [1] "character"


# solution 2: used factor method but changed gender from character vector to factor
# which way is correct?

# gender = factor (c(rep('male',20), rep('female',30)),
#                 levels = c('male','female'))

# class(gender)
# [1] "factor"
# table(gender)
# gender
# female   male 
#     30     20 


#----------------------------------------------------------------------
# Question 4

data("airquality") 
# head(airquality)
# how many missing values are in the Ozone column of this data frame? 

ozn <- subset (airquality, select = Ozone)
miss <- sum(is.na(ozn))
miss
# [1] 37

#----------------------------------------------------------------------
# Question 5
filt <- subset (airquality, Ozone > 31 & Temp > 90, 
                select = Solar.R )
colMeans(filt)
# Solar.R 
#   212.8 


#----------------------------------------------------------------------
# Question 6
newdt<- data.frame(airquality)

newdt$hotcold <- ifelse (newdt$Temp > median(newdt$Temp),newdt$hotcold <- "hot", newdt$hotcold <- "cold" )

head(newdt)
#  Ozone Solar.R Wind Temp Month Day hotcold
#1    41     190  7.4   67     5   1    cold
#2    36     118  8.0   72     5   2    cold
#3    12     149 12.6   74     5   3    cold
#4    18     313 11.5   62     5   4    cold
#5    NA      NA 14.3   56     5   5    cold
#6    28      NA 14.9   66     5   6    cold

tail(newdt)
#    Ozone Solar.R Wind Temp Month Day hotcold
#148    14      20 16.6   63     9  25    cold
#149    30     193  6.9   70     9  26    cold
#150    NA     145 13.2   77     9  27    cold
#151    14     191 14.3   75     9  28    cold
#152    18     131  8.0   76     9  29    cold
#153    20     223 11.5   68     9  30    cold


#----------------------------------------------------------------------
# Question 7
number <- function (a=1:100)  {
  for (a in (1:100)) {
        if (a%%3 == 0 & a%%5 == 0) {
              print ('FizzBuzz') }
        else if (a%%3 == 0) {
              print ('Fizz')}
        else if (a%%5 == 0) {
              print ('Buzz') }
        else { print (a) }
  } }
# number(a)


#----------------------------------------------------------------------
# Question 8
# compute the sum of each row plus 2
mat1 <- matrix(rep(seq(4), 4), ncol = 4) 
mat1
rowsum <- rowSums(mat1) +2
rowsum
#[1]  6 10 14 18


#----------------------------------------------------------------------
# Question 9
set.seed(210)
state <- sample (state.name, 10)
state
# [1] "Montana"       "South Dakota"  "Texas"         "Massachusetts" "Indiana"      
# [6] "Arkansas"      "Illinois"      "Alaska"        "Maryland"      "Florida"  
sort(state)
# [1] "Alaska"        "Arkansas"      "Florida"       "Illinois"      "Indiana"      
# [6] "Maryland"      "Massachusetts" "Montana"       "South Dakota"  "Texas"   


#----------------------------------------------------------------------
# Question 10
#  ?Sys.time() 
#  ?as.POSIXct()
#  ?strptime

x <- Sys.time()
x
#[1] "2019-05-02 13:26:48 PDT"

xct <- strptime( 'July 20, 1969 20:18','%B %d, %Y %H:%M', tz = 'GMT')
xct
# [1] "1969-07-20 20:18:00 GMT"
# class(xct)
# [1] "POSIXlt" "POSIXt" 

day <- x - xct
day

x-xct
# Time difference of 18183.01 days

year <- as.numeric(x-xct)/365
year
# [1] 49.81649
