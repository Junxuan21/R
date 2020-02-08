#----------------------------------------------------------------------
# Introduction to Data Science
# Homework 2
# Junxuan Wu
#----------------------------------------------------------------------

# Question 1

# install.packages("sqldf")
library (sqldf)

data(CO2)
head(CO2)

aggregate(x=CO2[,c("uptake")], by=data.frame(CO2$Type), FUN="mean")
#      CO2.Type        x
# 1      Quebec 33.54286
# 2 Mississippi 20.88333



#----------------------------------------------------------------------
# Question 2

df <- data.frame(Died.At <- c(22,40,72,41),
                 Writer.At <- c(16, 18, 36, 36),
                 First.Name <- c("John", "Edgar", "Walt", "Jane"), 
                 Second.Name <- c("Doe", "Poe", "Whitman", "Austen"),
                 Sex <- c("MALE", "MALE", "MALE", "FEMALE"),
                 Date.Of.Death <- c("2015-05-10", "1849-10-07", "1892-0326","1817-07-18"),
                 stringsAsFactors = FALSE)

# class(df$Sex)
# [1] "character"

names(df) <- c('age_at_death','age_as_writer', 'first_name','surname','gender','date_died')

# names(df)
# [1] "age_at_death"  "age_as_writer" "first_name"    "surname"       "gender"       
# [6] "date_died" 

df$gender <- as.factor(df$gender)
# class(df$gender)
# [1] "factor"

# ?strsplit
# ?paste

library(lubridate)

splitdate <- strsplit(df$date_died,'-')
# class(splitdate)

jh_yr <- as.numeric(splitdate[[1]][1])-df$age_at_death[1]
# [1] 1993

john_bday <- c(as.character(jh_yr), splitdate[[1]][2], splitdate[[1]][3])
# [1] "1993" "05"   "10"  

# The first way to concatenate a string 
# john_bday1 <- paste(john_bday, collapse = '-')
# john_bday1
# [1] "1993-05-10"
# class(john_bday1)
# [1] "character"

#----------------------------------------------------------------------

# The second way to convert a strig into a date-time object
john_bday <- paste(john_bday, collapse = '')
john_bday <- parse_date_time(john_bday, "%y-%m-%d")
john_bday
# [1] "1993-05-10 UTC"
class(john_bday)
# [1] "POSIXct" "POSIXt" 

# dear professor, when I checked how to concateenate strings in R, 
# I found both paste() and paste0() functions
# so what's the difference between these two?
# Thanks so much for your answer.

#----------------------------------------------------------------------
# Question 3

library(reshape2)

product <- c("A", "B") 
height <- c(10,20) 
width <- c(5,10) 
weight <- c(2,NA) 
observations_wide <- data.frame(product, height, width, weight) 
observations_wide

new_ob <- melt(observations_wide, id.vars  = 'product',
     variable.name = 'Attribute', na.rm = TRUE)

new_ob_sorted <- new_ob[order(new_ob$product),]
new_ob_sorted

# product Attribute value
#       A    height    10
#       A     width     5
#       A    weight     2
#       B    height    20
#       B     width    10

#----------------------------------------------------------------------
# second option: using dplyr package

# library(dplyr)
# new_ob %>% arrange(new_ob$product)

# product Attribute value
#       A    height    10
#       A     width     5
#       A    weight     2
#       B    height    20
#       B     width    10


#----------------------------------------------------------------------
# Question 4

library(datasets)  
data(mtcars)  
# ? mtcars     
# ?split
# head(mtcars)

sapply(split(mtcars$mpg, mtcars$cyl), mean) 
#        4        6        8 
# 26.66364 19.74286 15.10000 

# The answer is C


#----------------------------------------------------------------------
# Question 5
data(mtcars)  

hspw <- sapply(split(mtcars$hp, mtcars$cyl), mean) 
hp_diff <- hspw[[3]] - hspw[[1]]
hp_diff
# [1] 126.5779


#----------------------------------------------------------------------
# Question 6

mean(airquality$Ozone, na.rm = TRUE)
# [1] 42.12931

# The answer is A


#----------------------------------------------------------------------
# Question 7

sapply(split(airquality$Temp, airquality$Month), mean)
#        5        6        7        8        9 
# 65.54839 79.10000 83.90323 83.96774 76.90000 

# The answer is D


#----------------------------------------------------------------------
# Question 8

head(mtcars)

boxplot(mtcars$mpg ~ as.factor(mtcars$cyl),
        main = 'Car Millage Data',
        xlab = 'Number of Cylinders',
        ylab = 'Miles Per Gallon')



#----------------------------------------------------------------------
# Question 9

# install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(mtcars$wt, mtcars$disp, mtcars$mpg, 
              highlight.3d=TRUE, col.axis='PURPLE', col.grid='GREEN',
              pch=20, xlab="wt", ylab="disp", zlab="mpg"
              )


#----------------------------------------------------------------------
# Question 10
head(airquality)

# method 1:
aq_trimmed <- airquality[complete.cases(airquality[,]),]
# aq_trimmed

#----------------------------------------------------------------------
# method 2:
# aq_has_na <- apply(airquality,1,function(x){any(is.na(x))})
# sum(aq_has_na)
# aq_clean <- airquality[!aq_has_na,]


plot(aq_trimmed$Ozone, aq_trimmed$Temp, pch = 20, cex = 0.8, 
     xlab = 'Ozone',ylab = 'Temp', col='orange')


plot(aq_trimmed$Ozone, aq_trimmed$Temp, pch = 20, cex = 0.8,
     xlim = c(min(aq_trimmed$Ozone),100), ylim = c(min(aq_trimmed$Temp),80),
     xlab = 'Ozone',ylab = 'Temp', col='red')

