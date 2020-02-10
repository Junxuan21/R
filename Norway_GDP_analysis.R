setwd('/Users/junxxx/Practice')

# load relevant packages
library("ggplot2")    
library("gridExtra")  
library("vars")
library("reshape")
library("fpp")


# function to plot
plot.data <- function(dataToPlot, titles) {
  list.of.plots <- list();
  plots = lapply(1:(ncol(dataToPlot)-1), 
                 function(i) ggplot(data = dataToPlot, aes(x = dataToPlot[,1], y = dataToPlot[,i+1], group = 1)) + 
                   geom_line() + ggtitle(titles[i]) +
                   xlab("Date") + ylab(titles[i]) +
                   scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 week", date_labels = "%Y"))
  do.call(grid.arrange, plots)
}


# load data
norway <- read.csv("Norway_GDP.csv")
norway <- as.data.frame(norway)
head(norway)


norway$year <- as.Date(norway$year)
names(norway) <- c("year", "ex", "rgdp", "inf", "rnx")

ggplot(norway, aes(x=year, y=inf)) + geom_line()
# plot data for visualization
plot.data(norway, c("Exchange", "Real GDP", "Inflation", "Real Net"))


adf.test(norway$ex)
adf.test(norway$rgdp) 
adf.test(norway$inf) 
adf.test(norway$rnx) 

#exchange
norway$exDiff <- c(NA, diff(norway$ex))


#rnx
norway$rnxDiff <- c(NA, diff(norway$rnx))
norway <- norway[complete.cases(norway),]
adf.test(norway$exDiff)
adf.test(norway$rnxDiff)



# ordering the variables according the reaction (moving from slow to fast)
Y <- norway[, ("rgdp","rnxDiff","inf","exDiff")] 
# 3 variables stored on column 2 to 4 of dataframe
m1 <- VAR(Y, p=2, type="const")  # estimate a VAR(1) with a constant term
summary(m1)  # summarize estimation output (coefficients, fit, etc.)
plot(m1)  # plot fit


forecast1 <- predict(m1, n.ahead=8, ci=0.60)
forecast1$endog <- tail(forecast1$endog, 10)  
# shorten our dataset to emphasize forecast of the future

# plot forecast
fanchart(forecast1)  

B0 <- matrix(c(NA,0,0,0,
               NA,NA,0,0,
               NA,NA,NA,0,
               NA,NA,NA,NA), nrow = 4, ncol = 4)
Sigma_e <- matrix(c(1,0,0,0,
                    0,1,0,0,
                    0,0,1,0,
                    0,0,0,1), nrow = 4, ncol = 4)

# estimate using maximum likelihood
estSModel <- SVAR(x = m1, estmethod = "direct",
                  Amat = B0, Bmat = Sigma_e, max.iter = 100, conv.crit = 10e-8)

# effects of an increase in Exchange Rate
nAhead = 10;
temp <- irf(estSModel, impulse=c('exDiff'), n.ahead = nAhead, ci = 0.90)

plot(temp)


# more plots
ff.irf <- temp$irf$exDiff
ff.lower <- temp$Lower$exDiff
ff.upper <- temp$Upper$exDiff
#"rgdp", "rnxDiff","inf","exDiff")

period = 1:(nAhead+1)
dtp <- data.frame(period,ff.irf[,1],ff.lower[,1],ff.upper[,1])
colnames(dtp) <- c("period","irf","lower", "upper")
ff.gdp <- ggplot(dtp, aes(x=period)) + 
  geom_line(aes(y = upper)) + 
  geom_line(aes(y = lower)) +
  geom_line(aes(y = irf)) + geom_point(aes(y = irf), shape = 1) +
  geom_ribbon(aes(x = period, ymax = upper, ymin = lower), fill = "grey12", alpha = .25) +
  labs(x="",y="rgdp")


dtp <- data.frame(period,ff.irf[,2],ff.lower[,2],ff.upper[,2])
colnames(dtp) <- c("period","irf","lower", "upper")
ff.rnx <- ggplot(dtp, aes(x=period)) + 
  geom_line(aes(y = upper)) + 
  geom_line(aes(y = lower)) +
  geom_line(aes(y = irf)) + geom_point(aes(y = irf), shape = 1) +
  geom_ribbon(aes(x = period, ymax = upper, ymin = lower), fill = "grey12", alpha = .25) +
  labs(x="",y="rnxDiff")

dtp <- data.frame(period,ff.irf[,3],ff.lower[,3],ff.upper[,3])
colnames(dtp) <- c("period","irf","lower", "upper")
ff.inf <- ggplot(dtp, aes(x=period)) + 
  geom_line(aes(y = upper)) + 
  geom_line(aes(y = lower)) +
  geom_line(aes(y = irf)) + geom_point(aes(y = irf), shape = 1) +
  geom_ribbon(aes(x = period, ymax = upper, ymin = lower), fill = "grey12", alpha = .25) +
  labs(x="",y="inf")

dtp <- data.frame(period,ff.irf[,4],ff.lower[,4],ff.upper[,4])
colnames(dtp) <- c("period","irf","lower", "upper")
ff.ex <- ggplot(dtp, aes(x=period)) + 
  geom_line(aes(y = upper)) + 
  geom_line(aes(y = lower)) +
  geom_line(aes(y = irf)) + geom_point(aes(y = irf), shape = 1) +
  geom_ribbon(aes(x = period, ymax = upper, ymin = lower), fill = "grey12", alpha = .25) +
  labs(x="",y="exDiff")

grid.arrange(ff.gdp, ff.rnx, ff.inf,ff.ex, ncol = 2)


