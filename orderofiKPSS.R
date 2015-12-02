# Function to determine the order of integration of a timeseries base on KPSS tests
# returns the results as a list
orderofiKPSS <- function(timeseries, selectlags = "short", sig = 2, type="mu"){
   #default behavior is to test for lag structure up to maxlag by AIC calculation
   #default significance level is 5%
   # supports both inclusion of deterministic trend and drift but defaults to drift only version
   # load dependency libraries
   library(urca)
   
   
   order = 0
   trend = FALSE
   drift = FALSE
   
   repeat{
      first = FALSE
      itr = order
      if(type == "tau"){
         kpss.trend = ur.kpss(timeseries, lags = selectlags, type = "tau")
         if(kpss.trend@teststat > kpss.trend@cval[1,sig]){
            trend = TRUE
            order = order + 1   
         }
      }
      
      if(type == "mu"){
         kpss.drift = ur.kpss(timeseries, lags = selectlags, type = "mu")
         if(kpss.drift@teststat > kpss.drift@cval[1,sig]){
            order = order + 1
            drift = TRUE         
         }
      }
      
      if(order == 0){
         tt = 1:length(timeseries)
         stationary <- lm(timeseries ~ tt)
         if(summary(stationary)$coefficients[1,4] < sig){drift = TRUE}
         if(summary(stationary)$coefficients[2,4] < sig){trend = TRUE}
      }
      #if order has increased the series will be differentiated
      #and the loop begins from the top
      timeseries <- diff(timeseries)
      #if order of integration has not increased the loop is terminated
      if(itr == order){
         break
      }
   }
   returnList <- list("order" = order, "drift" = drift, "trend" = trend)
   #print(c("order is ",order))
   #print(c("there is a drift ",drift))
   #print(c("there is a trend ",trend))
   returnList
}