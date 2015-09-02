# Function to determine the order of integration of a timeseries
# returns the results as a list
orderofintegration <- function(timeseries, maxlags = 16, selectlags = "AIC", sig = 1){
   #default max lag order is 16
   #default behavior is to test for lag structure up to maxlag by AIC calculation
   #default order of significance
   #load dependency libraries
   library(urca)
   
   order = 0
   trend = FALSE
   drift = FALSE
   
   repeat{
      itr = order
      df.trend = ur.df(timeseries, selectlags = selectlags, lags = maxlags, type = "trend")
      if(df.trend@teststat[1,3] > df.trend@cval[3,sig]){
         trend = TRUE
         if(df.trend@teststat[1,1] > df.trend@cval[1,sig]){
            order = order + 1
            if(df.trend@teststat[1,2] > df.trend@cval[2,sig]){drift = TRUE}
         }
      }else{
         df.drift = ur.df(timeseries, selectlags = selectlags, lags = maxlags, type = "drift")
         if(df.drift@teststat[1,2] > df.drift@cval[2,sig]){
            if(df.drift@teststat[1,1] > df.drift@cval[1,sig]){
               order = order + 1
               drift = TRUE
            }
         }
         df.none = ur.df(timeseries, selectlags = selectlags,lags = maxlags, type = "none")
         if(df.drift@teststat[1,2] < df.drift@cval[2,sig]){
            if(df.none@teststat[1,1] > df.none@cval[1,sig]){   
               order = order + 1
            }
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