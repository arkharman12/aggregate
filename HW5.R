library(forecast)
library(lubridate)
library(fpp)


# Set the working directory
setwd("/Users/singhh/Downloads/CSCI48900")

potholes <-read.csv(file="IndyMAC_RequestForService_POTHOLES.csv")

# Create year and month column
potholes$datetime=mdy_hms(potholes$OPENED)
potholes$month=ymd(paste0(year(potholes$datetime),"-",month(potholes$datetime),"-",day(potholes$datetime)))

# Aggregate TRASH_DIST by year and month
# potholes$TRASH_DIST=1
trashdist_agg=aggregate(TRASH_DIST~month,data=potholes,FUN=mean)

# Create time series object
ts_trashdist = ts(trashdist_agg$TRASH_DIST, 
               start=2011, 
               frequency=12)

# Fit exp smooth model
m_ets = ets(ts_trashdist) 
# Check accuracy
accuracy(m_ets) 
# Forecast 12 months into the future
f_ets = forecast(m_ets, h=12) 
plot(f_ets,main="Trash District Forecast ",ylab="Trash discricts per month")


# ARIMA
arimafit <- auto.arima(ts_trashdist)
accuracy(arimafit)
fcast<-forecast(arimafit,h=12)
plot(fcast)
