library(lubridate)
library(ggplot2)
library(dplyr) 

X <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/04-12-2020.csv"))
head(X)
Y <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))
head(Y)


georgia<- filter(Y, Province_State == 'Georgia')
print(georgia)
georgia[1,]
rownames(georgia)
temp <-colnames(georgia)
georgia[1,12]

covid_cases <- numeric()
dates <- character()
#change 245 later
for (i in 12:245) {
  sum <-0
  for (j in 1:161) {
    sum <- sum + georgia[j,i]
    print(c(i,j,sum))
    #covid_cases[i-11] <- georgia[1,i]
    #temp1 <- substring(temp[i],2,10)
    #dates[i-11] <- temp1
  }
  covid_cases[i-11] <- sum
  temp1 <- substring(temp[i],2,10)
  dates[i-11] <- temp1
  }
  
print(covid_cases)
covid_cases[1]
new_dates = as.Date(dates,format="%m.%d.%y")
barplot(covid_cases)

barplot(covid_cases,
        main = "Daily Covid Cases",
        xlab = "Date",
        ylab = "No. of Cases",
        names.arg = dates,
        col = "firebrick1",
        horiz = FALSE)

##create a time series object
ts_dates <- seq(min(new_dates), max(new_dates), by = "day")
covid_ts <- ts(covid_cases, start = c(2020, as.numeric(format(ts_dates[1], "%j"))), frequency = 365)
#plot the time series
plot.ts(covid_ts)
#HoltWinters prediction
#gamma = FALSE - non seasonal model
#beta=FALAE - exponential smoothing
covid_pred <- HoltWinters(covid_ts, beta=TRUE, gamma=FALSE)
covid_pred 
covid_pred$fitted
plot(covid_pred)
#accuracy (sum of squared errors)
covid_pred$SSE
library("forecast")
#future forecasts
pred_next100 <- forecast:::forecast.HoltWinters(covid_pred, h=100)
pred_next100
plot(pred_next100)
