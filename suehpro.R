library(RCurl)
library(rjson)

APIkey <- 'AIzaSyBLgQ9onVxvyUvjgc1F-3dlhLbWhrBK5pk'

source('C:\\Users\\ddunn\\Desktop\\DD\\R\\win-library\\3.0\\RGoogleAnalytics\\R\\RGoogleAnalytics.R')
source('C:\\Users\\ddunn\\Desktop\\DD\\R\\win-library\\3.0\\RGoogleAnalytics\\R\\QueryBuilder.R')

query <- QueryBuilder()
access_token <- query$authorize()

ga <- RGoogleAnalytics()
ga.profiles <- ga$GetProfileData(access_token)
ga.profiles

query$Init(start.date='2012-06-18',end.date='2012-12-18',
           dimensions='ga:date,ga:pagePath',
           metrics='ga:visits,ga:pageviews,ga:timeOnPage',
           sort='ga:visits',
           #filters='',
           #segment='',
           max.results=99,
           table.id=paste('ga:',ga.profiles$id[1],sep='',collapse=','),
           access_token=access_token)

ga.data <- ga$GetReportData(query)
head(ga.data)

