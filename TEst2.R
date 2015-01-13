# izračun bete
library("zoo")
library("lubridate")
#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
setwd("d:\\ProjectR\\DataQuantFin\\")

calcdate <- as.Date("2014-11-10")

simpleReturn <- function(x,y){
  result <- ((x/y)-1)*100
  return(result)
}

# izračun datumov za donosnost
calcdatethreem <- calcdate - months(3)
calcdateoney <-   calcdate - months(12)
calcdatethreey <-   calcdate - months(36)
calcdatefivey <-   calcdate - years(5)

#branje podatkov
data.fund<-read.zoo("TEH.csv",sep=",", header = TRUE, format = "%Y-%m-%d")
data.benchmark <-read.zoo("TEHBenchmark.csv",sep=",", header = TRUE, format = "%Y-%m-%d")


# izračun returnov
fund.returns <- diff(data.fund) / lag(data.fund, k = -1)
benchmark.returns <- diff(data.benchmark) / lag(data.benchmark, k = -1)

fund.returns.oneyear <- window(fund.returns,end=calcdate,start=calcdateoney + days(1))
benchmark.returns.oneyear <- window(benchmark.returns,end=calcdate,start=calcdateoney + days(1))
 
ret <- Return.calculate(window(data.fund,end=calcdate,start=calcdateoney + days(1)),method="discrete")
retb <- Return.calculate(window(data.benchmark,end=calcdate,start=calcdateoney + days(1)),method="discrete")
chart.RelativePerformance(ret,retb,colorset=rich8equal,legend.loc="bottomleft",main="Relative Performance Technlology to Benchmark")



beta <- cov(fund.returns.oneyear,benchmark.returns.oneyear)/var(benchmark.returns.oneyear)

excess.returns.oneyear <- fund.returns.oneyear-benchmark.returns.oneyear

excess.return.sum <- sum(excess.returns.oneyear)

tracking.error <- sd(excess.returns.oneyear)*sqrt(length(excess.returns.oneyear)) 
tracking.error*100
information.ratio <- excess.return.sum/tracking.error/sqrt(length(excess.returns.oneyear)) 
