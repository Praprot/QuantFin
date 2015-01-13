library("zoo")
#install.packages("lubridate")
library("lubridate")

setwd("d:\\ProjectR\\DataQuantFin\\")

calcdate <- as.Date("2014-11-10")

# izračun datumov za donosnost
calcdatethreem <- calcdate - months(3)
calcdateoney <-   calcdate - months(12)
calcdatethreey <-   calcdate - months(36)
calcdatefivey <-   calcdate - years(5)

#branje podatkov
#teh<-read.zoo("GALILEO.csv",sep=",", header = TRUE, format = "%Y-%m-%d")
tehw<-read.zoo("GALILEOWeek.csv",sep=",", header = TRUE, format = "%Y-%m-%d")

#tehb<-read.zoo("TEHBenchmark.csv",sep=",", header = TRUE, format = "%Y-%m-%d")
#tehbw<-read.zoo("TEHBenchmarkWeek.csv",sep=",", header = TRUE, format = "%Y-%m-%d")

# izračun returnov
#rets <- diff(teh) / lag(teh, k = -1) * 100
#retsbench <- diff(tehb) / lag(tehb, k = -1) * 100

retsw <- ((tehw / lag(tehw, k = -5))-1)
#retsbenchw <- ((tehbw / lag(tehbw, k = -5))-1) * 100

retsoyw <- window(retsw,end=calcdate,start=calcdateoney)
#retsbenchoyw <- window(retsbenchw,end=calcdate,start=calcdateoney)

tail(retsoyw)
head(retsoyw)
sd(retsoyw)*sqrt()

retsoy <- window(rets,end=calcdate,start=calcdateoney)
retsbenchoy <- window(retsbench,end=calcdate,start=calcdateoney)