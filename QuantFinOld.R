# old test
#install.packages("RODBC")
#library("RODBC")
#channel <- odbcConnect("MOTHER", uid="mmarko", pwd="markopraprotnik")
#sqlTables(channel)
#sqlQuery(channel, "select f.imscode,
#fd.NavPerUnit,
#fd.NavDate 
#from Mot_FundData fd
#inner join Mot_Fund f on f.fundid = fd.fundid
#where NavDate > '20140101'
#order by fd.NavDate,fd.FundId")
#close(channel)


install.packages("zoo")
library("zoo")

setwd("d:\\ProjectR\\DataQuantFin\\")

teh<-read.zoo("TEH.csv",sep=",", header = TRUE, format = "%Y-%m-%d")
tehb<-read.zoo("TEHBenchmark.csv",sep=",", header = TRUE, format = "%Y-%m-%d")

plot(teh, main = "KD Technology Historical NavPerUnits ",ylab = "Price (EUR)", xlab = "Date")

plot(tehb, main = "KD Technology Benchmark ",ylab = "Price (EUR)", xlab = "Date")

head(teh)

tail(teh)

teh[which.max(teh)]
teh[which.min(teh)]

ret_simple <- diff(teh) / lag(teh, k = -1) * 100
ret_cont <- diff(log(teh)) * 100

summary(coredata(ret_simple))

ret_simple[which.max(ret_simple)]
ret_simple[which.min(ret_simple)]

hist(ret_simple, breaks=100, main = "Histogram of Simple Returns KD Technlogy",xlab="%")

teh_y <- window(teh, start = '2013-11-06', end = '2014-11-06')
teh_y[which.max(teh_y)]

ret_simple_y <- diff(teh_y) / lag(teh_y, k = -1) * 100
ret_cont_Y <- diff(log(teh_y)) * 100

quantile(ret_cont_Y, probs = 0.05)