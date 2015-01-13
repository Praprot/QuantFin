library("zoo")
library("lubridate")

setwd("d:\\ProjectR\\DataQuantFin\\")

calcdate <- as.Date("2014-11-24")

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

#enoletni podatki
data.fund.oneyear <- window(data.fund,end=calcdate,start=calcdateoney + days(1))
data.benchmark.oneyear <- window(data.benchmark,end=calcdate,start=calcdateoney + days(1))

# izračun returnov
fund.returns <- diff(data.fund) / lag(data.fund, k = -1)
benchmark.returns <- diff(data.benchmark) / lag(data.benchmark, k = -1)

fund.returns.oneyear <- window(fund.returns,end=calcdate,start=calcdateoney + days(1))
benchmark.returns.oneyear <- window(benchmark.returns,end=calcdate,start=calcdateoney)

# standardna deviacija in relative risk
fund.stdev <- sd(fund.returns.oneyear) * sqrt(length (fund.returns.oneyear)) 
bench.stdev <- sd(benchmark.returns.oneyear) * sqrt(length (benchmark.returns.oneyear)) 

#stdev1 <- sqrt(sum((retsoy - mean(retsoy))^2) / length (retsoy))
#stdev2 <- sqrt(sum((retsoy - mean(retsoy))^2) / (length (retsoy)-1))
#stdev1 <- stdev1 * sqrt(length (retsoy))  
#stdev2 <- stdev2 * sqrt(length (retsoy)-1)

fund.stdev*100
bench.stdev*100
relativerisk <- fund.stdev/bench.stdev
relativerisk

#maximum drawdown
fund.maxdrawdown <- (data.fund.oneyear/max(data.fund.oneyear)-1)
fund.maxdrawdown[which.min(fund.maxdrawdown)]

bench.maxdrawdown <- (data.benchmark.oneyear/max(data.benchmark.oneyear)-1)
bench.maxdrawdown[which.min(bench.maxdrawdown)]

# izris grafa Relativna uspešnost
rets <- fund.returns * 100
retsbench <- benchmark.returns * 100

RelativeFund <- cumsum(fund.returns.oneyear)+100
RelativeBenchmark <- cumsum(benchmark.returns.oneyear)+100

plot_colors <- c("blue","red")
plot(RelativeFund,type="l",col=plot_colors[1],main="Relativna uspešnost",xlab="Čas",ylab="Relativna vrednost",ylim=range(RelativeFund,RelativeBenchmark))
lines(RelativeBenchmark,type="l",col=plot_colors[2])
legend("topleft", c("Sklad","Benchmark"), cex=0.8,col= plot_colors,lwd=2, bty="n");

tehret <-data.fund[which.min(index(data.fund) <= calcdatethreey)-1] 
tehret <-rbind(tehret,data.fund[which.min(index(data.fund) <= calcdateoney)-1])
tehret <-rbind(tehret,data.fund[which.min(index(data.fund) <= calcdatethreem)-1])
tehret <-rbind(tehret,data.fund[which.min(index(data.fund) <= calcdate)-1])

tehret <- as.data.frame(tehret)

tehbret <-data.benchmark[which.min(index(data.benchmark) <= calcdatethreey)-1]
tehbret <-rbind(tehbret,data.benchmark[which.min(index(data.benchmark) <= calcdateoney)-1])
tehbret <-rbind(tehbret,data.benchmark[which.min(index(data.benchmark) <= calcdatethreem)-1])
tehbret <-rbind(tehbret,data.benchmark[which.min(index(data.benchmark) <= calcdate)-1])

tehbret <- as.data.frame(tehbret)

tehret$Ret <-  simpleReturn(tehret[4,1],tehret$tehret)
tehbret$Ret <- simpleReturn(tehbret[4,1],tehbret$tehbret)

FundReturn <- tehret$Ret
BenchmarkReturn <- tehbret$Ret
DiffReturn <- tehret$Ret - tehbret$Ret
Era <- c("36 Monhts","12 Months","3 Months","Now")

allreturns <- data.frame(Era,FundReturn,BenchmarkReturn,DiffReturn)
allreturns

