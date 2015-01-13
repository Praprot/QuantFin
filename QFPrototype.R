library("zoo")
library("lubridate")

setwd("d:\\ProjectR\\DataQuantFin\\")

calcdate <- as.Date("2014-11-10")

# izračun datumov za donosnost
calcdatethreem <- calcdate - months(3)
calcdateoney <-   calcdate - months(12)
calcdatethreey <-   calcdate - months(36)
calcdatefivey <-   calcdate - years(5)

#branje podatkov
teh<-read.zoo("TEH.csv",sep=",", header = TRUE, format = "%Y-%m-%d")
tehb<-read.zoo("TEHBenchmark.csv",sep=",", header = TRUE, format = "%Y-%m-%d")

# izračun returnov
rets <- diff(teh) / lag(teh, k = -1) * 100
retsbench <- diff(tehb) / lag(tehb, k = -1) * 100

retsoy <- window(rets,end=calcdate,start=calcdateoney)
retsbenchoy <- window(retsbench,end=calcdate,start=calcdateoney)


RelativeFund <- cumsum(retsoy)+100
RelativeBenchmark <- cumsum(retsbenchoy)+100

# izris grafa Relativna uspešnost
plot_colors <- c("blue","red")
plot(RelativeFund,type="l",col=plot_colors[1],main="Relativna uspešnost",xlab="Čas",ylab="Relativna vrednost",ylim=range(RelativeFund,RelativeBenchmark))
lines(RelativeBenchmark,type="l",col=plot_colors[2])
legend("topleft", c("Sklad","Benchmark"), cex=0.8,col= plot_colors,lwd=2, bty="n");

tehret <-teh[which.min(index(teh) <= calcdatethreey)-1] 
tehret <-rbind(tehret,teh[which.min(index(teh) <= calcdateoney)-1])
tehret <-rbind(tehret,teh[which.min(index(teh) <= calcdatethreem)-1])
tehret <-rbind(tehret,teh[which.min(index(teh) <= calcdate)-1])

tehret <- as.data.frame(tehret)


tehbret <-tehb[which.min(index(tehb) <= calcdatethreey)-1]
tehbret <-rbind(tehbret,tehb[which.min(index(tehb) <= calcdateoney)-1])
tehbret <-rbind(tehbret,tehb[which.min(index(tehb) <= calcdatethreem)-1])
tehbret <-rbind(tehbret,tehb[which.min(index(tehb) <= calcdate)-1])

tehbret <- as.data.frame(tehbret)

simpleReturn <- function(x,y){
  result <- ((x/y)-1)*100
  return(result)
}

tehret$Ret <-  simpleReturn(tehret[4,1],tehret$tehret)
tehbret$Ret <- simpleReturn(tehbret[4,1],tehbret$tehbret)

FundReturn <- tehret$Ret
BenchmarkReturn <- tehbret$Ret
DiffReturn <- tehret$Ret - tehbret$Ret
Era <- c("36 Monhts","12 Months","3 Months","Now")

allreturns <- data.frame(Era,FundReturn,BenchmarkReturn,DiffReturn)
allreturns


