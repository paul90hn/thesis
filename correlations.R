
setwd("C:/Users/USER/Desktop/Thesis/previous files")
library(forecast)
library(zoo)

#data0 <- read.csv("data3.csv")#data1.csv has log returns , data2.csv has normal returns of bonds
log.returns <- read.csv("logreturns.csv")
normal.returns <- read.csv("returns.csv")
data0 <- normal.returns
startMonth <- 02  #february
startYear <- 2008 

### setting variable time series ----
sp500 <- ts(data0$SP500, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)
ukx   <- ts(data0$UKX.GBP, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)
nikkei <- ts(data0$Nikkei.JPY, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
dax    <- ts(data0$DAX.EUR, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
euronext <- ts(data0$CAC.40.EUR, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
brent <- ts(data0$Brent, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
#eur <- ts(data0$eur, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
#jpy <- ts(data0$jpy, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
#gbp <- ts(data0$gbp, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
gold<- ts(data0$XAU.COMPN, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
dxy <- ts(data0$DXY, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)


tbill3m <- ts(data0$USGB090Y, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
germany3m <- ts(data0$GETB1.Index, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
japan3m <- ts(data0$GJGB3M, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)
france3m <- ts(data0$GBTF3MO, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)

ecb.m3 <- ts(data0$ECMSM3, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 
fed.m2 <- ts(data0$FED.M2, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)
cn.m2 <- ts(data0$CNMSM2, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)
shsz300 <- ts(data0$shsz300, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)



### Set hedging Asset ----


benchmark <- gold


### Volatilities ----
sp500.volatility <- sp500^2
ukx.volatility   <- ukx^2
nikkei.volatility <- nikkei^2
dax.volatility   <- dax^2
euronext.volatility <- euronext^2
gold.volatility <- gold^2
dxy.volatility <- dxy^2
tbill3m.volatility <- tbill3m^2
germany3m.volatility <- germany3m^2
france3m.volatility <- france3m^2
shsz300.volatility <- shsz300^2
fed.m2.volatility <- fed.m2^2
tbill3m.volatility <- tbill3m^2


benchmark.volatility <- benchmark^2


### linear regresions  ----

sp500.lm <- tslm(sp500 ~ benchmark)
sp500.coef <- sp500.lm$coefficients
#sp500.coef[2]   # the brent coefficient is the 2nd data point the the vector sp500.coef

ukx.lm <- tslm(ukx ~ benchmark)
ukx.coef <- ukx.lm$coefficients
nikkei.lm <-  tslm(nikkei ~ benchmark)
nikkei.coef <- nikkei.lm$coefficients
dax.lm <-  tslm(dax ~ benchmark)
dax.coef <- dax.lm$coefficients
euronext.lm <-  tslm(euronext ~ benchmark)
euronext.coef <- euronext.lm$coefficients
shsz300.lm <-  tslm(shsz300 ~ benchmark)
shsz300.coef <- shsz300.lm$coefficients


tbill3m.lm <- tslm(tbill3m ~ benchmark)
tbill3m.coef <- tbill3m.lm$coefficients
fed.m2.lm <- tslm(fed.m2 ~ benchmark)
fed.m2.coef <- fed.m2.lm$coefficients

benchmark.coef <- data.frame(sp500.coef[2], nikkei.coef[2], dax.coef[2], euronext.coef[2], tbill3m.coef[2], fed.m2.coef[2], shsz300.coef[2])                      
colnames(benchmark.coef) <- c("SP500", "NIKKEI", "DAX", "EURONEXT", "TBILL3M", "FED.M2", "SHSZ300")


###  Volatilities Linear Regression ----

sp500.vol.lm <- tslm(sp500.volatility ~ benchmark.volatility)
sp500.vol.coef <- sp500.vol.lm$coefficients
ukx.vol.lm <- tslm(ukx.volatility ~ benchmark.volatility)
ukx.vol.coef <- ukx.vol.lm$coefficients
nikkei.vol.lm <-  tslm(nikkei.volatility ~ benchmark.volatility)
nikkei.vol.coef <- nikkei.vol.lm$coefficients
dax.vol.lm <-  tslm(dax.volatility ~ benchmark.volatility)
dax.vol.coef <- dax.vol.lm$coefficients
euronext.vol.lm <-  tslm(euronext.volatility ~ benchmark.volatility)
euronext.vol.coef <- euronext.vol.lm$coefficients


tbill3m.vol.lm <- tslm(tbill3m.volatility ~ benchmark.volatility)
tbill3m.vol.coef <- tbill3m.vol.lm$coefficients
fed.m2.vol.lm <- tslm(fed.m2.volatility  ~ benchmark.volatility)
fed.m2.vol.coef <- fed.m2.vol.lm$coefficients
shsz300.vol.lm <-  tslm(shsz300.volatility ~ benchmark.volatility)
shsz300.vol.coef <- shsz300.vol.lm$coefficients


benchmark.vol.coef <- data.frame(sp500.vol.coef[2], nikkei.vol.coef[2], dax.vol.coef[2], euronext.vol.coef[2], tbill3m.vol.coef[2], fed.m2.vol.coef[2], shsz300.vol.coef[2])                          
colnames(benchmark.vol.coef) <- c("SP500", "NIKKEI", "DAX", "EURONEXT","TBILL3M", "FED.M2", "SHSZ300")

benchmark.coef.final <- rbind(benchmark.coef, benchmark.vol.coef)
benchmark.coef.final


### create recursive windows ----


#n <- matrix(  c(sp500.coef[2], nikkei.coef[2], dax.coef[2], euronext.coef[2],
#                sp500.vol.coef[2], nikkei.vol.coef[2], dax.vol.coef[2], euronext.vol.coef[2]), 
#              nrow = 2, ncol = 4, byrow = TRUE )

win.sp500 <- ts(sp500, c(startYear,startMonth), c(startYear,(startMonth + 99)), frequency = 12)
win.ukx   <- ts(ukx, c(startYear,startMonth), c(startYear,(startMonth + 99)), frequency = 12)
win.nikkei <- ts(nikkei, c(startYear,startMonth), c(startYear,(startMonth + 99)), frequency = 12) 
win.dax    <- ts(dax, c(startYear,startMonth), c(startYear,(startMonth + 99)), frequency = 12)
win.euronext <- ts(euronext, c(startYear,startMonth), c(startYear, (startMonth + 99)), frequency = 12) 
win.benchmark <- ts(benchmark, c(startYear,startMonth), c(startYear,(startMonth + 99)), frequency = 12)

win.tbill3m <- ts(tbill3m, c(startYear,startMonth), c(startYear, (startMonth + 99)), frequency = 12) 
win.fed.m2 <- ts(fed.m2, c(startYear,startMonth), c(startYear, (startMonth + 99)), frequency = 12) 

### Recursive windows correlations ----

rec_cor_sp500_benchmark <- cor(win.sp500,win.benchmark)
rec_cor_ukx_benchmark <- cor(win.ukx,win.benchmark)
rec_cor_nikkei_benchmark <- cor(win.nikkei,win.benchmark)
rec_cor_dax_benchmark <- cor(win.dax,win.benchmark)
rec_cor_euronext_benchmark<- cor(win.euronext,win.benchmark)
rec_cor_tbill3m_benchmark<- cor(win.tbill3m,win.benchmark)
rec_cor_fed.m2_benchmark<- cor(win.fed.m2,win.benchmark)

e <- 1
while (length(win.benchmark)<= length(benchmark)) {
  win.sp500 <- sp500[1: (100 + e)]
  win.ukx <- ukx[1: (100 + e)]
  win.nikkei<- nikkei[1: (100 + e)]
  win.dax<- dax[1: (100 + e)]
  win.euronext<- euronext[1: (100 + e)]
  win.benchmark<-  benchmark[1:(100 + e)]
  win.tbill3m<-  tbill3m[1:(100 + e)]
  win.fed.m2<-  fed.m2[1:(100 + e)]
  
  
  rec_cor_sp500_benchmark <- append( rec_cor_sp500_benchmark, cor(win.sp500,win.benchmark))
  rec_cor_ukx_benchmark <- append(  rec_cor_ukx_benchmark , cor( win.ukx ,win.benchmark))
  rec_cor_nikkei_benchmark <- append(  rec_cor_nikkei_benchmark, cor( win.nikkei,win.benchmark))
  rec_cor_dax_benchmark <- append( rec_cor_dax_benchmark, cor(win.dax,win.benchmark))
  rec_cor_euronext_benchmark <- append( rec_cor_euronext_benchmark, cor( win.euronext,win.benchmark))
  rec_cor_tbill3m_benchmark <- append(  rec_cor_tbill3m_benchmark, cor(win.tbill3m,win.benchmark))
  rec_cor_fed.m2_benchmark <- append( rec_cor_fed.m2_benchmark, cor(win.fed.m2,win.benchmark))
  
  e <- e+1
  if (length(win.benchmark)== length(benchmark) ) break   
} 


recursive.sp500 <- ts(rec_cor_sp500_benchmark, start= c(startYear,(startMonth + 99)), frequency = 12)
recursive.ukx <- ts(  rec_cor_ukx_benchmark, start= c(startYear,(startMonth + 99)), frequency = 12)
recursive.nikkei <- ts(rec_cor_nikkei_benchmark , start= c(startYear,(startMonth + 99)), frequency = 12)
recursive.dax<- ts(rec_cor_dax_benchmark, start= c(startYear,(startMonth + 99)), frequency = 12)
recursive.euronext <- ts( rec_cor_euronext_benchmark , start= c(startYear,(startMonth + 99)), frequency = 12)







### create rolling windows  ----

win.sp500 <- ts(sp500, c(startYear,startMonth), c(startYear,(startMonth + 99)), frequency = 12)
win.ukx   <- ts(ukx, c(startYear,startMonth), c(startYear,(startMonth + 99)), frequency = 12)
win.nikkei <- ts(nikkei, c(startYear,startMonth), c(startYear,(startMonth + 99)), frequency = 12) 
win.dax    <- ts(dax, c(startYear,startMonth), c(startYear,(startMonth + 99)), frequency = 12)
win.euronext <- ts(euronext, c(startYear,startMonth), c(startYear, (startMonth + 99)), frequency = 12) 
win.benchmark <- ts(benchmark, c(startYear,startMonth), c(startYear,(startMonth + 99)), frequency = 12)

### Rolling windows ----

roll_cor_sp500_benchmark <- cor(win.sp500,win.benchmark)
roll_cor_ukx_benchmark <- cor(win.ukx,win.benchmark)
roll_cor_nikkei_benchmark<- cor(win.nikkei,win.benchmark)
roll_cor_dax_benchmark <- cor(win.dax,win.benchmark)
roll_cor_euronext_benchmark<- cor(win.euronext,win.benchmark)

e2 <- 1

while ((e2 + length(win.benchmark))<= length(benchmark)) {
  win.sp500 <- sp500[(1+e2) : (100+ e2)]
  win.ukx <- ukx[(1+e2) : (100+ e2)]
  win.nikkei <- nikkei[(1+e2) : (100+ e2)]
  win.dax <- dax[(1+e2) : (100+ e2)]
  win.euronext <- euronext[(1+e2) : (100+ e2)]
  win.benchmark<-  benchmark[(1+e2) : (100+ e2)]
  
 
  
  
  roll_cor_sp500_benchmark <- append( roll_cor_sp500_benchmark, cor(win.sp500,win.benchmark))
  roll_cor_ukx_benchmark <- append( roll_cor_ukx_benchmark, cor(win.ukx,win.benchmark))
  roll_cor_nikkei_benchmark <- append( roll_cor_nikkei_benchmark, cor(win.nikkei,win.benchmark))
  roll_cor_dax_benchmark <- append( roll_cor_dax_benchmark, cor(win.dax,win.benchmark))
  roll_cor_euronext_benchmark <- append( roll_cor_euronext_benchmark, cor(win.euronext,win.benchmark))
  
  
  
  if ((e2 + length(win.benchmark))== length(benchmark) ) break   
  e2 <- e2+1
} 

rolling.sp500 <- ts(roll_cor_sp500_benchmark, start= c(startYear,(startMonth + 99)), frequency = 12)
rolling.ukx <- ts(  roll_cor_ukx_benchmark, start= c(startYear,(startMonth + 99)), frequency = 12)
rolling.nikkei <- ts(roll_cor_nikkei_benchmark , start= c(startYear,(startMonth + 99)), frequency = 12)
rolling.dax<- ts(roll_cor_dax_benchmark, start= c(startYear,(startMonth + 99)), frequency = 12)
rolling.euronext <- ts( roll_cor_euronext_benchmark , start= c(startYear,(startMonth + 99)), frequency = 12)

#### create final table ----

sp500.correlations <- data.frame(recursive.sp500, rolling.sp500)
ukx.correlations <- data.frame(recursive.ukx, rolling.ukx)
nikkei.correlations <- data.frame(recursive.nikkei, rolling.nikkei)
dax.correlations <- data.frame(recursive.dax, rolling.dax)
euronext.correlations <- data.frame(recursive.euronext, rolling.euronext)



write.csv(data.frame(as.Date.yearmon(time(rolling.sp500)), sp500.correlations, ukx.correlations, 
                     nikkei.correlations,dax.correlations, euronext.correlations)
                     , file = "france3mCorrelations.csv", row.names = FALSE )

### ----



sp5002 <- ts(data0$SP500, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)
ukx2   <- ts(data0$UKX.GBP, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)
nikkei2 <- ts(data0$Nikkei.JPY, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12) 

euronext2  <- ts(data0$CAC.40.EUR, start=c(startYear,startMonth), end = c(2016, 12), frequency = 12)


dax2<- ts(data0$DAX.EUR[(7*12):length(data0$DAX.EUR)],    start=c(2014,01), end = c(2016, 12), frequency = 12) 



par(mfrow= c(2,1))
plot(dax, main= "DAX index Log returns")
abline(h=mean(dax))
abline(h=sd(dax), lty= "dashed")
abline(h=-sd(dax), lty= "dashed")
abline(v=c(2015,5), lwd= 2, col= "coral" )


plot(dax.volatility, main= "DAX index volatility")
abline(h=mean(dax.volatility))
abline(h=sd(dax.volatility), lty= "dashed")
abline(v=c(2015,5), lwd= 2, col= "coral" )



par(mfrow= c(2,1))
plot(euronext, main= "euronext index Log returns")
abline(h=mean(euronext))
abline(h=sd(euronext), lty= "dashed")
abline(h=-sd(euronext), lty= "dashed")
abline(v=c(2015,5), lwd= 2, col= "coral" )


plot(euronext.volatility, main= "euronext index volatility")
abline(h=mean(euronext.volatility))
abline(h=sd(euronext.volatility), lty= "dashed")
abline(v=c(2015,5), lwd= 2, col= "coral" )



par(mfrow= c(2,1))
plot(sp500, main= "sp500 index Log returns")
abline(h=mean(sp500))
abline(h=sd(sp500), lty= "dashed")
abline(h=-sd(sp500), lty= "dashed")
abline(v=c(2015,5), lwd= 2, col= "coral" )


plot(sp500.volatility, main= "sp500 index volatility")
abline(h=mean(sp500.volatility))
abline(h=sd(sp500.volatility), lty= "dashed")
abline(v=c(2015,5), lwd= 2, col= "coral" )



par(mfrow= c(2,1))
plot(gold, main= "gold Log returns")
abline(h=mean(gold))
abline(h=sd(gold), lty= "dashed")
abline(h=-sd(gold), lty= "dashed")
abline(v=c(2011,8), lwd= 2, col= "coral" )


plot(gold.volatility, main= "gold volatility")
abline(h=mean(gold.volatility))
abline(h=sd(gold.volatility), lty= "dashed")
abline(v=c(2011,8), lwd= 2, col= "coral" )





par(mfrow= c(3,1))
plot(gold.volatility, main= " Gold volatility")
abline(h=mean(gold.volatility))
abline(h=sd(gold.volatility), lty= "dashed")
abline(h=-sd(gold.volatility), lty= "dashed")
abline(v=c(2011,8), lwd= 2, col= "coral" )


plot(dax.volatility, main= "dax volatility")
abline(h=mean(dax.volatility))
abline(h=sd(dax.volatility), lty= "dashed")
abline(v=c(2015,5), lwd= 2, col= "coral" )

plot(euronext.volatility, main= "euronext volatility")
abline(h=mean(euronext.volatility))
abline(h=sd(euronext.volatility), lty= "dashed")
abline(v=c(2015,5), lwd= 2, col= "coral" )

par(mfrow= c(1,1))
plot(gold)
lines(shsz300, col= "Blue")

par(mfrow= c(1,1))
plot(rolling.dax, main= "Dax. gold correlations", ylim= c(-0.07,0.015))
lines(recursive.dax, col= "blue")




#### ----



gold.training.ts <- window(gold, start = c(2007, 02), end = c(2015,12))
gold.validation.ts <- window(gold, start = c(2016, 01), end = c(2016,12))


ann <- nnetar(gold.training.ts, repeats = 20, p = 11, P = 1, size = 7)
gold.nnetar.pred <- forecast(ann, h = 12) 
accuracy(gold.nnetar.pred , gold.validation.ts)


plot(shsz300, xlab = "Time", bty = "l", xaxt = "n", lty = 1)
abline(v= c(c(2015,01), c(2016,01),c(2017.01)))
lines(gold.nnetar.pred$fitted, lwd = 2, col = "blue") 
lines(gold.nnetar.pred$mean, lwd = 2, col = "red")  
lines(gold.validation.ts, col= "blue")

variables <- colnames(data0)[-1][-8][-11]
nn <- neuralnet(gold ~ sp500 + dax + nikkei + ukx + brent + eur + gbp + dxy + tbill3m + germany3m + japan3m +            
                france3m, data= data0, hidden =  c(4,4))
pr.nn <- compute(nn,data0[,2:14])

#### ----

cor.table <- cor(log.returns[-1])
gold.cor <- cor.table[,7]



