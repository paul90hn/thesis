
setwd("C:/Users/USER/Desktop/Thesis/weeklydata")
library(forecast)
library(zoo)
library(xts)
library(neuralnet)
library(psych)
library(nnet)



#####------
spx <- xts(data1$SPX, order.by = date1)
ukx <- xts(data1$UKX, order.by = date1)
dax <- xts(data1$DAX, order.by = date1)
shsz300 <- xts(data1$SHSZ300, order.by = date1)
eur <- xts(data1$EUR, order.by = date1)
dxy <- xts(data1$DXY, order.by = date1)
gbp <- xts(data1$GBP, order.by = date1)
m2  <- xts(data1$M2 , order.by = date1)
jpy <- xts(data1$JPY, order.by = date1)
cny <- xts(data1$CNY, order.by = date1)
tbill  <- xts(data1$USGB090D, order.by = date1)
gerGB <- xts(data1$GTDEM3MO, order.by = date1)




spx.index <- xts(data1$SPXpoints, order.by = date1)
ukx.index <- xts(data1$UKXpoints, order.by = date1)
dax.index <- xts(data1$DAXpoints, order.by = date1)
shsz300.index <- xts(data1$SHSZ300points, order.by = date1)
gold.price <- xts(data1$XAUprice, order.by = date1)
eur.price <- xts(data1$EURprice, order.by = date1)
ukx.index <- xts(data1$UKXpoints, order.by = date1)
dxy.index <- xts(data1$DXYprice, order.by = date1)
gbp.price <- xts(data1$GBPprice, order.by = date1)
jpy.price <- xts(data1$JPYprice, order.by = date1)
cny.price <- xts(data1$CNYprice, order.by = date1)



train.xau <- window(xau, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.xau <- window(xau, start = as.Date("2016-01-31"), end = as.Date("2016-12-31"))
train.spx <- window(spx, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.spx <- window(spx, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.ukx <- window(ukx, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.ukx <- window(ukx, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.dax <- window(dax, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.dax <- window(dax, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.shsz300 <- window(shsz300, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.shsz300 <- window(shsz300, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.eur <- window(eur, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.eur <- window(eur, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.dxy <- window(dxy, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.dxy <- window(dxy, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.gbp <- window(gbp, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.gbp <- window(gbp, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.m2 <- window(m2, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.m2 <- window(m2, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.jpy <- window(jpy, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.jpy <- window(jpy, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.cny <- window(cny, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.cny <- window(cny, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )



train.xau.price <- window(gold.price, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.xau.price <- window(gold.price, start = as.Date("2016-01-31"), end = as.Date("2016-12-31"))
train.spx.index <- window(spx.index, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.spx.index <- window(spx.index, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.ukx.index <- window(ukx.index, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.ukx.index <- window(ukx.index, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.dax.index<- window(dax.index, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.dax.index <- window(dax.index, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.shsz300.index <- window(shsz300.index, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.shsz300.index <- window(shsz300.index, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.eur.price <- window(eur.price, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.eur.price <- window(eur.price, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.dxy.index <- window(dxy.index, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.dxy.index <- window(dxy.index, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.gbp.price <- window(gbp.price, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.gbp.price <- window(gbp.price, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.jpy.price <- window(jpy.price, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.jpy.price <- window(jpy.price, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
train.cny.price <- window(cny.price, start = as.Date("2006-01-31"), end = as.Date("2015-12-31") )
valid.cny.price <- window(cny.price, start = as.Date("2016-01-31"), end = as.Date("2016-12-31") )
##### 1 week lagged data ------

validation.week1 <- read.csv("weeklylaggedvalidation.csv")
train.week1 <- read.csv("weeklylaggedtraining.csv")
#validation.week4 <- read.csv("4weekslaggedvalidation.csv")
#train.week4 <- read.csv("4weekslaggedtraining.csv")
train.date <- train.week1$Date
valid.date <- validation.week1$Date

norma.training <- scale(read.csv("normalizedtraining.csv")[-1])
norma.validation <- scale(read.csv("normalizedvalidation.csv")[-1])
normal.gold <- ts(norma.training$xau, start = c(2006,01))
  
  xts(norma.training$xau, order.by = as.Date(norma.training$Date))


regression.week1 <- lm( xau ~  shsz300 +	eur+	dxy +	gbp +cny + spx + ukx, data = train.week1)


regression.week4 <- lm( xau ~  shsz300 +	eur+	dxy +cny + spx + gbp + ukx
 , data = train.week4)


#####------


regression <- regression.week1

summary(regression)

fitted.values <- xts(regression$fitted.values, order.by = as.Date(train.week1$Date))
fr <- xts(forecast(regression, newdata = validation.week1, h=31 )$mean, order.by = as.Date(validation.week1$Date))
fr2<- forecast(regression, newdata = validation.week1, h=31 )
as.matrix(accuracy(fr2, validation$xau))

plot.xts(xau[400:length(xau)], minor.ticks = TRUE, major.ticks = TRUE, main = "Gold Spot Price One week ahead forecast, h= 31")
legend("bottomleft", c("Gold Spot Price", "Forecast", "Fitted Model"),
       lty = c(1,1,2), col = c("black", "blue", "blue") )
lines(exp(fr), col="blue", lwd = 2)
lines(exp(fitted.values), col= "blue", lty= "dashed")



stargazer(regression, type = "text", summary = F, style = "all2", title = "Regression Model Results", align = TRUE )
#####-----

nn <- neuralnet(xau ~  spx + ukx + dax + shsz300 + m2 + dxy + eur + gbp + jpy + cny  , 
                data =  train ,hidden= c(7,4) , linear.output=T, stepmax = 100000000, rep = 100 )
predicted.nn.values <- compute(nn, norma.validation[,2:11])$net.result

fitted.net <- (nn$net.result* ) )+ mean(exp(train$xau))
ff <- xts((predicted.nn.values* sd(exp(train$xau)) )+ mean(exp(train$xau)), order.by= as.Date(validation$Date))
