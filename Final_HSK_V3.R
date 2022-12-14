a <- read.csv("D:/IVA/IVA503 Nihat/Final/data_final.csv")
View(a)
summary(a)

#Hale'nin verisi y7:
h <- a[, c(1, 8)]
View(h)
summary(h) #NA veri yok

h1 <- h[1:1450,]
h2 <- h[1451:1500,]
#grafik
par(mfrow=c(1,2))
plot(h)
plot(h1)
plot(h2)

y7.ts <- ts(h$y7)
plot.ts(y7.ts)

#Zaman serisine ?evirelim
par(mfrow=c(1,1))
h1.ts <- ts(h1$y7)
plot.ts(h1.ts)
#ry7 <- 100*diff(h1.ts, lag=1, diff=1)/lag(h1.ts, -1) #getirisi
#plot.ts(ry7)

#Correlogram
par(mfrow=c(1,2))
acf(h1$y7, lag.max=30) #geometrik azal?yor. ge?mi?i h?zla unutuyor. ge?mi? ?oklar etkisin kaybediyor. dura?an.
acf(h1$y7, lag.max=30, plot=F)#otokorelasyon fonk. de?erlerini g?relim
pacf(h1$y7, lag.max=30)#kismi otokorelason fonk. 
pacf(h1$y7, lag.max=30, plot=F)#kismi otokorelason fonk. de?erlerini g?relim

#buraya ADF test ekleyip dura?anl???n? onayla!
library(aTSA)
adf.test(h1$y7, output=T) #birim k?k yok, dura?an.

#Ljung-Box Testi (Q-Stat)
Box.test(h1$y7, lag=1, "Ljung-Box") #zaman serisi yap?s? var istatistiki olarak anlaml? p-value<2.2e-16
#20 lag i?in de?erlere bakarsam
LB.test <- lapply(1:30, function(i) Box.test(h1$y7, type="Ljung-Box", i))
LB.restults <- data.frame(
  lag=1:30,
  xsquared=sapply(LB.test, "[[", "statistic"),
  pvalue=sapply(LB.test, "[[", "p.value")
)
head(LB.restults, 30)
# s?f?r hipotezini reddet. ilk 30 korelasyon katsay?lar? 0'a e?it. 

#Estimate: ARMA(3,0)
arma30 <- arima(h1$y7, order=c(3,0,0))
arma30

#Diagnostic check
#1. Overfit
arma_40 <- arima(h1$y7, order=c(4,0,0))
arma_40 #Ho: ar4=0 ?
t = -0.0475/0.0263 #t= -1.806 > -1.96 , alfa=0.05 anlaml?l?k d?zeyine g?re non-rejection alan?nda kald?m yani istatistiki olarak ar4=0, ar(4) olamaz.
#modelim ar(3) ge?erlili?ini koruyor.
arma_31 <- arima(h1$y7, order=c(3,0,1))
arma_31 
t= 0.45/0.185 # t= 2.43>1.96, rejection regionda, istatistiki olarak ma(1) 0 dan farkl?
#istatistiki olarak anlaml? fakat ar2 ve ar3 anlams?z
#arma(1,1)'e g?re tekrar overfit edece?im.

arma_11 <- arima(h1$y7, order=c(1,0,1))
arma_11  #ar1 ve ma1 istatistiki olarak anlaml?

arma_12 <- arima(h1$y7, order=c(1,0,2))
arma_12 #Ho : ma2=0 ? ma2 istatistiki olarak 0 a e?it. 
arma_21 <- arima(h1$y7, order=c(2,0,1))
arma_21 #Ho : ar2=0 ? ar2 istatistiki olarak 0 a e?it.
#arma(1,1) modeli ge?erlili?ini koruyor.

# 2. Art?klarda bilgi var m??
arma <- arima(h1$y7, order=c(1,0,1))
arma

res <- residuals(arma)
plot.ts(res)
par(mfrow=c(1,2))
acf(res, lag.max=30)
pacf(res, lag.max=30)

Box.test(res, lag=1, "Ljung-Box") #zaman serisi yapisi yok diyor, 

LB.test <- lapply(1:20, function(i) Box.test(res, type="Ljung-Box", i))
LB.restults <- data.frame(
  lag=1:20,
  xsquared=sapply(LB.test, "[[", "statistic"),
  pvalue=sapply(LB.test, "[[", "p.value")
)
head(LB.restults, 20)
#Art?klar?m?n i?inde zamandan kaynaklanan bir bilgi yok. O zaman modelim ARMA(1,1)
#ARMA(1,1) modelimi tahminlerde kullanabilirim.

#oto identification ile ARMA(1,1) AIC de?eri en k???k m? kontrol edelim.
library(aTSA)
identify(h1$y7, p=6, q=6, nlag=6 , intercept = T, stat.test = F, output = T)
#ARMA (1,1) 'in do?ru model oldu?unu onaylam?? oldum. 
#fakat correlogram ile ??kan model ?rt??m?yor. i?imiz rahat etmedi structural break var m? bakal?m

#Chow Test for structural break
library(strucchange)
model1=Fstats(h1$y7~1)
plot(model1)
sctest(model1) #yap?sal k?r?lma var!
strucchange::breakpoints(h1$y7~1)

#breakpoints graph
bp_ts <- breakpoints(h1$y7~1, breaks=2)
m1 <- lm(h1$y7~1)
coef(m1)
plot(fitted(m1), type="l")
m1.fac <- breakfactor(bp_ts)
m2 <- lm(h1$y7~m1.fac-1)
coef(m2)
plot(fitted(m2))

#Bu durumda ARMA modelimi son breakpoint i?in yenilemek istiyorum.
#son breakpoint : 1228, 1230 den sonraki veriyi kullanaca??m
h3 <- h1[1250:1450,]
View(h3)
#Correlogram
par(mfrow=c(1,2))
acf(h3$y7, lag.max=20) #geometrik azal?yor. ge?mi?i h?zla unutuyor. ge?mi? ?oklar etkisin kaybediyor. dura?an.
pacf(h3$y7, lag.max=20)#kismi otokorelason fonk. 


#buraya ADF test ekleyip dura?anl???n? onayla!
adf.test(h3$y7, output=T) #birim k?k yok, dura?an.

#Ljung-Box Testi (Q-Stat)
Box.test(h3$y7, lag=1, "Ljung-Box") #zaman serisi yap?s? var istatistiki olarak anlaml? p-value<2.2e-16
#20 lag i?in de?erlere bakarsam
LB.test <- lapply(1:20, function(i) Box.test(h3$y7, type="Ljung-Box", i))
LB.restults <- data.frame(
  lag=1:20,
  xsquared=sapply(LB.test, "[[", "statistic"),
  pvalue=sapply(LB.test, "[[", "p.value")
)
head(LB.restults, 20)
# s?f?r hipotezini reddet. ilk 20 korelasyon katsay?lar? 0'a e?it. 

#Estimate: ARMA(3,0)
arma2 <- arima(h3$y7, order=c(3,0,0))
arma2

#Diagnostic check
#1. Overfit
arma_40 <- arima(h3$y7, order=c(4,0,0))
arma_40 #Ho: ar4=0 ?
t = 0.0001/0.0671 #t= 0.0014 < 1.96 , alfa=0.05 anlaml?l?k d?zeyine g?re non-rejection alan?nda kald?m yani istatistiki olarak ar4=0, ar(4) olamaz.
#modelim ar(3) ge?erlili?ini koruyor.
arma_31 <- arima(h3$y7, order=c(3,0,1))
arma_31 
t= -0.0989/0.3281 # t= -0.30>-1.96, non-rejection regionda, istatistiki olarak ma(1) 0 dan farks?z, anlams?z

# 2. Art?klarda bilgi var m??
arma2 <- arima(h3$y7, order=c(3,0,0))
arma2

res <- residuals(arma2)
plot.ts(res)
par(mfrow=c(1,2))
acf(res, lag.max=30)
pacf(res, lag.max=30)

Box.test(res, lag=1, "Ljung-Box") #zaman serisi yapisi yok diyor, 

LB.test <- lapply(1:20, function(i) Box.test(res, type="Ljung-Box", i))
LB.restults <- data.frame(
  lag=1:20,
  xsquared=sapply(LB.test, "[[", "statistic"),
  pvalue=sapply(LB.test, "[[", "p.value")
)
head(LB.restults, 20)
#Art?klar?m?n i?inde zamandan kaynaklanan bir bilgi yok. O zaman modelim ARMA(3,0)
#Yine de iki model i?in de tahmin deneyece?im.
arma
arma2

y7_pred <- predict(arma, n.ahead = 50)
y7_pred
h2_new <- cbind(h2, y7_pred)
View(h2_new)

#fark fonsiyonu yt-yt+1
d_y7 <- function(v){
  b <- c()
  i <- 1
  while (i < length(v)){
    b[i]<- v[i+1]-v[i]
    i<- i+1
  }
  return(b)  
}

delta_actual1 <- d_y7(h2_new$y7)
data.frame(delta_actual1)

delta_pred1 <- d_y7(h2_new$pred)
data.frame(delta_pred1)

dummyfortest <- function(v,k){
  d<- c()
  i <- 1
  while (i < length(v)+1){
    if((v[i]<0&&k[i]<0)||(v[i]>0&&k[i]>0)){
      d[i] = 1 
      i<- i+1
    }else{
      d[i] = 0
      i<- i+1
    }
  }
  return(d)  
}

dummy_1 <- dummyfortest(delta_actual1,delta_pred1)
data.frame(dummy_1)

result11 <-cbind(delta_actual1, delta_pred1, dummy_1)
View(result11)
colSums(result11)
prob11= 23/49 


MSE <- mean((h2_new$y7 - h2_new$pred)^2)
RMSE <- MSE^0.5
#MSPE <- mean((h2_new$pred / h2_new$y7)^2)
#MAPE <- mean(abs(h2_new$y7 - h2_new$pred)/abs(h2_new$y7)) * 100

y7_pred2 <- predict(arma2, n.ahead = 50)
y7_pred2
h2_new2 <- cbind(h2, y7_pred2)
View(h2_new2)
MSE2 <- mean((h2_new2$y7 - h2_new2$pred)^2)
RMSE2 <- MSE2^0.5

delta_actual2 <- d_y7(h2_new2$y7)
data.frame(delta_actual2)

delta_pred2 <- d_y7(h2_new2$pred)
data.frame(delta_pred2)

dummy_2 <- dummyfortest(delta_actual2,delta_pred2)
data.frame(dummy_2)

result30 <-cbind(delta_actual2, delta_pred2, dummy_2)
View(result30)
colSums(result30)
prob30 = 23/49 

