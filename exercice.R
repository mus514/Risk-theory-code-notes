
## q1, ... qn bernoulli
set.seed(2021)
nfft <- 2^14
vs <- 0:(nfft - 1)
q <- runif(10000)

ex <- sum(q)
ffxs <- rep(1, nfft)

for(i in 1:10000)
{
  ffxs <- fft(c(1-q[i], q[i], rep(0, nfft - 2))) * ffxs
}

fs <- Re(fft(ffxs, inverse = TRUE))/nfft
c(sum(fs*vs), ex)


## Execice Tradi 3

r <- 0.2
q <- 1/1.2

exs <- (r*(1-q)/q) * (1/0.4) *100
nfft <- 2^20
fb <- c(0, 0.4*(0.6)^(0:98))
fb <- c(fb , rep(0, 100000))
ffb <- fft(fb)
ftx <- (q/(1-(1-q)*ffb))^r
fx <- Re(fft(ftx, inverse = T)/length(fb))

fx <- c(fx, rep(0, nfft - length(fx)))
fxt <- fft(fx)
fs <- Re(fft(fxt^100, inverse = T)/nfft)
c(exs, sum(0:(nfft-1)*fs))

## Execice Tradi 6

nfft <- 2^21
r <- 0.4
q <- 2/3
fc <- function(x) 1 - exp(-(x/1000)^(0.8))

x <- seq(0, 4000, 1000)
FC <- fc(x + 1000) - fc(x)
FC[5] <- FC[5]+(1-fc(5000))
FC <- c(0, FC, rep(0, nfft - length(FC) -1))
ffc <- fft(FC)
pm <- (q/(1-(1-q)*ffc))^r

fx <- Re(fft(pm, inverse = TRUE)/(length(FC)))
FX <- cumsum(fx)
min(which(FX >= 0.99)-1) * 5000

## Execice Tradi 7

nfft <- 2^22

fb <- c(0, 0.4*(0.6)^(0:100))
fb <- c(fb, rep(0, nfft - length(fb)-1))
fbt <- fft(fb)

ftm <- 0.05*exp(fbt - 1) + 0.95
fx <- Re(fft(ftm, inverse = T)/nfft)
sum(fx)
fx[1:4]


## Execice Tradi 7

v <- 0.95
r <- 2
q <- 1/2
x1 <- seq(0, 1000000, 10) 

nfft <- 2^20
fb1 <- function(x) 1 - (3800/(x + 3800))^3
fb2 <- function(x) 1 - (3610/(x + 3610))^3
FB1 <- fb1(x1 + 10) - fb1(x1)
FB2 <- fb2(x1 + 10) - fb2(x1)

FB1 <- c(0, FB1)
FB2 <- c(0, FB2)
sum(FB1)
sum(FB2)
exs <- 4000*(v + v^2)

ffb1 <- fft(FB1)
pm1 <- (q/(1-(1-q)*ffb1))^2
fw1 <- Re(fft(pm1, inverse = T)/length(FB1))

ffb2 <- fft(FB2)
pm2 <- (q/(1-(1-q)*ffb2))^2
fw2 <- Re(fft(pm2, inverse = T)/length(FB2))

fw1 <- c(fw1, rep(0, nfft - length(fw1)))
fw2 <- c(fw2, rep(0, nfft - length(fw2)))

ftz <- fft(fw1)*fft(fw2)
fz <- Re(fft(ftz, inverse = T)/nfft)
sum(fz)
vz <- 0:(nfft-1)
c(exs, sum(fz*vz)*100)

FZ <- cumsum(fz)
min(which(FZ >= 0.99) - 1)*10



## Execice Tradi 11

nfft <- 2^20
fb<- c(0, 5/15, 4/15, 3/15, 2/15, 1/15)
lambda <- 0.15

fb <- c(fb, rep(0, nfft - length(fb)))
ftb <- fft(fb)
pm <- exp(lambda*(ftb -1))
fs <- Re(fft(pm, inverse = T)/nfft)

vs <- 0:(nfft-1)
sum(vs*fs)*1000

## Execice Info 1

nfft <- 60
fb <- c(0.5, 0.1,0.35, 0.05)
fb <- c(fb , rep(0, nfft -length(fb)))
ftb <- fft(fb)
ftb <- ftb^10
fs <- Re(fft(ftb, inverse = T)/nfft)
fs[c(1, 2, 11, 21)]

vs <- 0:(nfft-1)
sum(vs*fs)

## Execice Info 3

nfft <- 2^23
x1 <- seq(0, 50000, 0.01)
f <- function(x) 1- (5/(5+x))^1.5

fxl <- c(0, f(x1+0.01)-f(x1))
sum(fxl)

fxl <- c(fxl, rep(0, nfft - length(fxl)))
ffx <- (fft(fxl))^2
fs <- Re(fft(ffx, inverse = T)/nfft)
sum(fs)
FS <- cumsum(fs)
sapply(c(0.9, 0.99, 0.999,0.9999), function(x) min(which(FS>=x)-1)) * 0.01

## Execice Info 4

r <- 1.5
q <- 1/3
mu <- log(10)-0.36/2
sig <- 0.6
x1 <- 0:10000

fx <- c(0, plnorm(x1+1, mu, sig) - plnorm(x1, mu, sig))
sum(fx)

ffx<- fft(fx)

pm <- (q/(1-(1-q)*ffx))^r
fs <- Re(fft(pm, inverse = T)/length(fpm))
sum(fs)
FS <- cumsum(fs)
FS[51]
vx <- 0:(length(fs)-1)

sum(pmax(vx - 50, 0) * fs)


## Execice Info 7

r <- 0.01
q <- 1/4
nfft <- 2^20
x1 <- seq(0, 39999000, 1000)

f <- function(x) 1 - (15000/(15000 + x))^2.5
fb <- c(0, f(x1+1000)-f(x1))
ffb <- fft(fb)
pm <- (q/(1-(1-q)*ffb))^r
fx <-Re(fft(pm, inverse = T)/length(ffb))
vx <- 0:(length(fx)-1) * 1000
exx <- sum(vx*fx)
c(300, exx)

fx <- c(fx, rep(0, nfft-length(fx)))

ffx <- (fft(fx))^1000

fs <- Re(fft(ffx, inverse = T)/nfft)
sum(fs)

vs <- 0:(nfft-1)*1000
sum(fs*vs)
sum(pmax(vs-2000000, 0) * fs)

























  
