

panjer.poisson<-function(lam,ff,smax)
{
  aa<-0
  bb<-lam
  ll<-length(ff)
  ffs<-exp(lam*(ff[1]-1))
  ff<-c(ff,rep(0,smax-ll+1))
  for (i in 1 :smax)
  {
    j<-i+1
    ffs<-c(ffs,(1/(1-aa*ff[1]))*sum(ff[2 :j]*ffs[i :1]*(bb*(1 :i)/i+aa)))
  }
  return(ffs)
}

#####
nfft <-2^20

f1 <- c(dnbinom(0:100,2, 1/2), rep(0, nfft - 101))
f2 <- c(dnbinom(0:100,1, 1/3), rep(0, nfft - 101))
sum(f1)
sum(f2)
ff1 <- fft(f1)
ff2<- fft(f2)
fs<-Re(fft(ff1*ff2, inverse = T)/nfft)
sum(fs)
vs<-0:(nfft-1)
es<-sum(fs*vs)
es
fs[c(1,2,3)]



###########


nfft <- 20000*100
alpha1<-2
alpha2<-1
lambda1<-0.003
lambda2<-0.004


fb <- pgamma(1:20000, 2, 1/1000) - pgamma(0:19999, 2, 1/1000)
sum(fb)

fb2 <- pgamma(1:20000, 1, 1/1000) - pgamma(0:19999, 1, 1/1000)
sum(fb2)

fb <- c(fb, rep(0, nfft - length(fb)))
ffb <- fft(fb)


fb2 <- c(fb2, rep(0, nfft - length(fb2)))
ffb2 <- fft(fb2)

pm <- exp(lambda1*(ffb- 1))
fx <- Re(fft(pm, inverse = T)/nfft)
sum(fx)

pm2 <- exp(lambda2*(ffb2- 1))
fx2 <- Re(fft(pm2, inverse = T)/nfft)
sum(fx2)


Fx <- cumsum(fx)

fx1<- panjer.poisson(lambda1, pgamma(1:20000, 2, 1/1000) - pgamma(0:19999, 2, 1/1000), 50000)
Fx1 <- cumsum(fx1)

vx <- 0:(nfft -1)
vx1 <- 0:(length(fx1)-1)

VaR <- min(which(Fx >= 0.995) + 1)
VaR1 <- min(which(Fx1 >=0.995) + 1)

stopLoss <- sum(pmax(vx - VaR, 0) * fx)/(1-0.995) + VaR

stopLoss2 <- sum(pmax(vx1 - VaR1, 0) * fx1)/(1-0.995) +VaR1

c(stopLoss, stopLoss2)


pms <- exp(500*(lambda1- 0.001)*(ffb - 1))*exp(500*(lambda2- 0.001)*(ffb2 - 1))*
  exp(0.001*((ffb^500)*(ffb2^500) - 1))

fs <- Re(fft(pms, inverse = T)/nfft)
sum(fs)
Fs <- cumsum(fs)
min(which(Fs >= 0.999) + 1)
vs <- 0:(nfft-1)
sum(vs*fs)


ffsx1 <- Re(fft(fft(fx2)^500 * fft(fx)^500, inverse = T)/nfft)
sum(ffsx1)
sum(vs*ffsx1)


#################
library("actuar")
u <- 0:10000/10000

x1 <- qpareto(u, 1.5, 50)
x2 <- qexp(u, 1/100)
x3 <- qlnorm(u, log(100)-0.5, 1)

f <- function(x) abs(qpareto(x, 1.5, 50)+ qexp(x, 1/100) + qlnorm(x, log(100)-0.5, 1) - 2000)
optimize(f, c(0,1))$minimum

s <- x1+x2+x3
matplot(u,s)


f <- function(x) abs(qpareto(x, 1.5, 50)+ qexp(1 - x, 1/100)  - 2000)
optimize(f, c(0,1))$minimum
















