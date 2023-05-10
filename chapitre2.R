### Traditionnel

##2
#c)
Fcomp<- function(x, t)
{
   res <- dpois(0, t) + sum(sapply(1:100, function(y) dpois(y, t) * pgamma(x, 4*y, 0.2)))
   res
}
1- Fcomp(300, 2.7)
1-Fcomp(600, 2.7)
1-Fcomp(0, 2.7)
1 -Fcomp(1000, 2.7)

VaR <- function(kappa)
{
    VaR <- function(y) abs(FScomp(y) - kappa)
    optimize(VaR, c(0,1000))$minimum
}

TVaR <- function(kappa)
{
    res <- 1/(1-kappa)*sum(sapply(1:100, function(x) dpois(x, 2.7)*(20*x)*(1 - pgamma(VaR(kappa), 4*x + 1, 0.2))))
    res
    
}
Fcomp(300, 5.4)

##4
#a)

vrai <- function(x) abs(-12.7015148 + 30*((1/x) -((15*exp(-x*15))/(1- exp(-15*x)))))

optimize(vrai, c(0,10))$minimum

    
## 7
#b)
lambda <- function(t) 0.5*t + 0.025*t^2
curve(lambda,from = 0, to = 10)
    
## 8
# c)
lambda <- function(t) 1*0.5*(1*t)^-0.5
curve(lambda, from = 0, to =0.004)

###################################

### Informatique
##1)

t <- 0:33
y <- c(2,1,2,1,3,3,1,1,5,8,5,9,8,8,3,5,5,6,7,5,9,4,4,4,7,8,10,11,14,10,9,7,5,14)

poiLin <- function(p)
{
    a <- p[1]
    b <- p[2]
    -sum(-(a + b*(t + 0.5)) + y*log(a + b*(t + 0.5)))
}

optim(c(0, 2), poiLin)


poiPui <- function(p)
{
    c <- p[1]
    d <- p[2]
    -sum(-c*(t+1)^d + c*t^d + y*log(c*(t+1)^d - c*t^d))
}

a <- optim(c(0.01, 2), poiLin)$par[1]
b <- optim(c(0.01, 2), poiLin)$par[2]

f <- function(x)
{
    a*x + 0.5*b*x^2
}

c <- optim(c(0.01, 2), poiPui)$par[1]
d <- optim(c(0.01, 2), poiPui)$par[2]

g <- function(x)
{
    c*x^d
}


curve(f, from = 0, to = 36, col = '1', add = T)
curve(g, from = 0, to = 36, col = '2', add = T)

f(35) - f(34)
f(36) - f(35)
f(37) - f(36)
f(38) - f(37)
f(39) - f(38)

##2)
set.seed(2018)
w1 <- rexp(100, 1)
w2 <- rexp(100, 1)
w3 <- rexp(100, 1)
w4 <- rexp(100, 1)
w5 <- rexp(100, 1)

t1 <- cumsum(w1)
t1[t1 <= 10]
t2 <- cumsum(w2)
t2[t2 <= 10]
t3 <- cumsum(w3)
t3[t3 <= 10]
t4 <- cumsum(w4)
t4[t4 <= 10]
t5 <- cumsum(w5)
t5[t5 <= 10]

z <- 1:1e6
for(i in 1:1e6)
{
    w <- rexp(100, 1)
    t <- cumsum(w)
    z[i] <- sum(rlnorm(100, log(10) -0.5, 1)*exp(-0.03*t)*(t <= 10))
}

z <- sort(z)
z[0.99*1e6]

############################
##3)
l <- list()
m <- 5
for(i in 1:m)
{
    n <- rpois(1, 10)
    if(n > 0)
    {
        u <- runif(n)
        l[[i]] <- sort(u) * 10
    }
    else
    {
        l[[i]] <- 0
    }
 
}

Z <- list()
m <- 5
for(i in 1:m)
{
    n <- rpois(1, 10)
    if(n > 0)
    {
        u <- runif(n)
        Z[[i]] <- sum(exp(-(sort(u) * 10)*0.03) * rlnorm(n, log(10)-0.5, 1))
    }
    else
    {
        Z[[i]] <- 0
    }
}

Z <- 1:1e6
m <- Z
set.seed(2018)
for(i in m)
{
    n <- rpois(1, 10)
    if(n > 0)
    {
        u <- runif(n)
        Z[i] <- sum(exp(-(sort(u) * 10)*0.03) * rlnorm(n, log(10)-0.5, 1))
    }
    else
    {
        Z[i] <- 0
    }
    
}
Z <- sort(Z)
p <- rep(1/1e6, 1e6)
f <- cumsum(p)
min(Z[(f >= 0.99)])

################################
##4)

l <- list()
m <- 5
set.seed(2018)
for(i in 1:m)
{
    u <- runif(1)
    t <- vector()
    t[1] <- (-(0.5 + 0.05*0)+sqrt((0.5 + 0.05*0)^2 - 2*0.05*log(1 - u)))/0.05
    for(j in 2:100)
    {
        t[j] <- (-(0.5 + 0.05*t[j-1])+sqrt((0.5 + 0.05*t[j-1])^2 - 2*0.05*log(1 - u)))/0.05 + t[j-1]
    }
    
    
    l[[i]] <- cumsum(t)[cumsum(t) <= 10]
}    

qv <- function(u)
{
    (-0.5+sqrt(0.5^2 + 0.75*u))/0.05
}
for(i in 1:m)
{
    n <- rpois(1,7.5)
    print(n)
    u <- runif(n)
    l[[i]] <- sort(qv(u))
}

##########################
##5)
t <- 0:99
x <- rep(0,100)
x[c(1909-1908, 1916-1908, 1923-1908, 1934-1908, 1936-1908, 1937-1908, 1950-1908, 1954-1908, 1955-1908, 1961-1908, 1972-1908)] <- 1
x[c(1974-1908, 1983-1908, 1987-1908, 1993-1908, 1995-1908, 1997-1907, 1999-1908)] <- 1
x[c(1920 -1908, 1948-1908, 1996-1908, 2004-1908, 2005-1908)] <- 2

matplot(t, cbind(cumsum(rep(0.28 , 100)), cumsum(x)), type = "p", lty = 1)
plot(cumsum(x), t, type = "1")
line(cumsum(rep(0.28 , 100)), add = T, col = '2')

fn <- function(p)
{
    a <- p[1]
    b <- p[2]
    
    -sum(-(a + b*(t+0.5)) + x*log((a + b*(t+0.5))))
}

a <- optim(c(0, 2), fn)$par[1]
b <-optim(c(0, 2), fn)$par[2]

f <- function(y)
{
   a*y + b*(y^2)/2
}
matplot(t, cbind(f(t+1), cumsum(x)))

f(101) - f(100)
f(111) - f(110)

##########################
##6)

m <-  vector()
for(i in 1:1000)
{
    m[i] <- pgamma(1,i, 5)
}
sum(m)


for(i in 1:1000)
{
    m[i] <- pgamma(1,i*0.1, 0.5)
}
sum(m)

for(i in 1:1000)
{
    m[i] <- pgamma(1,i*5, 25)
}
sum(m)


x <- 0:100
p <- pgamma(1, 0:100*5, 25) - pgamma(1, (0:100+1) * 5, 25)


(mean <- sum(x*p))
(variance <- sum(p*x^2 ) - mean^2)
min(x[cumsum(p) >= 0.9])
min(x[cumsum(p) >= 0.99])

u <- c(0.65 , 0.24 , 0.98 , 0.76 , 0.34 , 0.92, 0.03, 0.07, 0.35, 0.51)
t <- cumsum(qgamma(u, 1, 5))
sum(t <=1)

##########################
##7)

m <- matrix(runif(6*100000), ncol = 6)
m <- 1/4*(-log(1-m))^(1/0.5)
m <- t(apply(m, 1, cumsum))
(probFt <- sapply(1:5, function(x) mean(m[,x] <= 1)))
x <- c(1, probFt)

sum((head(x) - f)*(0:5))
##########################
##8)
t <-c(0.3055285, 0.7095883, 0.7968152, 0.9986248, 1.411262, 1.690603, 2.21957, 2.368388, 
  3.567566 , 4.041499 , 4.39206 , 4.979485,  6.380634 , 6.98289 , 7.753156 , 8.309093 ,
  8.5659 , 9.045187 , 9.718129 , 10.5263 , 10.7752 , 11.26835 ,11.43449 , 11.55362 , 12.25831, 
  12.72269 , 13.02203 , 14.06437 , 14.58528 , 14.67648 , 14.83452)

y <- c(0, t)
w <- t - head(y, -1)
plot.ecdf(w)
curve(pexp(x, 2.08972), add = T, col = "2")

fn <- function(p)
{
    a <- p[1]
    b <- p[2]
    -sum(log(a*b) + (a-1)*(log(b) + log(w)) - (b*w)^a)
}

(a <- optim(c(0.01, 1), fn)$par[1])
(b <- optim(c(0.01, 1), fn)$par[2])

plot.ecdf(w)
curve(pweibull(x, a, 1/b), add = T, col = "2")
curve(pexp(x, 2.08972), add = T, col = "3")

##########################
##9)
x <- 0:6
r <- c(190, 681, 692, 345, 77, 13, 2)
y <- rep((0:6), r)

gn <- function(a)
{
    -sum(-a + y*log(a))
}

lambda <- optim(c(00.1,1), gn)$par[1]

mean(y)
plot(x, r, col = "2")
t <- dpois(x, mean(y))
points(x,mean(y) * r)

fn <- function(p)
{
   a <- p[1]
   b <- p[2]
   
   -sum(log(pgamma(1, a*y, b) - pgamma(1, (y+1)*a, b)))
}

a <- optim(c(1,1), fn)$par[1]
b <- optim(c(1,1), fn)$par[2]

(R <- 2*(fn(c(a, b)) - gn(lambda)))
qchisq(0.95,1)

##########################
##10)

tau <- 1.5
lambda <- 1.5
k <- 0:1000
p1 <- pgamma(1, k, 5) - pgamma(1, k+1, 5)
p2 <- pgamma(1, k*0.1, 0.5) - pgamma(1, (k+1)*0.1, 0.5)
p3 <- pgamma(1, k*5, 25) - pgamma(1, (k+1)*5, 25)
#E[N(1)]
sum(k*p1)
sum(k*p2)
sum(k*p3)

#E[S(1)]
sum(k*p1)*tau/lambda
sum(k*p2)*tau/lambda
sum(k*p3)*tau/lambda

#Var(N(1))
sum(p1*k^2) - (sum(k*p1))^2
sum(p2*k^2) - (sum(k*p2))^2
sum(p3*k^2) - (sum(k*p3))^2

#Var(S(1))

sum(p1*k^2) - (sum(k*p1))^2*(tau/lambda)^2 + sum(k*p1)*(tau/lambda^2)
sum(p2*k^2) - (sum(k*p2))^2*(tau/lambda)^2 + sum(k*p2)*(tau/lambda^2)
sum(p3*k^2) - (sum(k*p3))^2*(tau/lambda)^2 + sum(k*p3)*(tau/lambda^2)

sum(p1 * pgamma(10, 1.5*k, 1.5))
sum(p1 * pgamma(20, 1.5*k, 1.5))
sum(p2 * pgamma(10, 1.5*k, 1.5))
sum(p2 * pgamma(20, 1.5*k, 1.5))
sum(p3 * pgamma(10, 1.5*k, 1.5))
sum(p3 * pgamma(20, 1.5*k, 1.5))


fn <- function(x)
{
  abs(sum(p1 * pgamma(x, 1.5*k, 1.5)) - 0.99)
}

fn2 <- function(x)
{
    abs(sum(p2 * pgamma(x, 1.5*k, 1.5)) - 0.99)
}
fn3 <- function(x)
{
    abs(sum(p3 * pgamma(x, 1.5*k, 1.5)) - 0.99)
}
VaR1 <- optimize(fn, c(1, 40))$minimum
VaR2 <-optimize(fn2, c(1, 40))$minimum
VaR3<-optimize(fn3, c(1, 40))$minimum
 

(TVaR1 <-(1/(1-0.99))*sum(k*(1-pgamma(VaR1, 1.5*k + 1, 1.5))*p1))
(TVaR2 <-(1/(1-0.99))*sum(k*(1-pgamma(VaR2, 1.5*k + 1, 1.5))*p2))
(TVaR3 <-(1/(1-0.99))*sum(k*(1-pgamma(VaR3, 1.5*k +1 , 1.5))*p3))










