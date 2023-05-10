## Question 1

repErlangGeneraliser <- function(x) 1 - exp(-x/10) - 2*(exp(-x/20) - exp(-x/10))
kappa <- 0.99
f <- function(x) abs(repErlangGeneraliser(x) - kappa)
repErlangGeneraliser(100)
repErlangGeneraliser(1000)
VaR <- optimize(f, c(100, 1000))
c(VaR$minimum, qgamma(0.99, 2, 1/10))
c(kappa, repErlangGeneraliser(Var$minimum))

## Question 2
b1 <- 1/2
b2 <- 1/3
b3 <- 1/5

c1 <- (b2/(b2-b1))*(b3/(b3-b1))
c2 <- (b1/(b1-b2))*(b3/(b3-b2))
c3 <- (b2/(b2-b3))*(b1/(b1-b3))

repErlangGeneral <- function(x)  {c1*(1-exp(-x*b1)) + c2*(1-exp(-x*b2)) + c3*(1-exp(-x*b3))}

repErlangGeneral(40)
kappa <- 0.999
f <- function(x) abs(repErlangGeneral(x) - kappa)

c(kappa, repErlangGeneral(VaR$minimum))

VaR_1 <- qgamma(kappa, 3, 1/2)
VaR_2 <- optimize(f, c(40, 100))
VaR_3 <- qgamma(kappa, 3, 1/5)

c(VaR_1, VaR_2$minimum, VaR_3)

## Question 4

n1 <- 1
n2 <- 10
n3 <- 100
n4 <- 1000

a <- 0.5
b <- a/10

xx <- 0:400/10

FX1 <- pgamma(xx, n1*a, n1*b)
FX2 <- pgamma(xx, n2*a, n2*b)
FX3 <- pgamma(xx, n3*a, n3*b)
FX4 <- pgamma(xx, n4*a, n4*b)

matplot(xx, cbind(FX1, FX2, FX3, FX4))

kappa <- 0:10/10

VaR1 <- qgamma(kappa, n1*a, n1*b)
VaR2 <- qgamma(kappa, n2*a, n2*b)
VaR3 <- qgamma(kappa, n3*a, n3*b)
VaR4 <- qgamma(kappa, n4*a, n4*b)


TVaR1 <- 1/(1-kappa)*(a*n1)/(b*n1)*pgamma(VaR1, a*n1 + 1, b*n1)
TVaR2 <- 1/(1-kappa)*(a*n1)/(b*n1)*pgamma(VaR2, a*n2 + 1, b*n2)
TVaR3 <- 1/(1-kappa)*(a*n1)/(b*n1)*pgamma(VaR3, a*n3 + 1, b*n3)
TVaR4 <- 1/(1-kappa)*(a*n1)/(b*n1)*pgamma(VaR4, a*n4 + 1, b*n4)

cbind(kappa, VaR1, VaR2, VaR3, VaR4)

## Question 5

a = 2
n = 10
q = 2/10

kappa <- c(0.01, 0.3 ,0.5, 0.99)

VaR1 <-qbinom(kappa, n, q)
VaR2 <- qpois(kappa, a)
cbind(kappa, VaR1, VaR2)





