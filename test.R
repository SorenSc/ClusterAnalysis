################################################################################
# dnorm, pnorm, qnorm, rnorm
################################################################################
# The following code is based on 
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_dist.html
library(ggfortify)

d = ggdistribution(dnorm, seq(-3, 3, 0.1), mean = 0, sd = 1)
d + ggtitle('Dnorm, Dichtefunktion')

p = ggdistribution(pnorm, seq(-3, 3, 0.1), mean = 0, sd = 1)
p + ggtitle('Pnorm, Verteilungsfunktion')

main = 'Qnorm, Quantilfunktion'
plot(qnorm(seq(0,1,0.01)), main = main)

autoplot(density(rnorm(1:50)), fill = 'red')


################################################################################
# Tryings to PCA
################################################################################

ger = c(2,4,1,3,2,4,4,1,2,3)
phi = c(3,4,1,2,2,3,3,2,2,2)
mat = c(1,3,2,4,1,2,2,4,3,1)
phy = c(2,2,2,5,2,2,3,4,3,3)
A = cbind(ger,phi,mat,phy)

As = scale(A)

As

R = (1/length(A[,1]))*t(As)%*%As
R

eR = eigen(R)
eR

eigen(A)

a1 = sqrt(eR$values[1])*eR$vectors[,1]
a2 = sqrt(eR$values[2])*eR$vectors[,2]
a3 = sqrt(eR$values[3])*eR$vectors[,3]
a4 = sqrt(eR$values[4])*eR$vectors[,4]

Ar = cbind(a1,a2,a3,a4)
RK = Ar %*% t(Ar)

summary(princomp(A), loadings = TRUE)

?princomp

plot(as.data.frame(A))
pairs(as.data.frame(A),
      lower.panel = panel.smooth,
      upper.panel = panel.cor,
      diag.panel = panel.hist)


################################################################################
# Matrix multiplication
################################################################################

A = matrix(seq(1,9), nrow = 3)
A%*%t(A)
t(A)%*%A
solve(A)



c=rbind(c(1, -1/4), c(-1/4, 1))  
solve(c)
