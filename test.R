################################################################################
# Tryings to PCA
################################################################################

ger = c(2,4,1,3,2,4,4,1,2,3)
phi = c(3,4,1,2,2,3,3,2,2,2)
mat = c(1,3,2,4,1,2,2,4,3,1)
phy = c(2,2,2,5,2,2,3,4,3,3)
A = cbind(deu,phil,ma,phy)

As = scale(A)

As

R = (1/length(A[,1]))*t(As)%*%As
R

eR = eigen(R)

a1 = sqrt(eR$values[1])*eR$vectors[,1]
a2 = sqrt(eR$values[2])*eR$vectors[,2]
a3 = sqrt(eR$values[3])*eR$vectors[,3]
a4 = sqrt(eR$values[4])*eR$vectors[,4]

Ar = cbind(a1,a2,a3,a4)
RK = Ar %*% t(Ar)

summary(princomp(As), loadings = TRUE)

plot(as.data.frame(A))
pairs(as.data.frame(A),
      lower.panel = panel.smooth,
      upper.panel = panel.cor,
      diag.panel = panel.hist)


################################################################################
# Helper
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
