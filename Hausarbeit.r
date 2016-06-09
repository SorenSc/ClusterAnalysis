# -----------------------------------------------------------------------------
# -------- Hausarbeit Datenanalyse und Datenmanagement ------------------------
# -----------------------------------------------------------------------------


# -------- Simlation der Datenpunkte ------------------------------------------

library(mvtnorm)

# Mittelwertsvektor und Kovarianzmatrix für den ersten Cluster festlegen
mu1 <- c(0,0)
S1 <- matrix(c(1,0,0,1),2,2)

# Daten generieren
help(rmvnorm)
n1 <- 100
X1 <- rmvnorm(n1, mean = mu1, sigma = S1)

# zeichnen
plot(X1)
  
# zweiter Cluster
mu2 <- c(4,-2)
S2 <- matrix(c(2,0.5,0.5,2),2,2)
n2 <- 150
X2 <- rmvnorm(n2, mean = mu2, sigma = S2)

# zusammen in einen Datensatz und eine Grafik
X <- rbind(X1,X2)
group <- rep(1:2,c(n1,n2))
plot(X, col=group)

# ggf. dritten Cluster usw.
# ...



