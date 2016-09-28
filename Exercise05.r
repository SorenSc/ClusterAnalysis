################################################################################
# Aufgabe 15
################################################################################

################################################################################
# Aufgabe 16
################################################################################

jet <- read.csv("~/Documents/TU Clausthal/Datenanalyse und Management/jet.txt", sep="")

head(jet)
summary(jet)

# Mark all planes which are able to land on a carrier with a triangle.
plot(jet, pch = as.numeric(jet$CAR))

# Extract part of data
small_jet = jet[, c("SPR", "RGF", "PLF", "SLF")]

# Scale the data
scale_small_jet = scale(small_jet)

# Clustering with k-means-algorithm
two_cl = kmeans(scale_small_jet, 2, nstart = 1000)
thr_cl = kmeans(scale_small_jet, 3, nstart = 1000)
fou_cl = kmeans(scale_small_jet, 4, nstart = 1000)

# Plot colorful clusters
plot(small_jet, col = two_cl$cluster)
plot(small_jet, col = thr_cl$cluster)
plot(small_jet, col = fou_cl$cluster)

# Calculate sum of squares within group
wss = rep(0,10)
for(i in 1:length(wss)){
  wss[i] = sum(kmeans(scale_small_jet, i, nstart = 1000)$withinss)
}

# Plot withinss
xlab = 'Number of groups'
ylab = 'Withinss of group'
plot(wss, xlab = xlab, ylab = ylab)


################################################################################
# Calculation of the within sum of squares in general
B = matrix(c(29,5,0,33,5,0,10,22,3,7),ncol=2)
Bd = dist(B)
Bd_km = kmeans(B, 4)
# Only two points are within one cluster:
x1 = c(5,10)
x2 = c(5,7)
# The center of this cluster is:
xm = c(5,8.5)
# Within sum of squares is
w1 = (5-5)**2 + (5-5)**2 + (10-8.5)**2 + (8.5-7)**2 


