################################################################################
# Task 15
################################################################################

X = matrix(c(0,9,7,12,8,9,0,4,2,5,7,4,0,1,13,12,2,1,0,6,8,5,13,6,0), ncol = 5, nrow = 5)
X = as.dist(X)

sc = hclust(X, method = "single")
cc = hclust(X, method = "complete")

plot(sc, main = "Single Linkage", xlab = '')
plot(cc, main = "Complete Linkage", xlab = '')
abline(h = 10, col = "grey")


################################################################################
# Task 16
################################################################################

# Read data
jet <- read.csv("~/Documents/TU Clausthal/Datenanalyse und Datenmanagement/jet.txt", sep="")

# Get general information about the data
head(jet)
summary(jet)

################################################################################
# a)

# Mark all planes which are able to land on a carrier
plot(jet, pch = as.numeric(jet$CAR))

# Extract part of data
small_jet = jet[, c("SPR", "RGF", "PLF", "SLF")]

# Scale the data
scaled_small_jet = scale(small_jet)

# Clustering with k-means-algorithm
two_cl = kmeans(scaled_small_jet, 2, nstart = 1000)
thr_cl = kmeans(scaled_small_jet, 3, nstart = 1000)
fou_cl = kmeans(scaled_small_jet, 4, nstart = 1000)

# Plot colorful clusters
plot(small_jet, col = two_cl$cluster)
plot(small_jet, col = thr_cl$cluster)
plot(small_jet, col = fou_cl$cluster)

################################################################################
# b)

# Calculate sum of squares within group
wss = rep(0,length(as.list(small_jet)$SLF)-1)
for(i in 1:length(wss)){
  wss[i] = sum(kmeans(scaled_small_jet, i, nstart = 1000)$withinss)
}

# Plot withinss
xlab = 'Number of groups'
ylab = 'Withinss of group'
plot(wss, xlab = xlab, ylab = ylab, type='b')


################################################################################
# Example-calculation of the within sum of squares in general
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

################################################################################
# c)

# Use of agglomerative and hierarchical clustering-algorithm
hclust_small_jet = hclust(dist(scaled_small_jet), method = 'centroid')

# Display dendrogram
plot(hclust_small_jet)
plot(fou_cl$cluster)

# Divide hclust-output also into four clusters
csj = cutree(hclust_small_jet, k = 4)

# The contingency table shows how the members of cluster i in csj are placed in 
# cluster j of hclust_small_jet.
table(fou_cl$cluster,csj)


# Clustering with mixture distribution
library(mclust)
mc <- Mclust(scaled_small_jet)
summary(mc)

# BIC-plot
plot(mc, what = "BIC", col = "black")

# Scatter plot matrix
plot(scaled_small_jet, pch = fou_cl$cluster, col = mc$classification)

# Contingency table
table(fou_cl$cluster, mc$classification)
 
