# library(lattice)
# library(mvtnorm)
# 
# dat = rbind(rmvnorm(25, mean = c(3,3)),
#             rmvnorm(20, mean = c(10,8)),
#             rmvnorm(10, mean = c(20,1)))
# plot(abs(dat), xlab = expression(x[1]), ylab = expression(y[1]))
# 
# dat_dist = dist((dat[,1], dat[,1]))
# plot(hclust(dat_dist, method="average"))

# Datenanalyse und Datenmanagement
# Übung 4 - 11.05.2016

#############################################################
# Exercise 12
# a)
library(HSAUR2)
data("USairpollution")
UStmp = USairpollution[,names(USairpollution)!="SO2"]
UStmp = scale(UStmp)

dist_mat = dist(UStmp)

# Comparison of different clustering methods
cc = hclust(dist_mat, method = "ward.D")
plot(cc, main="Euclidean distance", xlab="City")
abline(h=8, col="grey")

cs = hclust(dist_mat, method = "single")
plot(cs, main="Single", xlab="City")

cc = hclust(dist_mat, method = "complete")
plot(cc, main="Complete", xlab="City")
abline(h=6.5, col="grey")

ca = hclust(dist_mat, method = "average")
plot(ca, main="Average", xlab="City")
abline(h=4, col="grey")

# b)
# Choosing of complete linkage for later calculation
cluster_structure = cutree(cc, h=6)

UStmp = USairpollution
UStmp = cbind(UStmp,cluster_structure,cbind(rep(1:41, each = 41)[1:41]))

c1 = cbind(UStmp$SO2[UStmp$cluster_structure == "1"])
c2 = cbind(UStmp$SO2[UStmp$cluster_structure == "2"])
c3 = cbind(UStmp$SO2[UStmp$cluster_structure == "3"])
dat = data.frame(c1,c2,c3)

boxplot(dat)

#############################################################
# Exercise 13
cluster = function(dist_mat, method, xlab){
  dendogram = hclust(dist_mat, method=method)
  title = paste(toupper(str_sub(method,1,1)),str_sub(method,2,-1L),sep='')
  plot(dendogram, main=title, xlab=xlab)
  return(dendogram)
}

library("cluster", lib.loc="/usr/lib/R/library")

data = dist(agriculture)

cluster(data, 'single', 'BIP')

dendogram = cluster(data, 'complete', 'BIP')
abline(h=15, col="grey")

cluster(data, 'average', 'BIP')
abline(h=10, col="grey")

# The complete and average linkage clustering mechanisms show the best results. 
# Based on the graphs I would recommend using to clusters.

groups = cutree(dendogram, h=15)


#############################################################
# Exercise 14
dist_mat = matrix(c(0,9,7,12,8,9,0,4,2,5,7,4,0,1,13,12,2,1,0,6,8,5,13,6,0),
                  nrow=5)
colnames(dist_mat) = rownames(dist_mat) = c("A","B","C","D","E")
dist_mat = as.dist(dist_mat)

cluster(dist_mat, 'single', 'City')
cluster(dist_mat, 'complete', 'City')
cluster(dist_mat, 'average', 'City')

