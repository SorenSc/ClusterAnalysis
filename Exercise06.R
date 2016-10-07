################################################################################
# Exercise 17
################################################################################

# Import data about headsize
headsize =
  matrix(c(191, 195, 181, 183, 176, 208, 189, 197, 188, 192, 179, 183, 174, 190,
           188, 163, 195, 186, 181, 175, 192, 174, 176, 197, 190, 155, 149, 148,
           153, 144, 157, 150, 159, 152, 150, 158, 147, 150, 159, 151, 137, 155,
           153, 145, 140, 154, 143, 139, 167, 163, 179, 201, 185, 188, 171, 192,
           190, 189, 197, 187, 186, 174, 185, 195, 187, 161, 183, 173, 182, 165,
           185, 178, 176, 200, 187, 145, 152, 149, 149, 142, 152, 149, 152, 159,
           151, 148, 147, 152, 157, 158, 130, 158, 148, 146, 137, 152, 147, 143,
           158, 150)
         , nrow = 25, ncol = 4
         , dimnames = list(character(0)
                           , c("head1", "breadth1", "head2", "breadth2")))

# Scale is not necessary, because all variables have the same unit

# PCA of the data
pca_headsize = princomp(headsize)
# princomp(cor(headsize)) leads to the same results

summary(pca_headsize, loadings = T)

(cov(headsize))
(cor(headsize))

plot(pca_headsize)
plot(pca_headsize$sdev/sum(pca_headsize$sdev), 
     type = 'b',
     ylab = 'Explained variances')

################################################################################
# Tryings to calculate all variables

# Analysis of principal components makes sense if 
#   1. The number of variables is large in comparison to the number of observations
#   2. The variables are highly correlated

headsize <-
  matrix(c(191, 195, 181, 183, 
           155, 149, 148, 153, 
           179, 201, 185, 188, 
           145, 152, 149, 149)
         , nrow = 4, ncol = 4
         , dimnames = list(character(0)
                           , c("head1", "breadth1", "head2", "breadth2")))

# Calculation of the variance matrix
var(headsize)

# Covariance matrix
cov(headsize)

# Corrleation matrix
cor(headsize)

#################################################################################
# Estimation of the principal components

# Returns eigenvectors and eigenvalues
eigen(headsize)

# Eigenvectors and matrix of the covariance matrix
# This equals the loadings of the summary of the princomp(headsize)!
eigen(cov(headsize))

# Format data
headsize <- as.data.frame(headsize)

# Principal components
head_pca <- princomp(headsize)
head_pca
summary(head_pca, loadings = T)

# Visualization of the explained variance
plot(head_pca)

# Eigenvectors


# Matrix multiplied with eigenvectos
head_mult_eigen = as.matrix(headsize)%*%eigenvectors
# Matrix contains the eigenvectors on the diagonal and 0 on the rest 
# of the fields.
cov(head_mult_eigen)

# Amount on the total variation which is explained by the eigenvalues
round(eigenvalues/sum(eigenvalues)*100,digits=2)


#################################################################################
# Use of the correlation matrix

# Correlation matrix
cor(headsize)

# Eigenvectors and -values
eigenvectors = eigen(cor(headsize))$vectors
eigenvalues = eigen(cor(headsize))$values

sqrt(diag(cov(headsize)))

std_head = sqrt(diag(cov(headsize)))
std_head

headsize <- as.data.frame(headsize)

headsize01_std.frame<-as.matrix(headsize)%*%diag(1/std_head)

cov(as.matrix(headsize)%*%diag(1/std_head))

eigen(cor(headsize))$values*100/4


# Principal components
head_pca <- princomp(headsize)
head_pca
summary(head_pca, loadings = T)

# Varanschaulichung der erkl?rten Varianz
plot(head_pca)

names(head_pca)

test = eigen(headsize)
headsize %*% test$vectors[,1]
test$values[1] %*% test$vectors[,2]


# Standard derivation
sd(c(191, 195, 181, 183))

as.matrix(headsize)%*%eigenvectors
eigen(cov(headsize))

cov(headsize)
var(headsize)

################################################################################
# Exercise 18
###############################################################################
source("functions.R")

prisoners = matrix(c(1.000,0.402,0.396,0.301,0.305,0.339,0.340,
                    0.402,1.000,0.618,0.150,0.135,0.206,0.183,
                    0.396,0.618,1.000,0.321,0.289,0.363,0.345,
                    0.301,0.150,0.321,1.000,0.846,0.759,0.661,
                    0.305,0.135,0.289,0.846,1.000,0.797,0.800,
                    0.339,0.206,0.363,0.759,0.797,1.000,0.736,
                    0.340,0.183,0.345,0.661,0.800,0.736,1.000),
                   ncol = 7)

names = c('Head length','Head breadth','Face breadth','Left finger length',
          'Left forearm length','Left foot length','Height')

summary(prisoners)
pairs(prisoners,
      lower.panel = panel.smooth,
      upper.panel = panel.cor,
      diag.panel = panel.hist)

colnames(prisoners) = rownames(prisoners) = names 

pca_prisoners = princomp(covmat = prisoners)

summary(pca_prisoners, loadings = T)

plot(pca_prisoners)

screeplot(pca_prisoners, type="lines")


# Which components should be included?
# Based on the eigenvectors the first two components should be included.
# Based on the a desired explained variance of more than nintey percent,
# the first four,
# based on the screeplot the first four (take the component with the greatest 
# break in the smoothness).

################################################################################
# Exercise 19
################################################################################

measure = read.csv("~/Documents/TU Clausthal/Datenanalyse und Datenmanagement/measure.txt", sep="")

summary(measure)

pairs(measure,
      lower.panel = panel.smooth,
      upper.panel = panel.cor,
      diag.panel = panel.hist,
      pch = as.numeric(measure$gender))

# The variable 'gender' cannot be included, because it's not numeric.
small_measure = matrix(c(measure$chest,
                  measure$waist,
                  measure$hips),
                  ncol = 3)

# Set colnames for better interpretation
colnames(small_measure) = c('chest','waist','hips')

# PC Analysis and having a look to it
pca_measure = princomp(small_measure)
summary(pca_measure, loadings = T)

# Visualize the data
plot(pca_measure$scores[,1:2],
     pch = as.numeric(measure$gender),
     xlab = 'Comp. 1',
     ylab = 'Comp. 2',
     main = 'Comparison of the components')
legend("bottomleft", levels(measure$gender), pch=1:2, bty="n")

# Tryin to understand the meaning of component 2
plot(measure$waist, measure$hips)
hist(measure$waist)
hist(measure$hips)

################################################################################
# Exercise 20
################################################################################

jet = read.csv("~/Documents/TU Clausthal/Datenanalyse und Datenmanagement/jet.txt", sep="")

summary(jet)
head(jet)

jet_small = matrix(c(jet$SPR, jet$RGF, jet$PLF, jet$SLF), ncol = 4)
colnames(jet_small) = colnames(jet)[2:5]

pca_jet = princomp(jet_small)

plot(pca_jet)
summary(pca_jet, loadings = T)
# Based on these results, two principal components should be chosen.

################################################################################
plot(pca_jet$scores[,1:2],
     pch = as.numeric(jet$CAR))

################################################################################
# Clustering based on the two principal components
jet_new = pca_jet$scores[,1:2]

jet_new = scale(jet_new)

km = kmeans(jet_new, centers = 2, nstart = 1000)
# if centers is a number, how many random sets should be chosen?

sl = hclust(dist(jet_new), method = "single")
cl = hclust(dist(jet_new), method = "complete")

par(mfrow=c(2,2))
plot(jet_new, col = km$cluster, main = 'K-means')
plot(jet_new, col = cutree(sl, k=2), main = 'Single linkage')
plot(jet_new, col = cutree(cl, k=2), main = 'Complete linkage')

# Another way to perform the PCA
prc_jet = prcomp(jet_small)
prc_jet = prc_jet$x[,1:2]

km = kmeans(prc_jet, centers = 2, nstart = 1000)
# if centers is a number, how many random sets should be chosen?

sl = hclust(dist(prc_jet), method = "single")
cl = hclust(dist(prc_jet), method = "complete")

par(mfrow=c(2,2))
plot(prc_jet, col = km$cluster, main = 'K-means')
plot(prc_jet, col = cutree(sl, k=2), main = 'Single linkage')
plot(prc_jet, col = cutree(cl, k=2), main = 'Complete linkage')

