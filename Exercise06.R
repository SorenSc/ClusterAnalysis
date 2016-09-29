################################################################################
# Aufgabe 17
################################################################################

# Import data about headsize
headsize <-
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
################################################################################

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
summary(head_pca, loadings = TRUE)

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
################################################################################

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
summary(head_pca, loadings = TRUE)

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
# Aufgabe 18
###############################################################################

################################################################################
# Aufgabe 19
################################################################################

################################################################################
# Aufgabe 20
################################################################################
