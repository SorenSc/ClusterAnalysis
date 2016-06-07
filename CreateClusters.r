library(mvtnorm)


################################################################################
# Declare functions
################################################################################

# Creates a user-defined number of clusters with 10 to 200 data points for each
# one.
# - number_of_clusters
create_clusters = function(number_of_clusters){
  
  data = matrix(0,0,3)

  for(i in 1:number_of_clusters){
    
    random_numbers = round(runif(4,0,10))
    number_of_points = round(runif(1,10,200))
    
    index = matrix(i,number_of_points,1)
    
    new_cluster = cbind(create_single_cluster(random_numbers, number_of_points),
                 index)
    
    data = rbind(data, new_cluster)
  }
  
  return(data)
}

# Creates a single cluster to given
# - random_numbers
# - number_of_points
create_single_cluster = function(random_numbers, number_of_points){
  
  data = matrix
  
  mean_of_data = c(random_numbers[1],random_numbers[2])
  # Covariance matrix of has to have a positiv semidefinite quadratic form
  # Matrix fullfills the criteria because Random_numbers is sum of 
  # random_numbers[4] and another random number
  sigma_of_data = matrix(c(sum(random_numbers[3],random_numbers[4]),
                           random_numbers[4],
                           random_numbers[4],
                           sum(random_numbers[3],random_numbers[4])),2)
  
  # Combine the matrixes
  data = rmvnorm(number_of_points,
                 mean_of_data,
                 sigma_of_data)
  
  return(data)
  
}

number_of_clusters = 4
data = create_clusters(number_of_clusters)
