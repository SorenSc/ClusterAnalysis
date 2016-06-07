library(mvtnorm)


################################################################################
# Declare functions
################################################################################

# Creates a user-defined number of clusters with 10 to 200 data points for each
# one.
create_cluster = function(number_of_clusters){
  
  data = matrix

  for(i in 1:number_of_clusters){
    
    random_numbers = round(runif(4,0,10))
    number_of_points = round(runif(1,10,200))
    
    mean_of_data = c(random_numbers[1],random_numbers[2])
    # Covariance matrix of has to have a positiv semidefinite quadratic form
    # Matrix fullfills the criteria because Random_numbers is sum of 
    # random_numbers[4] and another random number
    sigma_of_data = matrix(c(sum(random_numbers[3],random_numbers[4]),
                             random_numbers[4],
                             random_numbers[4],
                             sum(random_numbers[3],random_numbers[4])),2)

    # Combine the matrixes
    data = rbind(rmvnorm(number_of_points,
                         mean_of_data,
                         sigma_of_data),
                 rmvnorm(number_of_points,
                   mean_of_data,
                   sigma_of_data))
    
    

  }
  return(data)
}

number_of_clusters = 4
data = create_cluster(number_of_clusters)
