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
    
    # If the range of random_numbers is chosen larger the possibility 
    # of more different clusters is higher
    random_numbers = round(runif(4,0,100))
    number_of_points = round(runif(1,10,200))
    
    index = matrix(i,number_of_points,1)
    
    new_cluster = cbind(create_single_cluster(random_numbers, number_of_points),
                        index)
    
    # Combine the different matrixes to a data matrix
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


# Gives back the order of plots in the plot viewer
# - number_of_clusters
plot_order = function(number_of_clusters){
  if(number_of_clusters >= 1 && number_of_clusters <=16){
    if(number_of_clusters == 1){
      return(c(1,1))
    }else if(number_of_clusters == 2){
      return(c(1,2))
    }else if(number_of_clusters == 3 || number_of_clusters == 4){
      return(c(2,2))
    }else if(number_of_clusters == 5 || number_of_clusters == 6){
      return(c(2,3))
    }else if(number_of_clusters >= 7 && number_of_clusters <= 9){
      return(c(3,3))
    }else if(number_of_clusters >= 10 && number_of_clusters <= 12){
      return(c(3,4))
    }else if(number_of_clusters >= 13 && number_of_clusters <= 16){
      return(c(4,4))
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
}

# Returns information useful for a legend 
# - xdata
# - ydata
getLegendInformation = function(xdata, ydata){
  
  n = length(xdata)
  co = round(cor(xdata, ydata), 4)
  cv = round(cov(xdata, ydata), 4)
  
  leg = c(paste("n =",n),
          paste("cor =",co),
          paste("cov =", cv))
  
  return(leg)
}

# Gives back the x and y variables seperated for a given matrix
# - data
getxydata = function(data){
  
  nx = length(data$x)
  name_vector = NULL
  
  for(i in 1:number_of_clusters){
    # nx = max(nx, length(data$x[data$cluster_index == i]))
    name_vector = c(name_vector, paste("Cluster",i))
  }
  
  datax = matrix(0,nx,0)
  datay = matrix(0,nx,0)
  
  for(i in 1:number_of_clusters){
    
    current_x_vector = data$x[data$cluster_index == i]
    current_y_vector = data$y[data$cluster_index == i]
    
    length(current_x_vector) = nx
    length(current_y_vector) = nx
    
    datax = cbind(datax, current_x_vector)
    datay = cbind(datay, current_y_vector)
  }
  
  colnames(datax) = colnames(datay) = name_vector
  
  return(list(datax, datay))
}

##########################################################################################################
# Generieren der Daten:
##########################################################################################################

##########################################################################################################
# (a) Erzeugen Sie sich Daten für Cluster 1 mit Hilfe der Funktion rmvnorm() aus dem R Package
# mvtnorm. Orientieren Sie sich dabei an Hausarbeit.r auf Stud.IP.
# (b) Erzeugen Sie sich Daten für Cluster 2 ebenfalls mit Hilfe der Funktion rmvnorm() aber anderen
# Werten für den Mittelwertsvektor und die Kovarianzmatrix.
# (c) Erzeugen Sie sich nach Bedarf Daten für weitere Cluster.

# Create a predefined number of clusters
number_of_clusters = 4
data = create_clusters(number_of_clusters)
colnames(data) = c("x","y","cluster_index")

data = as.data.frame(data)

##########################################################################################################
# Visualisierung der Daten:
##########################################################################################################

##########################################################################################################
# (a) F?gen Sie die Daten in einer Datenmatrix zusammen (vgl. Hausarbeit.r).

# Already done during creation of the data

# (b) Visualisieren Sie die Daten durch geeignete Grafiken, z.B. Streudiagramme oder Histogramme/Boxplots,
# sowohl unter Einbeziehung der, gemäß Simulation oben, “wahren” ClusterZugehörigkeit
# als auch ohne. Letztere Fall entspricht der Situation, die Sie in der Praxis
# vorfinden.

# Inspect the data:

head(data)
# x          y cluster_index
# [1,]  4.148538810  3.1011608             1
# [2,]  9.859807586  5.8392289             1
# [3,]  8.529918834  8.8074696             1
# [4,]  6.153870381 -0.8054947             1
# [5,]  4.252371498  1.5169027             1
# [6,] -0.008676228 -0.9352170             1

summary(data)
# x                 y          cluster_index  
# Min.   :-10.326   Min.   :-3.953   Min.   :1.000  
# 1st Qu.:  1.259   1st Qu.: 1.081   1st Qu.:2.000  
# Median :  3.295   Median : 3.332   Median :3.000  
# Mean   :  4.047   Mean   : 3.361   Mean   :2.828  
# 3rd Qu.:  7.525   3rd Qu.: 5.739   3rd Qu.:4.000  
# Max.   : 16.772   Max.   :11.248   Max.   :4.000 

# Plot all data points together
par(mfrow=c(1,1))
plot(data[,c("x","y")],
     col = data[,"cluster_index"]+1,
     main = "Zufallsbasierter Datensatz von zwei Variablen")

# Get information contained in the legend 
# of the plot
leg = getLegendInformation(data$x,
                           data$y)

# Draw legend to graph
legend("topleft",
       leg,
       bty ='n')

# Set par(mfrow=...) parameter correct
plot_order(number_of_clusters) -> plot_vector
if(FALSE == is.null(plot_vector)){
  par(mfrow=plot_vector)
}

# Scatterplots combined with boxplots
# Idea's origin: http://statmethods.net/advgraphs/layout.html 
# Plot each cluster seperately and print number of data entries
# within cluster.
for(i in 1:number_of_clusters){
  plot(x = data$x[data$cluster_index == i],
       y = data$y[data$cluster_index == i],
       col = i+1,
       main = paste("Zufallsbasierte Datenpunkte von Cluster",i),
       xlab = "x",
       ylab = "y")
  
  # Get number of points for each cluster
  leg = getLegendInformation(data$x[data$cluster_index == i],
                             data$y[data$cluster_index == i])
  
  # Draw legend to graph
  legend("topleft",
         leg,
         bty ='n')
  
}

# Boxplots
xy_of_data = getxydata(data)

par(mfrow = c(1,1))

title_of_boxplots = "Boxplots der Variable"

# Draw boxplot of the x variable
x = cbind(data$x,xy_of_data[[1]])
colnames(x) = c("Gesamt", colnames(xy_of_data[[1]]))
col_vec = c(1:(number_of_clusters+1))
boxplot(x, 
        horizontal = TRUE,
        main = paste(title_of_boxplots,"x"),
        border = col_vec)

# Draw boxplot of the y variable
y = cbind(data$y,xy_of_data[[2]])
colnames(y) = c("Gesamt", colnames(xy_of_data[[2]]))
col_vec = c(1:(number_of_clusters+1))
boxplot(y,
        main = paste(title_of_boxplots,"y"),
        border = col_vec)


# Histogram
# Startung to ...

# Covarianz

# Dendogram


# eventuell weiter auseinander liegende Daten generieren lassen


# Streudiagramme
# Histogramm
# Boxplots

# Display n

##########################################################################################################
# Vergleich von Cluster-Verfahren
##########################################################################################################

##########################################################################################################
# (a) Betrachten Sie verschiedene Cluster-Verfahren Ihrer Wahl und wenden Sie diese auf die Daten
# oben an. Beachten Sie dabei, dass die “wahre” Cluster-Zugehörigkeit hier nicht eingehen darf
# (“unsupervised learning”).

##########################################################################################################
# (b) Vergleichen Sie die Ergebnisse unter Verwendung geeigneter Grafiken. Zur Beurteilung der
# Qualität der Cluster-Verfahren können Sie nun wieder die “wahre” Cluster-Zugehörigkeit heranziehen.

##########################################################################################################
# (c) Betrachten Sie nicht nur ein Simulationsszenario; d.h. untersuchen Sie verschiedene Settings,
# z.B. bzgl. Anzahl Variablen, Beobachtungen, Cluster, sowie Mittelwertsvektoren und Kovarianzmatrizen.

##########################################################################################################
# (d) Betrachten Sie nach Möglichkeit jeweils nicht nur einen Simulationsdurchlauf sondern führen
# Sie die Simulation mehrmals durch.
