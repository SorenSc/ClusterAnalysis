################################################################################
# Declare functions
################################################################################


# Creates a user-defined number of clusters with 10 to 200 data points for each
# one.
# - Number_of_clusters
# - Rn_start              # Random number starting value
# - Rn-stop               # Random number stopping value
# - Number_of_points      
create_clusters = function(number_of_clusters, rn_start, rn_stop, number_of_point){
  
  data = matrix(0,0,3)
  
  for(i in 1:number_of_clusters){
    
    # If the range of random_numbers is chosen larger the possibility 
    # of more different clusters is higher
    random_numbers = round(runif(4, rn_start, rn_stop))
    number_of_points = round(runif(1, 10, number_of_point))
    
    index = matrix(i,number_of_points,1)
    
    new_cluster = cbind(create_single_cluster(random_numbers, number_of_points),
                        index)
    
    # Combine the different matrixes to a data matrix
    data = rbind(data, new_cluster)
  }
  
  colnames(data) = c("x","y","cluster_index")
  
  data = as.data.frame(data)
  
  return(data)
}


# Creates a single cluster to given
# - Random_numbers
# - Number_of_points
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
# - Number_of_clusters
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
# - Xdata
# - Ydata
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
# - Data
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

# Draws a histogram and its density function.
# - Data matrix
# - Spec_data             # Data to be plotted
# - Binwidth              # Changes the width of the bars
# - Xlab
# - Ylab
# - Main
draw_histogram = function(ggplot, data, spec_data, binwidth, xlab, ylab, main, col = 1){
  
  if(ggplot == "ggplot"){
    plot = ggplot(data, aes(x = spec_data)) + 
      geom_histogram(aes(y = ..density..), 
                     binwidth = binwidth,
                     colour = 1,
                     fill = "white") + 
      geom_density() +
      xlab(xlab) +
      ylab(ylab) + 
      ggtitle(main)
    
  }else{
    hist(spec_data,
         breaks = 20,
         xlab = xlab,
         ylab = ylab,
         main = main,
         border = col,
         freq = FALSE)
    a = spec_data[!is.na(spec_data)]
    d = density(a)
    lines(d,col = col)
  }
  
  return(plot)
}

# Resets the parameters of par to it's default
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

# Returns the index of the last element which fits a condition
# - Data
max_index_of_vector = function(data){
  borders = c()
  for(i in 2:number_of_clusters){
    borders = c(borders,match(i,data$cluster_index)-1)
  }  
  max(borders, na.rm=TRUE)
  return(borders)
}

# Draws a heatmap with the given options
# - Cluster_data
# - Rowv            # Row dendrogram
# - Clov            # Column dendrogram
# - Borders         # Draw borders of clusters into the plot
# - Main            # Title of the plot
draw_heatmap = function(cluster_data, rowv = NULL, colv = NULL, borders = NULL, main){
  heatmap(as.matrix(cluster_data), 
          Rowv=rowv,
          Colv=colv,
          col = cm.colors(256),
          scale="column", 
          margins=c(1,1),
          main = main,
          add.expr = abline(h = borders, v = borders, lwd = 1))
}

# Uses the function hclust to perform hierarchical cluster analysis techniques
# - Cluster_mechanisms          # Names of cluster_mechanisms
# - Cluster_data                # Data on which analysis is performed on
perform_hierarchical_cluster_analysis = function(cluster_mechanisms, cluster_data){
  
  cluster_output = list()
  
  for(i in 1:length(cluster_mechanisms)){
    
    main = paste(toupper(substring(cluster_mechanisms[i],1,1)), substring(cluster_mechanisms[i],2),sep = "")
    
    cluster_output[[i]] = hclust(cluster_data, method = cluster_mechanisms[i])
    
  }
  
  return(cluster_output)
}

# Plots the dendrograms to given data
# - Cluster_mechanisms
# - Cluster_analysis_output
# - Labels
plot_dendrograms = function(cluster_mechanisms, cluster_analysis_output, labels){
  
  for(i in 1:length(cluster_mechanisms)){
    
    main = paste(toupper(substring(cluster_mechanisms[i],1,1)), substring(cluster_mechanisms[i],2),sep = "")
    
    plot(cluster_analysis_output[[i]],
         hang = -0.1,
         las = 2,
         labels = data$cluster_index,
         main = main,
         sub = NA,
         xlab = "Datenpunkte",
<<<<<<< HEAD
         ylab = "Höhe")
    
=======
         ylab = "HÃ¶he")

>>>>>>> e339923814513b2be7a45a6587cf6b35fe08f56c
    # The height can hardly be specified using an algorithm.
    # Because of that it is not done automatically.
    # abline(h = , 
    #        col = "lightgrey")
    
  }
}

# Uses the Code from the A2R package to draw dendrograms
# - Cluster_mechanisms
# - Cluster_analysis_output
plot_fancy_dendrograms = function(cluster_meachanism, hierarchical_cluster_analysis_output){
  
  source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
  
  op = par(bg = "gray25")
  cols = hsv(c(0.2, 0.4, 0.65, 0.95), 1, 1, 0.8)
  
  for(i in 1:length(hierarchical_cluster_analysis_output)){
    
    main = paste(toupper(substring(cluster_mechanisms[i],1,1)), substring(cluster_mechanisms[i],2),sep = "")
    
    A2Rplot(hierarchical_cluster_analysis_output[[i]],
            # labels = data$cluster_index,
            k = 4,
            boxes = FALSE,
            col.up = "gray50",
            col.down = cols,
            main = main)
  }
  
  par(resetPar())
}

# Plot round dendograms by using the library ape
# - Cluster_mechanisms
# - Cluster_analysis_output
plot_round_dendrograms = function(cluster_mechanisms, cluster_output){
  
  for(i in 1:length(cluster_output)){
    
    main = paste(toupper(substring(cluster_mechanisms[i],1,1)), substring(cluster_mechanisms[i],2),sep = "")
    
    # Transforming the hclust output to a plottable output
    CL1 = as.hclust(cluster_output[[i]])
    CL2 = as.phylo(CL1)
    
    plot(CL2, 
         type="fan", 
         main = main,
         las = 2,
         cex = 0.5)   # Scalling of text
    
  }
}

# Creates a scatterplot which shows the original belonging of data points to the cluster 
# and also displays the new assignment by a hierarchical cluster algorithm as specification 
# of the symbol.
# - Cluster_output
# - Number_of_clusters
# - cluster_mechansims
scatterplot_hclust_and_data = function(cluster_output, number_of_clusters, data, cluster_mechanisms){
  
  xlab = "x-Werte"
  ylab = "y-Werte"
  
  for(i in 1:length(cluster_output)){
    
    cluster_mechanism = paste(toupper(substring(cluster_mechanisms[i],1,1)), substring(cluster_mechanisms[i],2),sep = "")
<<<<<<< HEAD
    main = paste("Vergleich tatsächlicher und bestimmter Zuordnung für:", cluster_mechanism)
=======
    main = paste("Vergleich tatsÃ¤chlicher und bestimmter Zuordnung fÃ¼r:", cluster_mechanism)
>>>>>>> e339923814513b2be7a45a6587cf6b35fe08f56c
    
    # Divides data into given number of clusters
    analysed_belonging = cutree(cluster_output[[i]],
                                k = number_of_clusters)
    
    plot(data$x,
         data$y,
         xlab = xlab,
         ylab = ylab,
         main = main,
         las = 2,
         col = data$cluster_index + 1,
         pch = analysed_belonging)
    
  }
  
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

# Import statements
library(mvtnorm)

# Import statements
library(mvtnorm)

# Create a predefined number of clusters
number_of_clusters = 4
data = create_clusters(number_of_clusters, 
                       0,
                       100,
                       20)

# Mention:
# - Continous data
# - Random based data
# - Do I have to standardize the two variables?

##########################################################################################################
# Visualisierung der Daten:
##########################################################################################################

##########################################################################################################
# (a) F?gen Sie die Daten in einer Datenmatrix zusammen (vgl. Hausarbeit.r).

# Already done during creation of the data

# (b) Visualisieren Sie die Daten durch geeignete Grafiken, z.B. Streudiagramme oder Histogramme/Boxplots,
# sowohl unter Einbeziehung der, gemäß Simulation oben, "wahren" ClusterZugehörigkeit
# als auch ohne. Letztere Fall entspricht der Situation, die Sie in der Praxis
# vorfinden.

##########################################################################################################
# Inspect the data:

head(data)
# x        y cluster_index
# 1 67.16443 64.16422             1
# 2 86.07862 71.77400             1
# 3 87.80806 80.84759             1
# 4 78.15669 81.95668             1
# 5 84.38063 65.20528             1
# 6 82.87874 68.52617             1

summary(data)
# x                y          cluster_index  
# Min.   :-14.00   Min.   :-2.383   Min.   :1.000  
# 1st Qu.: 12.13   1st Qu.:25.835   1st Qu.:2.000  
# Median : 20.65   Median :45.192   Median :3.000  
# Mean   : 31.72   Mean   :46.242   Mean   :2.733  
# 3rd Qu.: 43.84   3rd Qu.:65.733   3rd Qu.:4.000  
# Max.   : 92.07   Max.   :90.555   Max.   :4.000  

##########################################################################################################
# Scatterplot
# Plot all data points together
par(mfrow=c(1,1))
plot(data[,c("x","y")],
     col = data[,"cluster_index"]+1,
     main = "Zufallsbasierter Datensatz von zwei Variablen",
     las = 2)

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

# TODO
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
       ylab = "y",
       las = 2)
  
  # Get number of points for each cluster
  leg = getLegendInformation(data$x[data$cluster_index == i],
                             data$y[data$cluster_index == i])
  
  # Draw legend to graph
  legend("topleft",
         leg,
         bty ='n')
  
}

##########################################################################################################
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


##########################################################################################################
# Histogram of all x - data

# Import statements
library(ggplot2)

par(mfrow = c(1,1))

draw_histogram("ggplot", data, data$x, 5 ,"X-Werte", "Dichte", "Verteilung der x-Werte")
draw_histogram("ggplot", data, data$y, 5 ,"Y-Werte", "Dichte", "Verteilung der y-Werte")

par(mfrow = plot_order(number_of_clusters))

# Be aware of the fact that the scales are different
for(i in 1:number_of_clusters){
  draw_histogram("", 
                 structure(xy_of_data), 
                 xy_of_data[[1]][,i], 
                 5 ,
                 "X-Werte", 
                 "Dichte", 
                 paste("Verteilung der x-Werte des Clusters",i),
                 i+1)
}

for(i in 1:number_of_clusters){
  draw_histogram("", 
                 structure(xy_of_data), 
                 xy_of_data[[2]][,i], 
                 5 ,
                 "Y-Werte", 
                 "Dichte", 
                 paste("Verteilung der y-Werte des Clusters",i),
                 i+1)
}


##########################################################################################################
# Heatmaps
par(mfrow=c(1,1))

# Prepare the cluster data
cluster_data = cbind(data$x, data$y)
dist(cluster_data) -> cluster_data_dist

borders = max_index_of_vector(data)

draw_heatmap(cluster_data_dist, NA, NA, main = "Heatmap of all clusters")
draw_heatmap(cluster_data_dist, NA, NA, borders, main = "Heatmap with real borders")
# Mention reordering of data
draw_heatmap(cluster_data_dist, main = "Heatmap with dendrograms")

# TODO
# How to set the binwidth?
# Make names of the functions consistent

##########################################################################################################
# Vergleich von Cluster-Verfahren
##########################################################################################################

##########################################################################################################
# (a) Betrachten Sie verschiedene Cluster-Verfahren Ihrer Wahl und wenden Sie diese auf die Daten
<<<<<<< HEAD
# oben an. Beachten Sie dabei, dass die "wahre" Cluster-Zugehörigkeit hier nicht eingehen darf
# ("unsupervised learning").
=======
# oben an. Beachten Sie dabei, dass die â€œwahreâ€ Cluster-ZugehÃ¶rigkeit hier nicht eingehen darf
# (â€œunsupervised learningâ€).
>>>>>>> e339923814513b2be7a45a6587cf6b35fe08f56c
par(resetPar())

##########################################################################################################
# Hierarchical Clustering
cluster_mechanisms = c("ward.D",
                       "ward.D2",
                       "single",
                       "complete",
                       "average",
                       "mcquitty",
                       "median",
                       "centroid")

hierarchical_cluster_analysis_output = perform_hierarchical_cluster_analysis(cluster_mechanisms, 
                                                                             cluster_data_dist)


# Mention:
# - No scaling is necessary because variables are created based on the same technique
# usa algorithm to compare different cluster techniques
# - Inversions are hard to interpret

##########################################################################################################
# (b) Vergleichen Sie die Ergebnisse unter Verwendung geeigneter Grafiken. Zur Beurteilung der
# Qualität der Cluster-Verfahren können Sie nun wieder die "wahre" Cluster-Zugehörigkeit heranziehen.

##########################################################################################################
# Dendrograms
plot_dendrograms(cluster_mechanisms,
                 hierarchical_cluster_analysis_output,
                 data$cluster_index)

# TODO
# - Code is producing an error
plot_fancy_dendrograms(cluster_mechanisms,
                       hierarchical_cluster_analysis_output)

par(resetPar())

# Import statements
library(ape)

par(mfrow = plot_order(length(cluster_mechanisms)))

# Plot round dendrograms
plot_round_dendrograms(cluster_mechanisms,
                       hierarchical_cluster_analysis_output)


##########################################################################################################
# Scatterplots
par(mfrow=plot_order(number_of_clusters))
scatterplot_hclust_and_data(hierarchical_cluster_analysis_output,
                            number_of_clusters,
                            data,
                            cluster_mechanisms)


##########################################################################################################
# Thanks to MingXueWang who provided some orientation on:
# https://www.kaggle.com/coolman/d/uciml/iris/notebook-f5225665739c989910b3/notebook
# K-means clustering

model = kmeans(cluster_data, 
               centers = number_of_clusters)

plot(cluster_data, 
     col = data$cluster_index + 1,
     pch = model$cluster,
     xlab = "x-Werte",
     ylab = "y-Werte",
     main = "K-means Clustern",
     las = 2)

# Display the centers of the cluster
points(model$centers, 
       pch=1:number_of_clusters, 
       cex=2)


##########################################################################################################
# Thanks to MingXueWang who provided some orientation on:
# https://www.kaggle.com/coolman/d/uciml/iris/notebook-f5225665739c989910b3/notebook
# Fuzzy C-Means

# Import statements
library(e1071)

model = cmeans(cluster_data,
               number_of_clusters,
               iter.max = 100,
               m = 2,
               method = "cmeans")

plot(cluster_data, 
     col = data$cluster_index + 1,
     pch = model$cluster,
     xlab = "x-Werte",
     ylab = "y-Werte",
     main = "Fuzzy C-Clustern",
     las = 2)

# Display the centers of the cluster
points(model$centers, 
       pch=1:number_of_clusters, 
       cex=2)


##########################################################################################################
# Thanks to MingXueWang who provided some orientation on:
# https://www.kaggle.com/coolman/d/uciml/iris/notebook-f5225665739c989910b3/notebook
# Multi-Gaussian with Expectation-Maximization

# Import statements
library(mclust)

model = Mclust(cluster_data,
               number_of_clusters)

plot(cluster_data, 
     col = data$cluster_index + 1,
     pch = model$classification,
     xlab = "x-Werte",
     ylab = "y-Werte",
     main = "Multi-Gaussian with Expectation-Maximization",
     las = 2)


##########################################################################################################
# Thanks to MingXueWang who provided some orientation on:
# https://www.kaggle.com/coolman/d/uciml/iris/notebook-f5225665739c989910b3/notebook
# Density-based Cluster

# Import statements
library(fpc)

# The value of eps needs some experimentation. For four clusters with 10 to 20 data points 6 seemed 
# to be a good value.
model = dbscan(cluster_data,
               eps = 6,
               MinPts = 4)

plot(cluster_data, 
     col = data$cluster_index + 1,
     pch = model$cluster,
     xlab = "x-Werte",
     ylab = "y-Werte",
     main = "Density-based Cluster",
     las = 2)


# TODO
# Random Forest for clustering 
# library(randomForest)
# 
# cluster_data.rf = randomForest(cluster_data, prox = TRUE)
# cluster_data.p = classCenter(data$x, data$y, cluster_data.rf$prox)
# plot(cluster_data,col= data$cluster_index + 1)
# points(cluster_data.p)
# 
# data(iris)
# iris.rf <- randomForest(iris[,-5], iris[,5], prox=TRUE)
# iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox)
# plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
#      bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
#      main="Iris Data with Prototypes")
# points(iris.p[,3], iris.p[,4], pch=21, cex=2, bg=c("red", "blue", "green"))

# - Maybe not c for combining the results but cbind or something like append
# - Cluster data based on the origin dataset to get an appropriate labeling of the data

##########################################################################################################
# Dendrograms
plot_dendrograms(cluster_mechanisms,
                 hierarchical_cluster_analysis_output,
                 data$cluster_index)

# TODO
# - Code is producing an error
plot_fancy_dendrograms(cluster_mechanisms,
                       hierarchical_cluster_analysis_output)

par(resetPar())

# Import statements
library(ape)

par(mfrow = plot_order(length(cluster_mechanisms)))

# Plot round dendrograms
plot_round_dendrograms(cluster_mechanisms,
                       hierarchical_cluster_analysis_output)


##########################################################################################################
# Scatterplots
par(mfrow=plot_order(number_of_clusters))
scatterplot_hclust_and_data(hierarchical_cluster_analysis_output,
                            number_of_clusters,
                            data,
                            cluster_mechanisms)


##########################################################################################################
# Thanks to MingXueWang who provided some orientation on:
# https://www.kaggle.com/coolman/d/uciml/iris/notebook-f5225665739c989910b3/notebook
# K-means clustering

model = kmeans(cluster_data, 
               centers = number_of_clusters)

plot(cluster_data, 
     col = data$cluster_index + 1,
     pch = model$cluster,
     xlab = "x-Werte",
     ylab = "y-Werte",
     main = "K-means Clustern",
     las = 2)

# Display the centers of the cluster
points(model$centers, 
       pch=1:number_of_clusters, 
       cex=2)


##########################################################################################################
# Thanks to MingXueWang who provided some orientation on:
# https://www.kaggle.com/coolman/d/uciml/iris/notebook-f5225665739c989910b3/notebook
# Fuzzy C-Means

# Import statements
library(e1071)

model = cmeans(cluster_data,
               number_of_clusters,
               iter.max = 100,
               m = 2,
               method = "cmeans")

plot(cluster_data, 
     col = data$cluster_index + 1,
     pch = model$cluster,
     xlab = "x-Werte",
     ylab = "y-Werte",
     main = "Fuzzy C-Clustern",
     las = 2)

# Display the centers of the cluster
points(model$centers, 
       pch=1:number_of_clusters, 
       cex=2)


##########################################################################################################
# Thanks to MingXueWang who provided some orientation on:
# https://www.kaggle.com/coolman/d/uciml/iris/notebook-f5225665739c989910b3/notebook
# Multi-Gaussian with Expectation-Maximization

# Import statements
library(mclust)

model = Mclust(cluster_data,
               number_of_clusters)

plot(cluster_data, 
     col = data$cluster_index + 1,
     pch = model$classification,
     xlab = "x-Werte",
     ylab = "y-Werte",
     main = "Multi-Gaussian with Expectation-Maximization",
     las = 2)


##########################################################################################################
# Thanks to MingXueWang who provided some orientation on:
# https://www.kaggle.com/coolman/d/uciml/iris/notebook-f5225665739c989910b3/notebook
# Density-based Cluster

# Import statements
library(fpc)

# The value of eps needs some experimentation. For four clusters with 10 to 20 data points 6 seemed 
# to be a good value.
model = dbscan(cluster_data,
               eps = 6,
               MinPts = 4)

plot(cluster_data, 
     col = data$cluster_index + 1,
     pch = model$cluster,
     xlab = "x-Werte",
     ylab = "y-Werte",
     main = "Density-based Cluster",
     las = 2)


# TODO
# Random Forest for clustering 
# library(randomForest)
# 
# cluster_data.rf = randomForest(cluster_data, prox = TRUE)
# cluster_data.p = classCenter(data$x, data$y, cluster_data.rf$prox)
# plot(cluster_data,col= data$cluster_index + 1)
# points(cluster_data.p)
# 
# data(iris)
# iris.rf <- randomForest(iris[,-5], iris[,5], prox=TRUE)
# iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox)
# plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
#      bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
#      main="Iris Data with Prototypes")
# points(iris.p[,3], iris.p[,4], pch=21, cex=2, bg=c("red", "blue", "green"))

# - Maybe not c for combining the results but cbind or something like append
# - Cluster data based on the origin dataset to get an appropriate labeling of the data

##########################################################################################################
# (c) Betrachten Sie nicht nur ein Simulationsszenario; d.h. untersuchen Sie verschiedene Settings,
# z.B. bzgl. Anzahl Variablen, Beobachtungen, Cluster, sowie Mittelwertsvektoren und Kovarianzmatrizen.

# Die Anzahl der Variablen zu variieren ist schwierig, da alle Verfahren zur Darstellung und zum Clustern 
# von Beginn an auf zwei Variablen ausgelegt wurden.

# TODO
# - Varierende Zahl an Beobachtungen
# - Varierende Zahl an Clustern

# - Verschiedene Mittelwertsvektoren/Kovarianzmatrizen
<<<<<<< HEAD
# Die Matrizen ändern sich aufgrund des Zufallsbasierten Verfahrens automatisch.
=======
# Die Matrizen Ã¤ndern sich aufgrund des Zufallsbasierten Verfahrens automatisch.
>>>>>>> e339923814513b2be7a45a6587cf6b35fe08f56c



##########################################################################################################
# (d) Betrachten Sie nach Möglichkeit jeweils nicht nur einen Simulationsdurchlauf sondern führen
# Sie die Simulation mehrmals durch.