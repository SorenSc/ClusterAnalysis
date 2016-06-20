library(mvtnorm)


################################################################################
# Declare functions
################################################################################


# Creates a user-defined number of clusters with 10 to 200 data points for each
# one.
# - number_of_clusters
create_clusters = function(number_of_clusters, rn_start, rn_stop, number_of_point){
  
  data = matrix(0,0,3)
  
  for(i in 1:number_of_clusters){
    
    # If the range of random_numbers is chosen larger the possibility 
    # of more different clusters is higher
    random_numbers = round(runif(4, rn_start, rn_stop))
    number_of_points = round(runif(1, 10, number_of_point)))
    
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

# Draws a histogram and its density function.
# - data matrix
# - spec_data to be plotted
# - binwidth to change width of the bars
# - xlab
# - ylab
# - main
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
# - data
max_index_of_vector = function(data){
  borders = c()
  for(i in 2:number_of_clusters){
    borders = c(borders,match(i,data$cluster_index)-1)
  }  
  max(borders, na.rm=TRUE)
  return(borders)
}

# Draws a heatmap with the given options
# - cluster_data
# - rowv            # row dendogram
# - clov            # column dendogram
# - borders         # draw borders of clusters into the plot
# - main            # title of the plot
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
data = create_clusters(number_of_clusters, 
                       c(0,100),
                       c(10,20))

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


# Histogram of all x - data
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



# Heatmaps
par(mfrow=c(1,1))
cluster_data = cbind(data$x, data$y)

dist(cluster_data) -> cluster_data
borders = max_index_of_vector(data)

draw_heatmap(cluster_data, NA, NA, main = "Heatmap of all clusters")
draw_heatmap(cluster_data, NA, NA, borders, main = "Heatmap with real borders")
# Mention reordering of data
draw_heatmap(cluster_data, main = "Heatmap with dendograms")

# TODO
# How to set the binwidth?
# Make names of the functions consistent

##########################################################################################################
# Vergleich von Cluster-Verfahren
##########################################################################################################

##########################################################################################################
# (a) Betrachten Sie verschiedene Cluster-Verfahren Ihrer Wahl und wenden Sie diese auf die Daten
# oben an. Beachten Sie dabei, dass die “wahre” Cluster-Zugehörigkeit hier nicht eingehen darf
# (“unsupervised learning”).

cluster_mechanisms = c("ward.D",
                       "ward.D2",
                       "single",
                       "complete",
                       "average",
                       "mcquitty",
                       "median",
                       "centroid")

for(i in 1:length(cluster_mechanisms)){
  
  main = paste(toupper(substring(cluster_mechanisms[i],1,1)), substring(cluster_mechanisms[i],2),sep = "")
  
  hclust_output = hclust(cluster_data, method = cluster_mechanisms[i])
  
  plot(hclust_output,
       hang = -0.1,
       las = 2,
       labels = data$cluster_index,
       main = main,
       sub = NA,
       xlab = "Datenpunkte",
       ylab = "Höhe")
}


# Mention:
# - No scaling is necessary because variables are created based on the same technique
# usa algorithm to compare different cluster techniques
# - Inversions are hard to interpret

##########################################################################################################
# (b) Vergleichen Sie die Ergebnisse unter Verwendung geeigneter Grafiken. Zur Beurteilung der
# Qualität der Cluster-Verfahren können Sie nun wieder die “wahre” Cluster-Zugehörigkeit heranziehen.

# source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# # colored dendrogram
# op = par(bg = "gray25")
# cols = hsv(c(0.2, 0.4, 0.65, 0.95), 1, 1, 0.8)
# A2Rplot(cluster_wardD, 
#         k = 4, 
#         boxes = FALSE, 
#         col.up = "gray50", 
#         col.down = cols,
#         labels = data$cluster_index,
#         main = "Dendogram")
# 
# par(resetPar())

# plot(cz <- hclust(dm, method = "centroid"))


# Round dendogram to get a better overview
library(ape)
CL1 <- as.hclust(wardD2)
CL2 <- as.phylo(CL1)
plot(CL2, type="fan", cex=0.5)

library(mclust)

# Clustern der pottery Daten
help(Mclust)
mc <- Mclust(data)
summary(mc)

# BIC-Plot
plot(mc, pots, what = "BIC", col = "black")
help(mclustModelNames)

model <- kmeans(iris[,1:4], 3)
# plot with first two attributes: Sepal.Length Sepal.Width 
plot(iris[,c(1,2)], col=model$cluster, main="K-Means")
# point center of first two attributes
points(model$centers[, c(1,2)], col=1:3, pch=8, cex=2)

model <- kmeans(cluster_data, 4)
plot(model)
# plot with first two attributes: Sepal.Length Sepal.Width 
plot(iris[,c(1,2)], col=model$cluster, main="K-Means")
# point center of first two attributes
points(model$centers[, c(1,2)], col=1:4, pch=8, cex=2)

# Maybe not c for combining the results but cbind or something like append

##########################################################################################################
# (c) Betrachten Sie nicht nur ein Simulationsszenario; d.h. untersuchen Sie verschiedene Settings,
# z.B. bzgl. Anzahl Variablen, Beobachtungen, Cluster, sowie Mittelwertsvektoren und Kovarianzmatrizen.

##########################################################################################################
# (d) Betrachten Sie nach Möglichkeit jeweils nicht nur einen Simulationsdurchlauf sondern führen
# Sie die Simulation mehrmals durch.
