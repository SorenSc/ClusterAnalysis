# Datenanalyse und Datenmanagement
# Übung1 - 14.04.2016

#############################################################
# Exercise 6
letters = c("E","D","A","D","C","D","B","E","C","E","D","B","B","C","B","A","B","C","C","C","A","B","C","A","A","C","A","E","E","A")

abs_dist_letters = table(letters)

rel_dist_letters = abs_dist_letters/sum(abs_dist_letters)

par(mfrow=c(3,2))
# bar diagram
barplot(abs_dist_letters, horiz = TRUE, col = "NA", main="Barplot")
barplot(abs_dist_letters, horiz = TRUE, col = "NA", main="Barplot")

# histogram
hist(abs_dist_letters, main="Histogram")
hist(rel_dist_letters, main="Histogram")

# piechart
library(plotrix)
pie3D(abs_dist_letters, main="Piechart")
pie3D(rel_dist_letters, main="Piechart")

#############################################################
# Exercise 7
men_women = HairEyeColor[,,1]+HairEyeColor[,,2]
barplot(men_women, main = "men and women")
barplot(HairEyeColor[,,1], main="men")
barplot(HairEyeColor[,,2], main="women")

#############################################################
# Exercise 8
par(mfrow = c(2,2))

# a)
agriculture
# [ , 1]	x	 numeric	 per capita GNP
# [ , 2]	y	 numeric	 percentage in agriculture

# b)
plot(agriculture)

# c)
agr_data = agriculture

agr_data$region = factor(c("N","N","N","S","S","S","N","S","N","N","N","N"))

# d)
boxplot(x~region,agr_data, main="GNP by region")
boxplot(y~region,agr_data, main="% of agr by region")

# e)
x = agr_data$x[agr_data$region == "N"]
y = agr_data$y[agr_data$region == "N"]
plot(x,y)

x = agr_data$x[agr_data$region == "S"]
y = agr_data$y[agr_data$region == "S"]
plot(x,y)

#############################################################
# Exercise 9
par(mfrow=c(1,1))
agr_data$region = NULL
names(agr_data) = c("GNP","useOfAgr")

plot(agr_data$GNP, agr_data$useOfAgr, pch="°", ylim=c(0,25))
text(agr_data$GNP, agr_data$useOfAgr, rownames(agr_data), adj=c(NA,-0.5))
