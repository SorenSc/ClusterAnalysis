################################################################################
# Exercise 10
################################################################################

################################################################################
# Exercise 11
################################################################################

data("heptathlon",package="HSAUR2")
heptathlon

summary(heptathlon)
boxplot(heptathlon)

# Remove last athlet
wa = which.min(heptathlon$score)
heptathlon = heptathlon[-wa,]

# Scale the data
hep = scale(subset(heptathlon,select = -score))

summary(hep)
boxplot(hep)

heatmap(hep, scale = 'none')
