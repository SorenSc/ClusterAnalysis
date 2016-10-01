################################################################################
# Exercise 21
################################################################################

library(MASS)
data = cabbages

head(data)
summary(data)

c39 = data$VitC[data$Cult == 'c39']
c52 = data$VitC[data$Cult == 'c52']

boxplot(c39,c52, names = c('c39','c52'))

ttvc = t.test(c39,c52)
# P-value is smaller than the given significant-level. Because of that,
# H0 is rejected. There seems to be a true difference in the means.

md = mean(c39)-mean(c52)
round(abs(md-ttvc$conf.int[2]),4)==round(md-ttvc$conf.int[1],4)

# The confidence interval can be calculated by doing the following: 
# Be aware that the result is not exactly the same. The intervall is
# shifted a little bit in the negative direction.
md-ttvc$statistic*sqrt((sd(c39)/length(c39))+(sd(c52)/length(c52)))

################################################################################
# Exercise 22
################################################################################
knie <- read.csv("~/Documents/TU Clausthal/Datenanalyse und Datenmanagement/knie.txt", sep="")

head(knie)

pla = knie$PAIN[knie$TH == 0]
the = knie$PAIN[knie$TH == 1]

t.test(pla, the)
# There is a true difference in means to the given significance level

chisq.test(table(knie$TH, knie$PAIN))
# There is a true difference in means to the given significance level

################################################################################

men = c(1,-1,0,-1,-1,1,2,0,1,-1,0,-1,2,
        0,2,2,1,0,1,1,1,-1,0,-1,0,1,0,-1,
        -1,1,0,1,-1,0,-2,2,2,-1,-1,1,0,1,0,
        -1,1,-1,0,0)

women = c(1,2,0,2,-2,1,-1,2,-1,0,-2,0,-1,-1,1,
          2,1,1,0,-2,2,-1,-1,-2,-1,2,-1,-1,1,1,
          -2,-2,-1,2,1,0,2,1,0,1,1,0,0,0,1,1,1,
          0,1,2,1,0)

data = c(men, women)
mean(data)
# The mean normally is not allowed because it is a ordinary scale.

t.test(data)
# Is a difference between men and women?
t.test(men, women)
# p-value = 0.854
# There is no significant difference between men and women

data = data.frame("Antwort" = c(women, men),
                  "Geschlecht" = factor(rep(c("w","m"),c(length(women),length(men)))))
head(data)

# Kreuzen Männer und Frauen unterschiedlich stark an?
chisq.test(table(data))


################################################################################
# Exercise 23
################################################################################

run_one = c(15.1,14.3,14.4,13.1,12.9,13.8,11.7,12.8,14.1,13.6,14.2)
run_two = c(14.9,14.2,14.5,13.1,12.6,14.0,11.7,12.3,13.8,13.7,14.0)

# The mean is nearly similar, only the 25% and 75% quantils have shifted to 
# lower times.
boxplot(run_one, run_two)

# Based on the task, I think, specifying the confidenc level is right.
alpha = 0.1
t.test(run_one, run_two, paired = T, alternative = 'greater', conf.level = alpha)

################################################################################
# Exercise 24
################################################################################

cakes = matrix(c(21,30,25,29,19,24), ncol = 2)
colnames(cakes) = c('tasty','distasteful')
rownames(cakes) = c('A','B','C')

chisq.test(cakes)
fisher.test(cakes)

################################################################################
# Exercise 25
################################################################################