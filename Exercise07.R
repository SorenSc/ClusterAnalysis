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

t.test(c39,c52)
# P-value is smaller than the given significant-level. Because of that,
# H0 is rejected. There seems to be a true difference in the means.


################################################################################
# Exercise 22
################################################################################
knie <- read.csv("~/Downloads/knie.txt", sep="")
head(knie)
t.test(PAIN~factor(TH), data = knie)
# Does show the variables in a factorized way. Without factor() 
# it works similar. Nevertheless factor() should be used.


attach(knie)
chisq.test(table(TH,PAIN))
detach(knie)

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

################################################################################
# Exercise 24
################################################################################

################################################################################
# Exercise 25
################################################################################