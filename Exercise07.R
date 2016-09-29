
################################################################################
# Aufgabe 21
################################################################################
library(MASS)
data(cabbages)
cabbages

library(ggplot2)
qplot(Cult, VitC, data=cabbages)
head(cabbages)

t.test(VitC~Cult, data=cabbages)
# Check wether the two means are equal
# Df is not a round number because the variance was estimated

# Welch Two Sample t-test
# 
# data:  VitC by Cult
# t = -6.3909, df = 56.376, p-value = 3.405e-08
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -16.94296  -8.85704
# sample estimates:
#   mean in group c39 mean in group c52 
# 51.5              64.4

t.test(VitC~Cult, data=cabbages, var.equal=TRUE)
# Df is a round value

# Welch Two Sample t-test
# 
# 
# Two Sample t-test

# data:  VitC by Cult
# t = -6.3909, df = 58, p-value = 3.065e-08
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -16.940479  -8.859521
# sample estimates:
#   mean in group c39 mean in group c52 
# 51.5              64.4 

################################################################################
# Aufgabe 22
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
# Aufgabe 23
################################################################################

################################################################################
# Aufgabe 24
################################################################################

################################################################################
# Aufgabe 25
################################################################################