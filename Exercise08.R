# The following link might be useful
# http://www.stat.yale.edu/Courses/1997-98/101/linmult.htm
# https://www.youtube.com/watch?v=ksOErR1ldgo

################################################################################
# Exercise 26
################################################################################

dose = c(1,5,3,8,2,2,10,8,7,4)
time = c(0.5,2.9,0.6,3.0,1.5,1.1,3.9,2.5,3.1,1.2)

cor(dose, time)
cov(dose, time)/(sd(dose)*sd(time))==(30/9)/(sd(dose)*sd(time))
sd(dose)==sqrt(86/9)

lm = lm(time~dose)

plot(dose, time, 
     xlab = 'Dosage',
     ylab = 'Reaction')
abline(lm$coefficients)


lm$coefficients[1]+lm$coefficients[2]*5.5
predict(lm, newdata = data.frame(dose=c(5.5)))

summary(lm)

1-((1-0.8188)*((length(dose)-1)/(length(dose)-2)))
1-((1-R^2)*(N-1)/(N-k-1))

plot(dose, time)
plot(lm)

################################################################################
dose = c(1,5,3,8,2,2,10,8,7,4)
reaction = c(0.5,2.9,0.6,3.0,1.5,1.1,3.9,2.5,3.1,1.2)
data = data.frame(dose,reaction)

corOfData = round(cor(data$dose, data$reaction),4)
plot(data)
legend("topleft",
       c(paste("Cor =",corOfData)),
       bty ='n')

regressionLine <- lm(data$reaction~data$dose)
summary(regressionLine)
# Call:
#   lm(formula = data$reaction ~ data$dose)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.7323 -0.3945  0.0200  0.3107  0.8700 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.28581    0.33631   0.850 0.420106    
# data$dose    0.34884    0.05802   6.013 0.000319 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.538 on 8 degrees of freedom
# Multiple R-squared:  0.8188,	Adjusted R-squared:  0.7962 
# F-statistic: 36.15 on 1 and 8 DF,  p-value: 0.0003189
abline(regressionLine, col = 3)

# Klausr
# Einheiten erwähnen
# R-squared
# Circa 82% der Variation in y lässt sich durch x erklären
# corOfData ist gleich R-squared
# Da es nur einen Preditktor gibt ist die p-value der beiden Test gleich
# Generell erklärt der zweite Test, ob die erklärenden Variablen einen tatsächlichen
# Einfluss auf den Wert haben

predict(regressionLine, newdata = data.frame(reaction=c(5.5,6)))
# TODO
# Fehler, da Zeilen und Spaltenzuordnung nicht übereinstimmt
plot(regressionLine)

# Dritter plot ...Varianz ist im mittleren bereich relativ hoch
# Letzter Graph 
# hohes residuum und hohen laverage sind ein schlechtes zeichen
################################################################################
# Exercise 27
################################################################################

?trees
head(trees)
summary(trees)
boxplot(trees)

################################################################################

plot(trees$Volume~trees$Girth)
plot(trees$Volume~trees$Height)

################################################################################

lmG = lm(trees$Volume~trees$Girth)
lmH = lm(trees$Volume~trees$Height)

summary(lmG)
summary(lmH)

################################################################################

plot(trees$Volume~trees$Girth)
abline(lmG$coefficients)
plot(trees$Volume, lmG$fitted.values)

plot(trees$Volume~trees$Height)
abline(lmH$coefficients)
plot(trees$Volume, lmH$fitted.values)

################################################################################

lmB = lm(trees$Volume~trees$Girth + trees$Height)
summary(lmB)

plot(trees$Volume,lmB$fitted.values)

################################################################################

anova(lmG, lmB)
anova(lmH, lmB)

################################################################################

plot(lmB)

################################################################################

lmL = lm(log(trees$Volume) ~ log(trees$Girth) + log(trees$Height))
summary(lmL)
