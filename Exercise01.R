# Datenanalyse und Datenmanagement
# Übung1 - 14.04.2016

################################################################################
# Aufgabe 1
################################################################################
duration_of_calls = c(8, 17, 2, 3, 31, 7, 14, 1, 1, 11)
cost_in_euro = sum(duration_of_calls)*0.49

################################################################################
# Aufgabe 2
################################################################################
calls_lt_ten_min = length(duration_of_calls[duration_of_calls>10])
number_of_calls_lt_10_min = sum(duration_of_calls>10)
calls_lt_ten_min = which(duration_of_calls>10)
mean_length_of_calls = mean(duration_of_calls)

################################################################################
# Aufgabe 3
################################################################################
data = rnorm(10, mean = 10, sd = 1)
log_data_gt_23 = log(data)>2.3
a = log_data_gt_23[2]
b = log_data_gt_23[4]
c = log_data_gt_23[8]
# test if log of observation 3,5,9 are larger than 2.3
test_result = ((a+b+c) == 3)

# rnorm returns random generation for the normal distribution 
# with mean equal to mean and standard deviation equal to sd

################################################################################
# Aufgabe 4
################################################################################
# a)
veg_data = c('Alice'=TRUE,'Bob'=FALSE,'Carol'=TRUE,'Dan'=FALSE,'Chuck'=FALSE)

# b)
veg_data['Bob']=TRUE

################################################################################
# Aufgabe 5
################################################################################
# a)
veg_data_txt = read.table("~/Dokumente/TU Clausthal/Datenanalyse und Datenmanagement/veg_data.txt", quote="\"", comment.char="")
names(veg_data_txt) = c("Name","Vegetarier")

# b)
veg_data_csv <- read.csv("~/Dokumente/TU Clausthal/Datenanalyse und Datenmanagement/veg_data.csv", header=FALSE)
names(veg_data_csv) = c("Name","Vegetarier")

# c)
# change data
veg_data_csv$Vegetarier[veg_data_csv$Name == "Bob"] = !(veg_data_csv$Vegetarier[veg_data_csv$Name == "Bob"])
veg_data_txt$Vegetarier[veg_data_txt$Name == "Bob"] = !(veg_data_csv$Vegetarier[veg_data_csv$Name == "Bob"])

# export data.frame
write.csv(veg_data_txt, "~/Dokumente/TU Clausthal/Datenanalyse und Datenmanagement/veg_data_export.txt")
write.csv(veg_data_csv, "~/Dokumente/TU Clausthal/Datenanalyse und Datenmanagement/veg_data_export.csv")

# Trying to implement changing as a function
# change_preference = function(data, column_names, entry){
#   
#   x = column_names[1]
#   y = column_names[2]
#   
#   data$y[data$y == entry] = !(data$y[data$x == entry])
#   
# }
# 
# change_preference(veg_data_csv, c("Name", "Vegetarier"), "Bob")



