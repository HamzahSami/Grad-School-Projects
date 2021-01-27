data <- read.csv(file = "~/Documents/csulb statistics/stat 482/housing.csv", header = TRUE, sep = ",")
housing <- data$prices



housing_prices <- c(610000,799999,814999,739000,850000,899000,779000,700000,550000,689000,925000,1300000,711000,950000,585000,620000,529000,850000,899000,1275000,835000,1449900,649000,699900,663000,674800,799999,675000,725000,1495000,899000,950000,799000,1199000,1599000,900000)
hist(log(housing_prices),col= 'orange',xlab = "Housing Prices", ylab = "Number of Houses")
mean(housing_prices)

#normally distributed
shapiro.test(log(housing_prices))




#lambda:
lambda = 46/365


#Find expected value

#Find standard deviation

#simulate 100 trajectories

