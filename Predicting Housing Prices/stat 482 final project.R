data <- read.csv(file = "~/Documents/csulb statistics/stat 482/housing.csv", header = TRUE, sep = ",")
housing <- data$prices

hist(log(housing_prices),col= 'orange',xlab = "Housing Prices", ylab = "Number of Houses")

#Expected Value and Standard Deviation
house_avg = mean(housing)
house_var = var(housing)

#Verify if data is normally distributed
shapiro.test(log(housing_prices))

#Given total homes sold, designate this as lambda.
lambda = 46/365

#Simulate trajectories for an entire year.
t<- 365
lambda<- 46/61

#generating N(t)
set.seed(12345)
Nt<- rpois(1,lambda*t)

#generating N(t) standard uniforms
unif<- 1:Nt
for(i in 1:Nt)
  u[i]<- runif(1) 

#sorting standard uniforms
sort_unif<- sort(unif)

#computing N(t) event times
events<- t* sort_unif

#simulating trajectory for one year (365 days)
time<- 1:2*Nt
profit<- 1:2*Nt
time[1]<- 0
profit[1]<- 0
for (i in seq(2, 2*Nt, 2)) {
  time[i]<- events[i/2]-0.001
  time[i+1]<- events[i/2]
  profit[i]<- profit[i-1]
  profit[i+1]<- profit[i-1]+(abs(rnorm(1)))
}

#plotting simulated trajectory
plot(time, profit, type='l', col=4, panel.first=grid())

#estimated number of homes sold within a year
Nt

#Estimated gross profit of homes sold 
profit[length(profit)]
