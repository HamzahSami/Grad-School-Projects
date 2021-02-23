blood.data = read.csv(file = "./Documents/blood-test.csv", header = TRUE, sep = ",")
install.packages("VGAM")
library(VGAM)

blood.data$volume <- blood.data$volume/1000

#fitting truncated negative binomial model
summary(fitted.model<- vglm(donations ~  monthslastdonation+volume+monthsfirst+donationmarch, data=blood.data, family=posnegbinomial()))

#checking model fit
intercept.only.model<- vglm(donations ~ 1, data=blood.data,family=posnegbinomial())
print(deviance<- -2*(logLik(intercept.only.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=4, lower.tail=FALSE))

#using fitted model for prediction
print(bloo <- predict(fitted.model, data.frame(monthslastdonation = 60, volume = 3, monthsfirst = 54, donationmarch = "no"),type="response"))

print(bloo)
