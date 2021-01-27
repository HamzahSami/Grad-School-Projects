#code for final project

x1 = data$X1
x2 = data$X2
x3 = data$X3
x4 = data$X4
x5 = data$X5
x6 = data$X6
x7 = data$X7
x8 = data$X8

y1 = data$Y1

install.packages("ggplot2")
library(ggplot2)
install.packages("broom")
library(broom)
install.packages("GGally")
library(GGally)
install.packages("tidyverse")
library(tidyverse) 

mod0 <- lm(y1~1)
add1(mod0, ~.+x1+x2+x3+x4+x5+x6+x7+x8, test = 'F')
#Because of large F value and small p-value, we use x5

#choose second predictor with x5 already in model.
mod1 = update(mod0, ~.+x5)
summary(mod1)
add1(mod1, ~.+x1+x2+x3+x4+x6+x7+x8, test = 'F')

#so we use x7 as second predictor.
mod2 = update(mod1, ~.+x7)
summary(mod2)
add1(mod2, ~.+x1+x2+x3+x4+x6+x8, test = 'F')

#pick x3 as third predictor.
mod3 = update(mod2, ~.+x3)
summary(mod3)

add1(mod3, ~.+x1+x2+x4+x6+x8, test = 'F')

#pick x1 as fourth predictor
mod4 = update(mod3, ~.+x1)
summary(mod4)
add1(mod4, ~.+x2+x4+x6+x8, test = 'F')

#since x2 and x3 are exactly the same.  They are the fifth and sixth predictors.
mod5 = update(mod4, ~.+x2+x4)
summary(mod5)
#However when we look at the summary of the model, we see that x4 is no longer significant.

#only include x2 in the model. Remove x4
mod5_new = update(mod4,~.+x2)

#test x6 and x8.
add1(mod5_new, ~.+x6+x8, test = 'F')

#add x8 into model. X6 is not significant
mod6 = update(mod5_new, ~.+x8)
summary(mod6)

#test x6 in model
add1(mod6, ~.+x6, test = 'F')
#not significant so do not include in model.

#Starting model: y1 ~ x5 + x7 + x3 + x1 + x2 + x8

#stepwise regression with AIC
mod0 = lm(y1 ~ 1)
mod.upper = lm(y1 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8)
step(mod0, scope = list(lower = mod0, upper = mod.upper))

#best-subsets regression
install.packages("leaps")
library(leaps)
mod = regsubsets(cbind(x1,x2,x3,x4,x5,x6,x7,x8), y1)
summary.mod = summary(mod)

summary.mod$which

summary.mod$adjr2 

summary.mod$cp

#the model with the highest adjusted R^2 value and lowest Mallow’s Cp value is 
#the model with six predictors. From looking at the which portion of the summary() function, 
#we can see that the predictors excluded from the model are x4 and x6.

starting_model <- lm(y1~x1+x2+x3+x5+x7+x8)
#correlation for starting model
ggpairs(subset(data, select = -X4 -X6))

#residual vs fit plot for starting model

starting_model %>% augment() %>%
  ggplot(., aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept = 0, col=4) +
  labs(x = 'Fitted Values', y = 'Residuals') + ggtitle("Residual vs Fit")

#normality condition
shapiro.test(starting_model$residuals) 

#Plot each predictor against the residual

plot(x1,final_modelh$residuals)
#Residual vs x1

plot(x2,final_modelh$residuals)
#Residual vs x2

plot(x3,final_modelh$residuals)
#Residual vs x3

plot(x5,final_modelh$residuals)
#Residual vs x5

plot(x7,final_modelh$residuals)
#Residual vs x7

plot(x8,final_modelh$residuals)
#residual vs x8

#Now that we know which terms to transform, we use a quadratic transform
#linearity condition
q_model <- lm(y1~x1+x2+x3+x5+x7+x8+I(x1^2)+I(x2^2)+I(x3^2)+I(x8^2)) 

q_model %>% augment() %>%
  ggplot(., aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept = 0, col=4) +
  labs(x = 'Fitted Values', y = 'Residuals') + ggtitle("Residual vs Fit")

#normality condition
shapiro.test(q_model$residuals) 

#Box-CoxTransform
Install.packages(“MASS”)
Library(MASS) 
bc = boxcox(q_model)
lambda.opt = bc$x[which.max(bc$y)]
lambda.opt

updated_model <- lm(log(y1)~x1+x2+x3+x5+x7+x8+I(x1^2)+I(x2^2)+I(x3^2)+I(x8^2))
#residual vs fit plot for updated model
updated_model %>% augment() %>%
  ggplot(., aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept = 0, col=4) +
  labs(x = 'Fitted Values', y = 'Residuals') + ggtitle("Residual vs Fit")

#normality condition
shapiro.test(updated_model$residuals) 

updated_model <- lm(log(y1)~x1+x2+x3+x5+x7+x8+I(x1^2)+I(x2^2)+I(x3^2)+I(x8^2))
add1(updated_model, ~.+(x1*x2)+(x1*x3)+(x1*x5)+(x1*x7)+(x1*x8)+(x2*x3)+(x2*x5)+(x2*x7)+(x2*x8)+(x3*x5)+(x3*x7)+(x3*x8)+(x5*x7)+(x5*x8)+(x7*x8), test = 'F')

#step1
final1 = update(updated_model, ~.+(x2*x5)+(x3*x5))
summary(final1)

final2 = lm(log(y1)~x1+x2+x3+x5+x7+x8+I(x3^2)+I(x8^2)+(x2*x5))
add1(final2, ~.+(x1*x2)+(x1*x3)+(x1*x5)+(x1*x7)+(x1*x8)+(x2*x3)+(x2*x7)+(x2*x8)+(x3*x7)+
       (x3*x8)+(x5*x7)+(x5*x8)+(x7*x8))

final3 = update(final2,~.+(x7*x8))
summary(final3)

add1(final3,~.+(x1*x2)+(x1*x3)+(x1*x5)+(x1*x7)+(x1*x8)+(x2*x3)+(x2*x7)+(x2*x8)+(x3*x7)+(x3*x8)+(x5*x7)+(x5*x8))
final4 = update(final3,~.+(x3*x7))
summary(final4)

add1(final4,~.+(x1*x2)+(x1*x3)+(x1*x5)+(x1*x7)+(x1*x8)+(x2*x3)+(x2*x7)+(x2*x8)+(x3*x8)+(x5*x7)+(x5*x8))
final5 = update(final4,~.+(x2*x7))
summary(final5)

add1(final5,~.+(x1*x2)+(x1*x3)+(x1*x5)+(x1*x7)+(x1*x8)+(x2*x3)+(x2*x8)+(x3*x7)+(x3*x8)+(x5*x7)+(x5*x8))
final6 = update(final5, ~.+(x1*x5))
summary(final6)

add1(final6,~.+(x1*x2)+(x1*x3)+(x1*x7)+(x1*x8)+(x2*x3)+(x2*x8)+(x3*x7)+(x3*x8)+(x5*x7)+(x5*x8))
final7 = update(final6, ~.+(x1*x2))
summary(final7)

final_model <- lm(log(y1)~x1+x2+x3+x5+x7+x8+I(x1^2)+I(x2^2)+I(x3^2)+I(x8^2)+(x1*x7)+(x3*x7))

final_model %>% augment() %>%
  ggplot(., aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept = 0, col=4) +
  labs(x = 'Fitted Values', y = 'Residuals') + ggtitle("Residual vs Fit")

shapiro.test(final_model$residuals)

summary(final_model)

anova(final_model)

#answer for question 1
confint(final_model)

#answer for question 2 

red_model <- lm(log(y1)~x1+x2+x3+x5+x7+x8+I(x1^2)+I(x2^2)+I(x3^2)+I(x8^2)+(x3*x7))
final_model <-lm(log(y1)~x1+x2+x3+x5+x7+x8+I(x1^2)+I(x2^2)+I(x3^2)+I(x8^2)+(x1*x7)+(x3*x7))
anova(red_model,final_model)

#answer for question 3
hv = hatvalues(final_model)
which(hv == max(hv))

which(hv >= 39/769)

dist = cooks.distance(final_model)
hist(dist)






