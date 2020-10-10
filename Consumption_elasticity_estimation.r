# The purpose of this script is to estimate a multivariate regression model, which estimates the consumption elasticity of 
# the model used for the thesis. This includes OLS with fixed and random effects. 

#Packages used 

library(AER)
library(sandwich)
library(plm)
library(car)
library(lmtest)
library(dplyr)
library(ivpack)
library(strucchange)
library(olsrr)

#opening the data, attaching it to d1

Data1_thesis_1_ <- read_excel("C:/Users/Anddu/Downloads/Data1_thesis (1).xlsx")
View(Data1_thesis_1_)
d1<-Data1_thesis_1_
attach(d1)

#plotting the important variables
plot(Sales)
plot(Outlet1)
plot(LCT1)
plot(Arrest)
plot(DUI)


#correlation matrix

rmx1 <-d1[4:21]
rmx2 <- d1[26:31]
rmx3<- cbind(rmx1, rmx2)
rmx4 <- na.omit(rmx3)
cor(rmx4, y = NULL)


#Running the OLS regressions
attach(d1)
reg1 <- lm(DUI~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data = d1)
reg2 <- lm(Arrest~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data = d1)
summary(reg1)
summary(reg2)


#GVLMA model
gvmodel1<- gvlma(lm(DUI~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex))
gvmodel2<- gvlma(lm(Arrest~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex))
summary(gvmodel1)
summary(gvmodel2)


#controlling that the residuals are normally distributed


ols_test_normality(reg1)
ols_test_normality(reg2)
ols_plot_resid_hist(reg1)
ols_plot_resid_hist(reg2)


#run tests (Breusch-Pagan, Coefficient test and Durbin-Watson)

bptest(reg1, varformula = NULL, data = list())
bptest(reg2, varformula = NULL, data = list())
vcovHAC(reg1, omega = NULL, type = "HC4")
vcovHAC(reg2, omega = NULL, type = "HC4")
coeftest(reg1, df = Inf, vcov = vcovHAC(reg1, type = "HC4"))
coeftest(reg2, df = Inf, vcov = vcovHAC(reg2, type = "HC4"))
dwtest(reg1)
dwtest(reg2)


#confidence intervals for the OLS regressions

confint(reg1, level = 0.95)
confint(reg2, level = 0.95)


#Fixed//Random tests

fixed1 <- plm(DUI ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="within")
fixed2 <- plm(Arrest ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="within")
summary(fixed1)
summary(fixed2)
random1 <- plm(DUI ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="random")
random2 <- plm(Arrest ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="random")
summary(random1)
summary(random2)


#confidence intervals for the second step

confint(fixed1, level = 0.95)
confint(fixed2, level = 0.95)
confint(random1, level = 0.95)
confint(random2, level = 0.95)


#testing the models against OLS regressions with pFtest and lagrange multiplier test

pool1 <- plm(DUI ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="pooling")
pool2 <- plm(Arrest ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="pooling")
pFtest(fixed1, reg1)
pFtest(fixed2, reg2)
plmtest(pool1, type=c("bp"))
plmtest(pool2, type=c("bp"))
phtest(fixed1, random1)
phtest(fixed2, random2)


#creates regressions for the IV, first we need to establish that the IV is suitable

y1<- cbind(DUI)
y2<- cbind(Outlet1)
x1<- cbind(LCT1, Sales,FRATIO1, LogInc, Pindex)
x2 <- cbind(EL)
y2.2<- cbind(Arrest)

olsreg1 <- lm(y2~x1 + x2)
summary(olsreg1)
y2hat <- fitted(olsreg1)
olsreg2 <- lm(y1~y2hat+x1)
summary(olsreg2)

olsreg1.1 <- lm(y2.2~x1 + x2)
summary(olsreg1.1)
y2.2hat <- fitted(olsreg1.1)
olsreg2.2<- lm(y1~y2.2hat+x1)
summary(olsreg2.2)


#here I run the actual IV regression after confirming the suitability of U100 as an IV
#here ivreg 3 and 4 are the ones used, as they are confirmed as the most relevant 

ivreg1 = ivreg(DUI~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex | LCT1+Sales+FRATIO1+LogInc+Pindex+EL, data = d1)
ivreg2 = ivreg(Arrest~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex | LCT1+Sales+FRATIO1+LogInc+Pindex+EL, data = d1)
summary(ivreg1, vcov = sandwich, diagnostics = TRUE)
summary(ivreg2, vcov = sandwich, diagnostics = TRUE)

ivreg3 = ivreg(DUI~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex | Outlet1+Sales+FRATIO1+LogInc+Pindex+EL, data = d1)
ivreg4 = ivreg(Arrest~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex | Outlet1+Sales+FRATIO1+LogInc+Pindex+EL, data = d1)
summary(ivreg3, vcov = sandwich, diagnostics = TRUE)
summary(ivreg4, vcov = sandwich, diagnostics = TRUE)


#confidence intervals for IV regressions

confint(ivreg1, level = 0.95)
confint(ivreg2, level = 0.95)
confint(ivreg3, level = 0.95)
confint(ivreg4, level = 0.95)



#Running the regressions but with the smaller sample of Sales, that exludes the small and large obs

d3 <- subset(d1, Sales >= 3.2 & Sales <= 12.9)
summary(d3)

#Running the OLS regressions
attach(d1)
regx1 <- lm(DUI~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data = d3)
regx2 <- lm(Arrest~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data = d3)
summary(regx1)
summary(regx2)


#GVLMA model
gvmodelx1<- gvlma(lm(DUI~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex))
gvmodelx2<- gvlma(lm(Arrest~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex))
summary(gvmodelx1)
summary(gvmodelx2)


#controlling that the residuals are normally distributed


ols_test_normality(regx1)
ols_test_normality(regx2)
ols_plot_resid_hist(regx1)
ols_plot_resid_hist(regx2)


#run tests (Breusch-Pagan, Coefficient test and Durbin-Watson)

bptest(regx1, varformula = NULL, data = list())
bptest(regx2, varformula = NULL, data = list())
vcovHAC(regx1, omega = NULL, type = "HC4")
vcovHAC(regx2, omega = NULL, type = "HC4")
coeftest(regx1, df = Inf, vcov = vcovHAC(reg1, type = "HC4"))
coeftest(regx2, df = Inf, vcov = vcovHAC(reg2, type = "HC4"))
dwtest(regx1)
dwtest(regx2)


#confidence intervals for the OLS regressions

confint(reg1, level = 0.95)
confint(reg2, level = 0.95)


#Fixed//Random tests

fixedx1 <- plm(DUI ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="within")
fixedx2 <- plm(Arrest ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="within")
summary(fixedx1)
summary(fixedx2)
randomx1 <- plm(DUI ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="random")
randomx2 <- plm(Arrest ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="random")
summary(randomx1)
summary(randomx2)


#confidence intervals for the second step

confint(fixed1, level = 0.95)
confint(fixed2, level = 0.95)
confint(random1, level = 0.95)
confint(random2, level = 0.95)


#testing the models against OLS regressions with pFtest and lagrange multiplier test

pool1 <- plm(DUI ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="pooling")
pool2 <- plm(Arrest ~ Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex, data=d1, index=c("Municipalities", "Year"), model="pooling")
pFtest(fixed1, reg1)
pFtest(fixed2, reg2)
plmtest(pool1, type=c("bp"))
plmtest(pool2, type=c("bp"))
phtest(fixed1, random1)
phtest(fixed2, random2)


#creates regressions for the IV, first we need to establish that the IV is suitable

yx1<- cbind(DUI)
yx2<- cbind(Outlet1)
xx1<- cbind(LCT1, Sales,FRATIO1, LogInc, Pindex)
xx2 <- cbind(EL)
yx2.2<- cbind(Arrest)

olsregx1 <- lm(yx2~xx1 + xx2)
summary(olsregx1)
yx2hat <- fitted(olsregx1)
olsregx2 <- lm(yx1~yx2hat+xx1)
summary(olsregx2)

olsregx1.1 <- lm(yx2.2~xx1 + xx2)
summary(olsregx1.1)
yx2.2hat <- fitted(olsregx1.1)
olsregx2.2<- lm(yx1~yx2.2hat+xx1)
summary(olsregx2.2)


#here I run the actual IV regression after confirming the suitability of U100 as an IV
#here ivreg 3 and 4 are the ones used, as they are confirmed as the most relevant 

ivregx1 = ivreg(DUI~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex | LCT1+Sales+FRATIO1+LogInc+Pindex+EL, data = d3)
ivregx2 = ivreg(Arrest~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex | LCT1+Sales+FRATIO1+LogInc+Pindex+EL, data = d3)
summary(ivregx1, vcov = sandwich, diagnostics = TRUE)
summary(ivregx2, vcov = sandwich, diagnostics = TRUE)

ivregx3 = ivreg(DUI~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex | Outlet1+Sales+FRATIO1+LogInc+Pindex+EL, data = d3)
ivregx4 = ivreg(Arrest~Outlet1+LCT1+Sales+FRATIO1+LogInc+Pindex | Outlet1+Sales+FRATIO1+LogInc+Pindex+EL, data = d3)
summary(ivregx3, vcov = sandwich, diagnostics = TRUE)
summary(ivregx4, vcov = sandwich, diagnostics = TRUE)


#confidence intervals for IV regressions

confint(ivregx1, level = 0.95)
confint(ivregx2, level = 0.95)
confint(ivregx3, level = 0.95)
confint(ivregx3, level = 0.95)

#End of the code
