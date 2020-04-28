# Stony Brook University
# Graduate Department of Economics
# ECO522 Econometrics
# 2016 Fall Semester
# Hua Shi
# SBU ID#110635839

#==============================================================================================================
#=================================================================================================================
rm(list=ls()) # clear console - control + L
help(logbin)
library(logging) #it offers hierarchic loggers, multiple handlers per longger and so on.
library(plm) # it intends to make the estimation of linear panel models.
library(foreign)# allows you to import data files from some statistical software packages.
library(xtable) # generate nicely formated tables for LaTex or HTML
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
library(car) #Companion to applied regression
library(lmtest) # test linear regression models
library(stargazer) # Well-formatted regression and summary statistics tables
library(gplots) # plotting data 
require(urca) # for time-series data to test unit root test and cointegrarion test 
# if there is unite root, that means the coefficient is 1, variance of yt is depends on t. 
#it is not stationary time series.the moments of the stochastic process depends on t.
library(MASS)# For commmand confint() and it computes confidence intervals for  parameters in a fitted model.
library(ggplot2) # create elegant data visualisation using the grammar of graphics
library(lattice)# trellis graphics for R
library(reshape)# reshape data flexibly
library(splm) # Econometrix Models for Spatial panel data

#=================================================================================================================
#=================================================================================================================

# input data 
mydata<-read.csv("/Users/yingyuxuan/Desktop/R/asian39countriespdata.csv",header = TRUE)
#mydata<-read.csv("/Users/yingyuxuan/Desktop/R/asian17countriespdata.csv",header = TRUE)
attach(mydata)

#=================================================================================================================
#=================================================================================================================

# Variables  
#C - CO2 emissions (kg per PPP $ of GDP)
#E-Energy use (kg of oil equivalent per capita)*
#Y - GDP per capita
#PD - Population density (people per sq. km of land area)	
#X - Forest area % of total land 

#=================================================================================================================
#=================================================================================================================
# Exploring Panel Data

#Plot the trend of the CO2 emission during 1996-2013 for East Asia 
EastAsiadata<-read.csv("/Users/yingyuxuan/Desktop/R/Five-Parts-of-Asia/EastAsia.csv",header = TRUE)
scatterplot(C~year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "CO2 emission per capita",data=EastAsiadata)
#Plot the trend of the CO2 emission during 1996-2013 for Southeast Asia
SoutheastAsiadata<-read.csv("/Users/yingyuxuan/Desktop/R/Five-Parts-of-Asia/SoutheastAsia.csv",header = TRUE)
scatterplot(C~year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "CO2 emission per capita",data=SoutheastAsiadata)
#Plot the trend of the CO2 emission during 1996-2013 for South Asia
SouthAsiadata<-read.csv("/Users/yingyuxuan/Desktop/R/Five-Parts-of-Asia/SouthAsia.csv",header = TRUE)
scatterplot(C~year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "CO2 emission per capita",data=SouthAsiadata)
#Plot the trend of the CO2 emission during 1996-2013 for West Asia
WestAsiadata<-read.csv("/Users/yingyuxuan/Desktop/R/Five-Parts-of-Asia/WestAsia.csv",header = TRUE)
scatterplot(C~year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "CO2 emission per capita",data=WestAsiadata)

CentralAsiadata<-read.csv("/Users/yingyuxuan/Desktop/R/Five-Parts-of-Asia/CentralAsia.csv",header = TRUE)
scatterplot(C~year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "CO2 emission per capita",data=CentralAsiadata)

#=================================================================================================================
#=================================================================================================================

#Plot the trend of the GDP per Capita during 1996-2013 for East Asia
scatterplot(Y~year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "GDP per capita", data=EastAsiadata)
#Plot the trend of theGDP per Capita during 1996-2013 for Southeast Asia
scatterplot(Y~year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "GDP per capita",data=SoutheastAsiadata)
#Plot the trend of the GDP per Capita during 1996-2013 for South Asia
scatterplot(Y~year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "GDP per capita",data=SouthAsiadata)
#Plot the trend of the GDP per Capita during 1996-2013 for West Asia
scatterplot(Y~year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "GDP per capita",data=WestAsiadata)
#Plot the trend of the GDP per Capita during 1996-2013 for Central Asia
scatterplot(Y~year|Country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "GDP per capita",data=CentralAsiadata)
#=================================================================================================================
#=================================================================================================================

#Estimation of the basic models with plm: 
#Several models can be estimated with plm by filling the model argument:
#1#􏰀 the pooling model (pooling)
#3#􏰀 the fixed effects model (within),
#5# the error components model (random).

#=================================================================================================================
#=================================================================================================================

#Set data as panel data
pdata<-plm.data(mydata,index=c("country","year"))

lapply(pdata[c(3,4,5,6,7)], function(x) 
                            rbind( mean = mean(x) ,
                             sd = sd(x) ,
                             median = median(x) ,
                             varance = var(x),
                             minimum = min(x) ,
                             maximum = max(x) ) )

stargazer((pdata[c(3,4,5,6,7)]))

#=================================================================================================================
#=================================================================================================================

#Testing for unit roots/stationary

#The Dickey-Fuller test to check for stochastic trends. 
#The null hypothesis is that the series has a unit root (i.e. non-stationary)
library(tseries)

Panel.set<-plm.data(pdata,indexes = c("country","year"))

adf.test(log(Panel.set$C))

adf.test(log(Panel.set$E), k = trunc((length(log(E))-1)^(1/3)))

adf.test(log(Panel.set$Y), k= trunc((length(log(Y))-1)^(1/3)))

adf.test(log(Panel.set$X), k = trunc((length(log(X))-1)^(1/3)))

adf.test(log(Panel.set$PD), k = trunc((length(log(PD))-1)^(1/3)))  

# LLC test (unit root test )
d<-log(Panel.set[c(3,4,5,6,7)])
LLC <- purtest(d, test = "levinlin", exo = "intercept",lags = "AIC", pmax = 5)
summary(LLC)

#If p-value < 0.05 then no unit roots present 

library(lmtest)
bptest(log(C) ~ log(E)+log(Y)+log(PD)+log(X) ,data=pdata,studentize = F)

#=================================================================================================================
#=================================================================================================================

#
ooled OLS estimator:
pdata<-plm.data(mydata,index=c("country","year"))
pooling<-plm(log(C) ~ log(E)+log(Y)+log(PD)+log(X) ,data=pdata,model="pooling")
summary(pooling)    # Generate the resulte of pooled model
stargazer(pooling,title="Pooled OLS Model", align=TRUE) # generate code for LaTex.

#=================================================================================================================

#Random effects estimator :
random<-plm(log(C) ~ log(E)+log(Y)+log(PD)+log(X) ,data=pdata,index=c("country","year"),model="random",random.method = "swar")
summary(random)
stargazer(random,title="Random Effects Model", align=TRUE)

#=================================================================================================================

#Fixed effects estimator (within estimator):
fixed<-plm(log(C) ~ log(E)+log(Y)+log(X)+log(PD),data=pdata,index = c("country","year"),model="within")
summary(fixed)
stargazer(fixed, title="Fixed Effects Model", align=TRUE)


##=================================================================================================================
#=================================================================================================================


# Testing for fixed effects, Null: Pooling OSL estimator is bettern than Fixed estimator
pFtest(fixed,pooling)
#p-value < 2.2e-16 which means that my variables are 
#super significant. 2.2e-16 is 2.2 to the power of -16, it is very samll number 
#and it is smaller than 0.05.
#so the fixed model is  a better choice

#=================================================================================================================
#ausman test for fixed versus random effects model
phtest(fixed,random)

# if p-value is < 0.05 then use fixed effect,if >0.05 then use random effect
# The result shows that p-value = 0.5418 which is bigger that 0.05 ,
# then we use random model.
#=
================================================================================================================
#=================================================================================================================


serageco2adata<-read.csv("/Users/yingyuxuan/Desktop/R/Five-Parts-of-Asia/averageco2ofasia.csv",header = TRUE)

scatterplot(AC~Year|Region, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "CO2 emission per capita",data=averageco2adata)
scatterplot(AE~Year|Region, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "Energy Use per capita",data=averageco2adata)
scatterplot(AY~Year|Region, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "GDP per capita",data=averageco2adata)
scatterplot(AX~Year|Region, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "Forest Area",data=averageco2adata)
scatterplot(APD~Year|Region, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "Population Density",data=averageco2adata)

Asianadata<-read.csv("/Users/yingyuxuan/Desktop/R/Five-Parts-of-Asia/Asiansituation1996to2013.csv",header = TRUE)
par(mfrow=c(2,3))
plot(C~Year,xlim=c(1996,2013),xlab="Year",ylab="CO2 emissions per capita", type="l",data=Asianadata)
plot(Y~Year,xlim=c(1996,2013),xlab="Year",ylab="GDP per capita",  type="l",data=Asianadata)
plot(X~Year,xlim=c(1996,2013),xlab="Year",ylab="Forest area of land",  type="l",data=Asianadata)
plot(PD~Year,xlim=c(1996,2013),xlab="Year",ylab="Population Density", type="l",data=Asianadata)
plot(E~Year,xlim=c(1996,2013),xlab="Year",ylab="Energy use per capita",type="l",data=Asianadata)


