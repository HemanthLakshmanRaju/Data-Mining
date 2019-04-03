library(readxl)
library(car)


#Task 3

Concrete_Slump_Test_Data <- read_excel("Concrete Slump Test Data.xlsx")
Concrete_Slump_Test_Data

#Question 1
conc <- Concrete_Slump_Test_Data[,-1]
scatterplotMatrix(conc,spread = FALSE,lty.smooth=2,main="Scatter Plot Matrix")

# We selected Slump flow as the Output variable, as we see that we can find clear relations among Slump flow and other parameters when compared to the other 2 output variables.
# We select all the 7 input variables for our model, as they all have a linear relationship with Slump flow.

#Question 2

#Using multiple linear regression
fit_MLR<-lm(SlumpFlow~Cement+Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate,data=conc)
summary(fit_MLR)

#The coefficients show the effect of a predictor variable over the outcome variable, keeping the other input variables constant.
#Altogether, the predictor variables account for 50% of the total variance.


#Using multiple linear regression with interactions
#Interactions should be created between variables with similar linear relationship
fit_LRI<-lm(SlumpFlow~Cement+Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate+FineAggregate:FlyAsh+Cement:Water,data=conc)
summary(fit_LRI)


#Using Second order Regression- multiple linear regression with interactions between every pair of variables
fit_SLR<-lm(SlumpFlow~Cement+Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate+
          Cement:Slag+Cement:FlyAsh+Cement:Water+Cement:SP+Cement:CoarseAggregate+Cement:FineAggregate+ Slag:FlyAsh+Slag:Water+Slag:SP+Slag:CoarseAggregate+Slag:FineAggregate+ FlyAsh:Water+FlyAsh:SP+FlyAsh:CoarseAggregate+FlyAsh:FineAggregate+ Water:SP+Water:CoarseAggregate+Water:FineAggregate+
          SP:CoarseAggregate+SP:FineAggregate+
          CoarseAggregate:FineAggregate, data = conc)
summary(fit_SLR)

#The predictive power of the model has improved. Adjusted R squared value has increased to 0.65.Altogether, the predictor variables account for 75% of the total variance.

#Question 3 - Regression diagnostics

#Typical approach
par(mfrow=c(2,2)) 
plot(fit_MLR)

plot(fit_LRI)

plot(fit_SLR)


#We observe that the resulting graphs are generally similar for all the three cases. 
#Residual-vs-Fitted represents linearity quality. There should be no systematic relationship between the residuals and the predicted.
#Normal Q-Q plot represents normality quality. The residual should be normally distributed with the mean “0”. The point on this graph falls on the straight 45-degree line.
#Scale-Locationrepresents Homoscedasticity. There is constant variance assumption or random band around the horizontal line.
#Residual-vs-Leverage represents information on individual observations that need attention.


#Enhanced approach
#Normality:
par(mfrow=c(1,1))
qqPlot(fit_MLR, labels=row.names(conc), id.method="identify", simulate=TRUE,main="Linear Regression Q-Q Plot")
qqPlot(fit_LRI, labels=row.names(conc), id.method="identify", simulate=TRUE,main="Linear Regression Q-Q Plot")
qqPlot(fit_SLR, labels=row.names(conc), id.method="identify", simulate=TRUE,main="Linear Regression Q-Q Plot")

#The qqPlot() function provides a more accurate method for assessing the normality assumption.
#In the case of all 3 linear regression models, all points fall close to the line and are within the confidence envelope, suggesting that the normality condition has been met.

#Plotting Studentized residuals
residualPlot = function(fit, nbreaks = 10){
  z = rstudent(fit)
  hist(z, breaks = nbreaks, freq = FALSE, xlab = "Studentized Residual", main = "Distribution of Errors") 
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)), add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y, col="red", lwd=2, lty=2)
  legend("topright", legend = c("Normal Curve", "Kernel Density Curve"), lty = 1:2, col=c("blue", "red"), cex=.7 )
} 
residualPlot(fit_MLR)
residualPlot(fit_LRI)
residualPlot(fit_SLR)

#All three models are consistent with respect to the normal distribution.

#Independence of Errors- Durbin-Watson Test
durbinWatsonTest(fit_MLR)
durbinWatsonTest(fit_LRI)
durbinWatsonTest(fit_SLR)

# The durbinWatsonTest checks the residuals for autocorrelation. When the p-value is < 0.5, the residuals are significantly correlated whereas p > 0.05 provides no evidence of correlation.
# All 3 models are significantly correlated since p>0.5.

#Linearity- Component plus residual plots
crPlots(fit_MLR)

#Any nonlinearity in any of the above graphs suggest that we may have not adequately modeled the functional form of that predictor in the regression.
#From above graphs, we can confirm that we have met the linearity assumption.

#Homoscedasticity
ncvTest(fit_MLR)
spreadLevelPlot(fit_MLR)

ncvTest(fit_LRI)
spreadLevelPlot(fit_LRI)

ncvTest(fit_SLR)
spreadLevelPlot(fit_SLR)

#All 3 models met the constant variance assumption with p = 0.6295221, 0.5573325, 0.1004604, respectively. 
#Also, in all three graphs above, there are random points about the horizontal best fit line. 
#If we had violated the assumption, we would see non-horizontal line.

#Global validation of linear model assumption
library(gvlma)
gvlma(fit_MLR)
gvlma(fit_LRI)
gvlma(fit_SLR)

#The gvlma() function provides a go/no-go test of model assumption.
#Based on the result ,SecondOrderRegression passes with all test condition. 
#Multiple linear regression with and without interaction did not pass on Link and Global Stat.

#Multicollinearity
vif(fit_MLR)
sqrt(vif(fit_MLR))>2

vif(fit_LRI)
sqrt(vif(fit_LRI))>2

vif(fit_SLR)
sqrt(vif(fit_SLR))>2

# The vif() function provides a check for multicollinearity condition using variance inflation factor. 
#If SQRT(vif) >2 it indicates a multicollinearity problem. 
#Based on the result , in all 3 models  have multicollinearity problem
#It doesn’t make any sense to drop them all,so we would keep the model as what they were and not drop any variables.


#Question 4:
#Unusual observations:
#Outliers
outlierTest(fit_MLR)
outlierTest(fit_LRI)
outlierTest(fit_SLR)

#The results of three models show no outliers. 
#However, the points 69 and 22 have the largest studentized residual and should be considered to delete in the following corrective steps.

#High Leverage points 

hat.plot<-function(fit){
  p<-length(coefficients(fit))
  n<-length(fitted(fit))
  plot(hatvalues(fit),ylim=c(0,3.2)*p/n,main="Index plot of Hat values")
  abline(h=c(2,3)*p/n,col="red",lty=2)
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))
}
hat.plot(fit_MLR)
hat.plot(fit_LRI)
hat.plot(fit_SLR)

#There are no high leverage points greater than 3 times average hat value, which would facilitate the process of deleting outliers in the following steps.

#Influential observations
#Cook's distance
Dplot=function(fit,data){ 
  cutoff<-4/(nrow(data)-length(fit$coefficients)-2)
  plot(fit,which=4,cook.levels=cutoff)
  abline(h=cutoff,lty=2,col="blue")
}
Dplot(fit_MLR,conc)
Dplot(fit_LRI,conc)
Dplot(fit_SLR,conc)

#Influence observations are observations that have a disproportionate impact on the values of the model parameters. 
#In LR, 69, 8, and 14, are those with most influence among the model parameters. 
#In LR with Iteractions, 69,49,41 are those with most influence.
#In Second Order Regression, 14, 22, 35 are those with most influence.

#Added variable plots
avPlots(fit_MLR, ask=FALSE, id.method ="identify",onepage=TRUE )
avPlots(fit_LRI, ask=FALSE, id.method ="identify",onepage=TRUE )
avPlots(fit_SLR, ask=FALSE,id.method ="identify", onepage=TRUE )

#The Added-variable plots not only identify influential observations,they also explain the impact of various variables on response variable.

#Combined information by influence plot
influencePlot(fit_MLR,id.method="identify", main="Influence Plot:MLR", sub="Circle size is proportial to Cook's Distance")
influencePlot(fit_MLR,id.method="identify", main="Influence Plot:MLR with Interactions", sub="Circle size is proportial to Cook's Distance")
influencePlot(fit_MLR,id.method="identify", main="Influence Plot:Second order", sub="Circle size is proportial to Cook's Distance")

#Items above +2 or below -2 on the vertical axis are considered outliers.Items above 0.5 on the horizontal axis have high leverage. 
#Circle size is proportional to influence.


#Corrective measures:
#Deleting outliers
#Delete record 69 since it had the largest studentized residual and Cook's distance.
ConcreteData_deleted = conc[c(-69),]
ConcreteData_deleted
fit_MLR_del<-lm(SlumpFlow ~ Cement + Slag + FlyAsh + Water + SP + CoarseAggregate + FineAggregate,data =ConcreteData_deleted)
summary(fit_MLR_del)
Dplot(fit_MLR_del, ConcreteData_deleted)

fit_LRI_del<-lm(SlumpFlow~Cement+Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate+FineAggregate:FlyAsh+Cement:Water,data =ConcreteData_deleted)
summary(fit_LRI_del)
Dplot(fit_LRI_del, ConcreteData_deleted)

#The dataset without record 69 provides better results for both models.

#Transforming variables
#Box-cox transformation to Normality
summary(powerTransform(fit_MLR_del))

#Y1 needs transformation
#MLR
Concrete_MLR_del_Trans<- ConcreteData_deleted
Concrete_MLR_del_Trans
Concrete_MLR_del_Trans[,1]<-Concrete_MLR_del_Trans[,1]^1.7071
Concrete_MLR_del_Trans
fit_MLR_del_trans<-lm(SlumpFlow~Cement+Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate,data =Concrete_MLR_del_Trans)
summary(powerTransform(fit_MLR_del_trans))
#For 

#qqPlot(fit_MLR_del_trans, labels=row.names(fit_MLR_del_trans), id.method="identify", simulate=TRUE, main="Linear Regression Model Q-Q Plot")


#LRI
summary(powerTransform(fit_LRI_del))

#Y1 needs transformation
Concrete_LRI_Trans <-ConcreteData_deleted
Concrete_LRI_Trans
Concrete_LRI_Trans[,1]<-Concrete_LRI_Trans[,1]^1.695
Concrete_LRI_Trans
fit_LRI_del_trans<-lm(SlumpFlow~Cement+Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate+FineAggregate:FlyAsh+Cement:Water,data =Concrete_LRI_Trans)
#qqPlot(fit_LRI_del_trans, labels=row.names(fit_LRI_del_trans), id.method="identify", simulate=TRUE, main="Linear Regression with Interaction Q-Q Plot")
summary(powerTransform(fit_LRI_del_trans))

#For lambda = 1, pval=0.999. Y1 conforms to normality



#SLR
summary(powerTransform(fit_SLR))
#For lambda = 1, pval=0.12. Y1 conforms to Normality and doesn't need transformation 

#Adding or deleting variables
#Based on the result of SQRT(vif) > 2 for most of the variables multicollinearity condition is not satisfied, 
#However, we cannot deleting all variables or cannot add anything else.

#Question 5:
#Selecting the best regression model
#Comparing Mutiple linear regression, linear regression with interaction and Second order linear regression:
anova(fit_MLR, fit_LRI, fit_SLR)
#Model with least RSS value is the best- Second order linear regression is the best model.
AIC(fit_MLR, fit_LRI, fit_SLR)
#Models with smaller AIC values are preferred- Second order linear regression is the best model.

#Variable selection
#Backwards stepwise selection
library(MASS)
stepAIC(fit_SLR, direction = "backward")
#The predicted variables have been refined and AIC drops from 503.83 to 490.72
fit_SLR_Back<-lm(SlumpFlow ~ Cement + Slag + FlyAsh + Water + SP + CoarseAggregate + 
                   FineAggregate + Cement:Water + Slag:FlyAsh + Slag:Water + 
                   Slag:CoarseAggregate + Slag:FineAggregate + FlyAsh:Water + 
                   FlyAsh:CoarseAggregate + FlyAsh:FineAggregate + Water:CoarseAggregate + 
                   Water:FineAggregate,data=conc)
summary(fit_SLR_Back)
summary(fit_SLR)


#All subsets regression
library(leaps)
leaps<-regsubsets(SlumpFlow~Cement+Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate+
                    Cement:Slag+Cement:FlyAsh+Cement:Water+Cement:SP+Cement:CoarseAggregate+Cement:FineAggregate+ Slag:FlyAsh+Slag:Water+Slag:SP+Slag:CoarseAggregate+Slag:FineAggregate+ FlyAsh:Water+FlyAsh:SP+FlyAsh:CoarseAggregate+FlyAsh:FineAggregate+ Water:SP+Water:CoarseAggregate+Water:FineAggregate+
                    SP:CoarseAggregate+SP:FineAggregate+
                    CoarseAggregate:FineAggregate, data = conc, nbest=1)
plot(leaps, scale="adjr2")


#The model needs to be refined by reducing redundant variables from a larger group. 
#Based on the Backwards stepwise selection, the model has been refined into a simpler one and its AIC drops from 503.83 to 490.72. 
#Another approach to reselect the variables is all subset regression, either based on adjusted R-squared or on Mallow’s Cp value. 
#Comparing adjusted R-Squared of the refined model and original one, the original model is around 0.66, 
#and the refined model has 0.675. Also, the residual error is lesser for the original model.
#Therefore, it is the better model.


#    *******************--------********************


#Task 4

Forest_Fires_Data <- read_excel("Forest Fires Data.xlsx")
View(Forest_Fires_Data)
Forest_Fires_Data

#Question 2.1
Forest_Fires_Data$X.f<-factor(Forest_Fires_Data$X)
Forest_Fires_Data$Y.f<-factor(Forest_Fires_Data$Y)
Forest_Fires_Data$Month.f<-factor(Forest_Fires_Data$Month)
Forest_Fires_Data$Day.f<-factor(Forest_Fires_Data$Day)
Forest_Fires_Data$Area<-log1p(Forest_Fires_Data$Area)
ForestfiresData<-Forest_Fires_Data[,c(13,14:17,5:12)]
ForestfiresData
scatterplotMatrix(ForestfiresData,spread = FALSE,lty.smooth=2,main="Scatter Plot Matrix")
# X-Y coordinates and Month and day information have been transformed into factors
# Area has been transformed into LN(Area+1)

#The 4 categorical variables have been transformed to dummy variables by factor() method. Area has been transformed into Ln(Area+1). 
#The output data frame includes one response and twelve independent variables.
#In the scatter plot matrix, the points in four categorical variables are vertically arranged and don’t show much correlation with the outcome variable.

#Question 2.2:
#Use regression models:

#STFWI - Spacial, temporal and WI attributes
FF_fit_STFWI = lm(Area ~ X.f + Y.f + Month.f + Day.f + FFMC + DMC + DC + ISI, data=ForestfiresData) 
summary(FF_fit_STFWI)
#STM- Spacial, temporal and M( weather conditions)
FF_fit_STM = lm(Area ~ X.f + Y.f + Month.f + Day.f + Temp + RH + Wind + Rain, data=ForestfiresData) 
summary(FF_fit_STM)
#FWI- Fire Weather Index
FF_fit_FWI = lm(Area ~ FFMC + DMC + DC + ISI, data=ForestfiresData)
summary(FF_fit_FWI)
#Model M- uses the four weather conditions
FF_fit_M = lm(Area ~ Temp + RH + Wind + Rain, data=ForestfiresData)
summary(FF_fit_M)

#Interpretation:
#There are four algorithms mentioned in the paper- STFWI, STM, FWI, M that have been used. 
#In terms of spatial and temporal variables lack of linearity as shown

#Question 3.1:
#Regression diagnostics:
#Typical Approach
par(mfrow=c(2,2)) 
plot(FF_fit_STFWI) 
plot(FF_fit_STM)
# Point 421 and 517 are highly leveraged that should be considered for deletion in the corrective step #
plot(FF_fit_FWI) 
plot(FF_fit_M)

#Interpretation:
#In the normal Q-Q plot, there are too many points that deviate from the 45 degree diagonal- the dataset violates the normality assumption. 
#In the residual VS fitted plot, the regression curve is flat.
#In the Scale-Location plot, data points are not randomly around the horizontal line, but align along a convex curve that suggests the dataset doesnt have homoscedasticity.
#The Residuals VS Leverage plot identifies several high-leveraged points.


#Enhanced Approach
#Normality
#Enhanced Q-Q Plot
par(mfrow=c(1,1))
qqPlot(FF_fit_STFWI,labels=row.names(ForestfiresData), id.method="identify", simulate=TRUE,
       main="Linear Regression Model Q-Q Plot")
qqPlot(FF_fit_STM,labels=row.names(ForestfiresData), id.method="identify", simulate=TRUE,
       main="Linear Regression Model Q-Q Plot")
qqPlot(FF_fit_FWI,labels=row.names(ForestfiresData), id.method="identify", simulate=TRUE,
       main="Linear Regression Model Q-Q Plot")
qqPlot(FF_fit_M,labels=row.names(ForestfiresData), id.method="identify", simulate=TRUE,
       main="Linear Regression Model Q-Q Plot")

#Interpretation:
#Besides the four dummy variables, ISI and FFMC don’t have linearity with respect to the area.

# Studentized Residuals Distribution 
residualPlot = function(fit, nbreaks = 10){
  z = rstudent(fit)
  hist(z, breaks = nbreaks, freq = FALSE, xlab = "Studentized Residual", main = "Distribution of Errors") 
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)), add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y, col="red", lwd=2, lty=2)
  legend("topright", legend = c("Normal Curve", "Kernel Density Curve"), lty = 1:2, col=c("blue", "red"), cex=.7 )
} 

#Interpretation:
#we can clearly see the dependent variable is very much left skewed, though it has been already transformed by log Area.

# Since STFWI and STM contain dummy variables, the normal and Kernel Density curve don't apply to them.
residualPlot(FF_fit_FWI)
residualPlot(FF_fit_M)
#The two models are consistent with respect to the normal distribution

#Indepedence of Errors
#Durbin-Watson Test 
durbinWatsonTest(FF_fit_STFWI) 
durbinWatsonTest(FF_fit_STM) 
durbinWatsonTest(FF_fit_FWI) 
durbinWatsonTest(FF_fit_M)

# The durbinWatsonTest checks the residuals for autocorrelation. When the p-value is < 0.5, the residuals are significantly correlated whereas p > 0.05 provides no evidence of correlation.
# All 4 models have autocorrelation feature.

#Linearity- Component plus residual plots
crPlots(FF_fit_STFWI) 
crPlots(FF_fit_STM) 
crPlots(FF_fit_FWI) 
crPlots(FF_fit_M)
#Any nonlinearity in any of the above graphs suggest that we may have not adequately modeled the functional form of that predictor in the regression.
#From above graphs, we can confirm that we have met the linearity assumption.

#Homoscedasticity
ncvTest(FF_fit_STFWI)
spreadLevelPlot(FF_fit_STFWI)

ncvTest(FF_fit_STM)
spreadLevelPlot(FF_fit_STM)

ncvTest(FF_fit_FWI)
spreadLevelPlot(FF_fit_FWI)

ncvTest(FF_fit_M)
spreadLevelPlot(FF_fit_M)

#In all the graphs, there are random points about the horizontal best fit line. 
#If we had violated the assumption, we would see non-horizontal line.


# Global validation of linear model assumption 
library(gvlma)
gvlma(FF_fit_STFWI)
gvlma(FF_fit_STM)
gvlma(FF_fit_FWI)
gvlma(FF_fit_M)

#The spread level plot has a positively correlated curve.
# which means the variances increases as the variables increase.

# Multicollinearity 
vif(FF_fit_STFWI) 
sqrt(vif(FF_fit_STFWI))>2 

vif(FF_fit_STM) 
sqrt(vif(FF_fit_STM))>2 

vif(FF_fit_FWI) 
sqrt(vif(FF_fit_FWI))>2 

vif(FF_fit_M) 
sqrt(vif(FF_fit_M))>2 


#Only the DC variable has the problem of Multicolinearity


#Question 2.4:
# Identify unusual observations and take corrective measures

#Outlier Test 

outlierTest(FF_fit_STFWI)
outlierTest(FF_fit_STM)
outlierTest(FF_fit_FWI)
outlierTest(FF_fit_M)

#The results of these models show that points 239 and 416 are outliers.

hat.plot<-function(fit){
  p<-length(coefficients(fit))
  n<-length(fitted(fit))
  plot(hatvalues(fit),main="Index plot of Hat values")
  abline(h=c(2,3)*p/n,col="red",lty=2)
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))
}
hat.plot(FF_fit_STFWI)
hat.plot(FF_fit_STM)
hat.plot(FF_fit_FWI)
hat.plot(FF_fit_M)

#Influential Observations
#Cook's distance
Dplot=function(fit,data){ 
  cutoff<-4/(nrow(data)-length(fit$coefficients)-2)
  plot(fit,which=4,cook.levels=cutoff)
  abline(h=cutoff,lty=2,col="blue")
}
Dplot(FF_fit_STFWI,Forest_Fires_Data)
Dplot(FF_fit_STM,Forest_Fires_Data)
Dplot(FF_fit_FWI,Forest_Fires_Data)
Dplot(FF_fit_M,Forest_Fires_Data)


#Added variable plots
avPlots(FF_fit_STFWI, ask=FALSE, id.method ="identify",onepage=TRUE )
avPlots(FF_fit_STM, ask=FALSE, id.method ="identify",onepage=TRUE )
avPlots(FF_fit_FWI, ask=FALSE,id.method ="identify", onepage=TRUE )
avPlots(FF_fit_M, ask=FALSE,id.method ="identify", onepage=TRUE )


#Combined information by influence plot
influencePlot(FF_fit_STFWI,id.method="identify", main="Influence Plot 1", sub="Circle size is proportial to Cook's Distance")
influencePlot(FF_fit_STM,id.method="identify", main="Influence Plot 2", sub="Circle size is proportial to Cook's Distance")
influencePlot(FF_fit_FWI,id.method="identify", main="Influence Plot 3", sub="Circle size is proportial to Cook's Distance")
influencePlot(FF_fit_M,id.method="identify", main="Influence Plot 4", sub="Circle size is proportial to Cook's Distance")


#Corrective measures:
#Deleting outliers
Forest_Fires_Data_Deleted = Forest_Fires_Data[c(-105, -239, -305, -416, -421, -517, -472, -480, -500),]
Forest_Fires_Data_Deleted
FF_fit_STFWI_Deleted = lm(Area ~ X.f + Y.f + Month.f + Day.f + FFMC + DMC + DC + ISI, data=Forest_Fires_Data_Deleted) summary(FF_fit_STFWI_Deleted)
D.plot(Model_STFWI_Deleted, Model_STFWI_Deleted)

FF_fit_STM_Deleted = lm(Area ~ X.f + Y.f + Month.f + Day.f + Temp + RH + Wind + Rain, data=Forest_Fires_Data_Deleted) summary(FF_fit_STM_Deleted)

FF_fit_FWI_Deleted = lm(Area ~ FFMC + DMC + DC + ISI, data=Forest_Fires_Data_Deleted) summary(FF_fit_FWI_Deleted)

FF_fit_M_Deleted = lm(Area ~ Temp + RH + Wind + Rain, data=Forest_Fires_Data_Deleted) summary(FF_fit_M_Deleted)



Forest_Fires_Data_Transformed = Forest_Fires_Data_Transformed$Area+1
summary(powerTransform(ForestfireData_Trans$Area))
Forest_Fires_Data_Transformed[,1]=Forest_Fires_Data_Transformed[,1]^(-0.7143)
FF_fit_STFWI_Trans = lm(Area ~ X.f + Y.f + Month.f + Day.f + FFMC + DMC + DC + ISI, data=Forest_Fires_Data_Transformed) 
summary(powerTransform(FF_fit_STFWI_Trans))



FF_fit_STM_Trans = lm(Area ~ X.f + Y.f + Month.f + Day.f + Temp + RH + Wind + Rain, data=Forest_Fires_Data_Transformed) 
summary(powerTransform(FF_fit_STM_Trans))


FF_fit_FWI_Trans = lm(Area ~ FFMC + DMC + DC + ISI, data=Forest_Fires_Data_Transformed) 
summary(powerTransform(FF_fit_FWI_Trans))


FF_fit_M_Trans = lm(Area ~ Temp + RH + Wind + Rain, data=Forest_Fires_Data_Transformed) 
summary(powerTransform(FF_fit_M_Trans))


#Deleting outliers, performing log transformations and removing influential observations can be executed as corrective measures.

#Question 5:
#Selecting the best regression model
anova(FF_fit_STFWI_Trans, FF_fit_STM_Trans, FF_fit_FWI_Trans, FF_fit_M_Trans) 
AIC(FF_fit_STFWI_Trans, FF_fit_STM_Trans, FF_fit_FWI_Trans, FF_fit_M_Trans) 
summary(FF_fit_STM_Trans)
summary(FF_fit_M_Trans)

#Variable selection
stepAIC(FF_fit_STM_Trans, direction = "backward")
FF_fit_STM_Back = lm(formula = Area ~ X.f + Y.f + Month.f + Wind, data = ForestfireData_Trans)
# The predicted variables have been refined 

stepAIC(FF_fit_STM_Trans, direction = "forward")
FF_fit_STM_Forward = lm(formula = Area ~ X.f + Y.f + Month.f + Day.f + Temp + RH + Wind + Rain, data = ForestfireData_Trans)
# The variables have not changed.

stepAIC(FF_fit_STM_Trans, direction = "both")

#Comparing the models, the backward model is better.


#Interpretation of the results
#According to the results of the three kinds of stepwise methods, Forward, Backward and stepwise,
#The RSS of the models , both in the forward and backward direction, were pretty close.
#There was a lower AIC model for the backward method which gave it the advantage of being a better model.
#Therefore the lower AIC value of the backward model makes it the optimal model for this dataset
# The Optimal Model would be : Area ~ X.f + Y.f + Month.f + Wind
