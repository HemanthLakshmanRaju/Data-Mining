library(readxl)
library(car)
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