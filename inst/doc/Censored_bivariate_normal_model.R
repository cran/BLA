## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BLA)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(aplpack)

## -----------------------------------------------------------------------------
data("soil")

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------
x<-soil$P
y<-soil$yield

#Distribution of the x variable

summastat(x) 

summastat(log(x)) 

x<-log(x) # since x required a transformation to assume it is from a normal distribution.

#Distribution of the y variable

summastat(y)

## ----eval=FALSE, echo=TRUE,fig.align='center', fig.dim=c(4,4)-----------------
#  
#  # Create a dataframe containing x and y
#  
#  df<-data.frame(x,y)
#  
#  # Input the dataframe into the bagplot() function
#  
#  bag<-bagplot(df,show.whiskers = FALSE, ylim=c(0,20),create.plot = FALSE)
#  
#  # Combine data points from "bag" and within the loop i.e. exclude the outliers
#  
#  dat<-rbind(bag$pxy.bag,bag$pxy.outer)
#  
#  # Output is a matrix, we can pull out x and y variables for next stage
#  
#  x<-dat[,1]
#  y<-dat[,2]

## ----eval=TRUE, echo=FALSE,fig.align='center', fig.dim=c(4,4)-----------------
# for vignette purpose only
x<-log(SoilP$P) # since we required a transformation
y<-SoilP$yield


## ----eval=FALSE,fig.align='center', fig.dim=c(4,4)----------------------------
#  
#  expl_boundary(x,y) # may take a few minutes to complete
#  

## ----eval=FALSE---------------------------------------------------------------
#  ?cbvn

## -----------------------------------------------------------------------------
data<-data.frame(x,y) #This is an input dataframe containing the variables

## ----eval=FALSE---------------------------------------------------------------
#  plot(x,y)
#  
#  startValues("lp")

## -----------------------------------------------------------------------------
mean(x) 
mean(y) 
sd(x) 
sd(y) 
cor(x,y)

#The parameters of the boundary line and the data can be combined in a #vector start in the order 
#start<-c(intercept, slope, max response, mean(x), mean(y), sd(x), sd(y), cor(x,y))

## ----eval=FALSE---------------------------------------------------------------
#  ?ble_profile

## ----eval=FALSE,fig.align='center', fig.dim=c(4,4)----------------------------
#  
#  sigh=c(0.5,0.7,0.8)
#  ble_profile(data,start,sigh,model = "lp") # may take a few minutes to run for large datasets

## ----eval=TRUE,fig.align='center', fig.dim=c(4,4)-----------------------------
start<-c(4,3,13.6,3,9,0.50,1.9,0.05)

model1<-cbvn(data,start=start,sigh=0.7,model = "lp", xlab=expression("P /log ppm"), ylab=expression("Yield /t ha"^{-1}), pch=16, col="grey")

model1

## ----eval=FALSE---------------------------------------------------------------
#  ?predictBL

## ----eval=TRUE----------------------------------------------------------------

xp<-log(soil$P) # let xp be the P content in our dataset
xp[which(is.na(xp)==T)]<-mean(xp,na.rm=T)
P<-predictBL(model1,xp)

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------
x<-soil$pH
y<-soil$yield

# Distribution of the x variable

summastat(x) 


## ----eval=FALSE, echo=TRUE,fig.align='center', fig.dim=c(4,4)-----------------
#  
#  # Create a dataframe of x and y which is an input into the bagplot() function
#  
#  df<-data.frame(x,y)
#  
#  # Input the dataframe into the bagplot() function.
#  
#  bag<-bagplot(df,show.whiskers = FALSE, ylim=c(0,20),create.plot = FALSE)
#  
#  # Combine data points from "bag" and within the loop
#  
#  dat<-rbind(bag$pxy.bag,bag$pxy.outer)
#  
#  # Output is a matrix, we can pull out x and y variables for next stage
#  
#  x<-dat[,1]
#  y<-dat[,2]

## ----eval=TRUE, echo=FALSE----------------------------------------------------
# for vignette purpose only
x<-SoilpH$pH # since we required a transformation
y<-SoilpH$yield

## -----------------------------------------------------------------------------
data<-data.frame(x,y) #This is an input dataframe containing the variables

## ----eval=FALSE---------------------------------------------------------------
#  plot(x,y)
#  
#  startValues("lp")

## -----------------------------------------------------------------------------
mean(x) 
mean(y) 
sd(x) 
sd(y) 
cor(x,y)

#The parameters of the boundary line and the data can be combined in a #vector start in the order 
#start<-c(intercept, slope, max response, mean(x), mean(y), sd(x), sd(y), cor(x,y))

start<-c(-9,3, 13.5,7.5,9,0.68,2.3,0.12)

## ----eval=TRUE,fig.align='center', fig.dim=c(4,4)-----------------------------

model2<-cbvn(data,start=start,sigh=0.7,model = "lp", xlab=expression("pH"), ylab=expression("Yield /t ha"^{-1}), pch=16, col="grey")

model2

## ----eval=TRUE----------------------------------------------------------------

xpH<-soil$pH # let xpH be the P content in our dataset
xpH[which(is.na(xpH)==T)]<-mean(xpH,na.rm=T)
pH<-predictBL(model2,xpH)

## ----eval=FALSE---------------------------------------------------------------
#  ?limfactor

## ----eval=TRUE----------------------------------------------------------------

Limiting_factor<-limfactor(P,pH) # This produces a list of length 2 containg a vector limiting factors at each pont and the maximum predicted response in the dataset.

Limiting_factors<-Limiting_factor[[1]] 

## ----eval=TRUE,fig.align='center', fig.dim=c(4,4)-----------------------------
Lim_factors<-Limiting_factors$Lim_factor

barplot(prop.table(table(Lim_factors))*100,
        ylab = "Percentage (%)",
        xlab = "Soil property",
        col = "grey",
        ylim=c(0,90))

axis(side = 1, at = seq(0, 4, by = 1), labels = FALSE, lwd = 1, col.ticks = "white")  
axis(side = 2, lwd = 1)  

## ----eval=TRUE,fig.align='center', fig.dim=c(4,4)-----------------------------

plot(Limiting_factors$Rs, soil$yield,
     xlab="Predicted yield (ton/ha)",
     ylab="Actual yield (ton/ha)", pch=16, col="grey")

abline(h=Limiting_factor[[2]], col="blue", lty=5, lwd=1)
lines(c(min(Limiting_factors$Rs),max(Limiting_factors$Rs)), 
c(min(Limiting_factors$Rs),max(Limiting_factors$Rs)), 
col="red", lwd=2)

legend("bottomleft",legend = c("Att yield", "1:1 line"),
       lty=c(5,1), col=c("blue", "red"), lwd=c(1, 2), cex = 0.8)

