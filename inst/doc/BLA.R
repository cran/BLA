## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BLA)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(aplpack)

## ----echo=FALSE---------------------------------------------------------------
original_scipen <- getOption("scipen")
options(scipen = 999)#  disabling scientific notation i.e 1.1*10^2

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------
head(soil)


## -----------------------------------------------------------------------------
x<-soil$P
y<-soil$yield

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------
summastat(x)

summastat(y)

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------
summastat(log(x))

x<-log(x) # transforms soil P to log 

## ----eval=FALSE, echo=TRUE,fig.align='center', fig.dim=c(4,4)-----------------
#  
#  vals_ur<-matrix(NA,length(x),2) #Create a matrix with x and y as required by the bag plot function
#  vals_ur[,1]<-x
#  vals_ur[,2]<-y
#  
#  bag<-bagplot(vals_ur, ylim=c(0,20), show.whiskers =F,create.plot = TRUE) # run the bagplot function
#  legend("topright", c("bag","loop","outliers", "d.median"),
#          pch = c(15,15,16,8),col=c("blue","lightblue","red","red"),
#         cex=0.7)
#  
#  vals<-rbind(bag$pxy.bag,bag$pxy.outer) # to remove outliers, select points in the bag and loop only
#  

## ----eval=TRUE, echo=FALSE,fig.align='center', fig.dim=c(4,4)-----------------

# for vignette purpose only
x<-log(SoilP$P) # since we required a transformation
y<-SoilP$yield
vals <- data.frame(x,y)

## -----------------------------------------------------------------------------
x<-vals[,1]
y<-vals[,2]

## ----fig.align='center', fig.dim=c(5,4)---------------------------------------
bound_test<-expl_boundary(x,y,shells = 10, simulations = 100, 
                          pch=16, col="grey") # 

bound_test

## ----eval=FALSE---------------------------------------------------------------
#  ?bolides

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------
bolides(x,y,model = "explore", pch=16, col="grey")

## ----eval=FALSE---------------------------------------------------------------
#  ?startValues()

## ----eval=FALSE---------------------------------------------------------------
#  
#  startValues("trapezium") # then select the five points at the edge of the dataset that make up the trapezium model in order of increasing x values.

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------

start<-c(4,3,14,104,-22) # start values is a vector of five consists of intercept, slope, plateau yield, intercept2 and slope2. 

model1<-bolides(x,y, start = start,model = "trapezium",
                xlab=expression("Phosphorus/ln(mg L"^-1*")"), 
                ylab=expression("Yield/ t ha"^-1), pch=16, 
                col="grey", bp_col="grey")

model1


## -----------------------------------------------------------------------------

P<-c(4.5, 7.4, 12.2, 20.1, 54.5)
P_log<-log(P)

Max_Response<-predictBL(model1, P_log) # the argument inputs are the boundary line model and the independent values (in this case P_log)

Max_Response

## ----eval=FALSE---------------------------------------------------------------
#  ?blbin

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------
bins<-c(1.61,4.74,0.313) 

blbin(x,y, bins,model = "explore", tau=0.99, pch=16, col="grey")

## ----eval=FALSE---------------------------------------------------------------
#  startValues("trapezium")

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------

start<-c(4.75, 3.23, 13.3, 24.87,-2.95 )

model2<-blbin(x,y, bins,start = start,model = "trapezium", 
              tau=0.99, 
              ylab=expression("t ha"^-1), 
              xlab=expression("Phosphorus/ln(mg L"^-1*")"), 
              pch=16, col="grey", bp_col="grey")

model2

## ----eval=FALSE---------------------------------------------------------------
#  ?blqr

## ----fig.align='center', fig.dim=c(4,4), eval=FALSE---------------------------
#  
#  plot(x,y)
#  
#  startValues("trapezium")

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------

start<-c(4,3,13.5,31,-4.5)

model3<-blqr(x,y, start = start, model = "trapezium",
             xlab=expression("Phosphorus/ mg L"^-1), 
             ylab=expression("Phosphorus/ln(mg L"^-1*")"),
             pch=16,tau=0.99, col="grey") # may take a few seconds to ran

model3

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------
bolides(x,y,model="explore", pch=16, col="grey")

## -----------------------------------------------------------------------------
custom_function<-function(x,a,b,c){
  y<- a - b*(x-c)^2
}

## ----fig.align='center', fig.dim=c(4,4)---------------------------------------
start<-c(13.5,3,3.3)  

model4<-bolides(x,y, start = start,model = "other",
                equation=custom_function,
                xlab=expression("Phosphorus/mg L"^-1), 
                ylab=expression("Phosphorus/ln(mg L"^-1*")"), 
                pch=16, ylim=c(3.8,14.5), col="grey",bp_col="grey")

model4

## ----echo=FALSE---------------------------------------------------------------
options(scipen = original_scipen)# Restore the original scipen value

