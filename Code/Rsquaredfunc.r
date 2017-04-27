library(vars)
library(RCurl)

TopImplicit <- read.csv(text=getURL("https://raw.githubusercontent.com/ckuras/Wiki_VAR_Analysis/master/Data/TopImplicit.csv"))
TopImplicit$FileId <- factor(TopImplicit$FileId)

# function for returning individual VAR results with a max lag of 4 weeks determined by the AIC test 
VARS <- function(FID=221, start=2001, end=2016, freq=52, Lag=1, lagmax=4){
  
  ID <- subset(TopImplicit, FileId == FID, select=c(Date, Page, Talk))
  
  # ID221 as time series with weekly frequency 
  PageEdits <- ts(ID$Page, start=c(2001, 1), freq=52)
  Talk <- ts(ID$Talk, start=c(2001, 1), freq=52)
  
  
  #making series stationary with lagg differences
  PageD <- diff(window(PageEdits, start=c(start, 1), end=c(end, 1), frequency=freq), Lag)
  TalkD <- diff(window(Talk, start=c(start, 1), end=c(end, 1), frequency=freq), Lag)
  ts.D <- cbind(PageD, TalkD)
  
  #VAR estimation which includes trend deterministic regressors
  ts.VAR_D <- VAR(ts.D, type="none", lag.max=lagmax, ic="AIC")
  
  summary(ts.VAR_D)
}

#EXAMPLE
t <- VARS(FID=180)
t
# r squared is extracted from summary of VAR results
t[["varresult"]][[1]][["r.squared"]]
#correlation extracted
t[["corres"]][[2]]


#func that loops all wiki IDs through the VARS function and extracts the r squared from each
#Note cannot loop through entire set of 221 IDs because of 3 problem IDs shown later
r.sq <- function(lag=1, lmax=4, btmrange=2, toprange=40){
  x <- c()
  for (i in btmrange:toprange){
    v <- VARS(FID=i,Lag=lag, lagmax=lmax)
    x <- append(x, (v[["varresult"]][[1]][["r.squared"]]))
  }
  x
}

#func that loops all wiki IDs through the VARS function and extracts the correlation from each
vcorr <- function(lag=1, lmax=10, btmrange=2, toprange=40){
  c <- c()
  for (i in btmrange:toprange){
    v <- VARS(FID=i,Lag=lag, lagmax=lmax)
    c <- append(c, (v[["corres"]][[2]]))
  }
  c
}

#Problem IDs that will be ommitted from final results
VARS(FID = 1)
VARS(FID = 41)
VARS(FID = 131)

#func for applying r.sq or vcorr to the TopImplicit set without the problem IDs
func <- function(func=r.sq){
  x <- func(btmrange = 2, toprange = 40)
  y <- func(btmrange = 42, toprange = 130)
  z <- append(x, y)
  a <- func(btmrange = 132, toprange = 221)
  r.sqr <- append(z, a)
}

#vector of VAR r squared results from 218 wiki pages
rsqr.vector <- func(r.sq)
rsqr.vector
summary(rsqr.vector)
boxplot(rsqr.vector)

#vector of VAR correlation results from 218 wiki pages
corr.vector <- func(vcorr)
corr.vector
summary(corr.vector)





