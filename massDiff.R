##
# Mass functions provided by Keith Chamberlain
# www.ChamberlainStatistics.com
# Licensed for free and open use
# Doc Rev 2.0 10Feb18
# Revision in R-code, 1.0 27Feb18
##

##
# Function to return the PPM mass difference between
# the observed and theoretical masses after 
# conditioning the input for diff() (a wrapper to
# diff()) to be specific for masses. 
##
massDiff<-function(observed_Mass = 1E-16,
                   exact_Mass = 1E-16,
                   fitOption = 2,
                   reference_Mass = 548.5799){
  observed_Mass <- as.numeric(observed_Mass)
  exact_Mass    <- as.numeric(exact_Mass)
  reference_Mass<- as.numeric(reference_Mass)
  fitOption     <- as.numeric(fitOption)
  if(observed_Mass == 0)  observed_Mass <- 1E-16
  if(exact_Mass == 0)     exact_Mass    <- 1E-16
  if(reference_Mass == 0) reference_Mass<- 1E-16
  if(fitOption <= 2) fitOption <- 2
  if(fitOption >= 5) fitOption <- 5
  result <- diff(observed_Mass, exact_Mass, fitOption, 
                 reference_Mass)
  if(result != "Option Out of Range"){
    result * 1000000
  } else print(result)
}
# Example: Proton + Electron vs. Proton 
massDiff(1.007276466879+0.00054857990924,
         1.007276466879,4)
massDiff(1.007276466879+0.00054857990924,
         1.007276466879,2)

##
# Function to return the classical percent difference
# between the observed and expected values after 
# conditioning the input for diff() (a wrapper to diff()) 
# for option 1 of diff() only, that also handles negative
# values.
##
classicPercentDiff<-function(value1 = 1E-16, 
                             value2 = 1E-16){
  if(value1 == 0) value1<-1E-16
  if(value2 == 0) value2<-1E-16
  diff(value1, value2, 1) * 100
}

##
# namedDiff() is a wrapper for diff() that permits
# the fitOption to be named instead of referenced
# by number. Partial matching is alowed.
##
namedDiff<-function(observed = 1E-16, 
          expected = 1E-16, reference = 1E-16, 
          fitOption = c("Classic","Mass", "Log",
                        "Scaled","Chamberlain",
                        "LogScaled",
                        "LogChamberlain")){
  observed <- as.numeric(observed)
  expected <- as.numeric(expected)
  reference<- as.numeric(reference)
  fitOption<-match.arg(toupper(fitOption),
             c("CLASSIC", "MASS","LOG", "SCALED",
               "LOGSCALED","CHAMBERLAIN",
               "LOGCHAMBERLAIN"))
  if(observed==0)  observed<-1E-16
  if(expected==0)  expected<-1E-16
  if(reference==0) reference<-1E-16
  # Grab numeric option for diff() call.
  numericOption<-switch(fitOption,
                        CLASSIC    = 1,
                        MASS       = 2,
                        LOG        = 3,
                        SCALED     = 4,
                        CHAMBERLAIN= 4,
                        LOGSCALED = 5,
                        LOGCHAMBERLAIN = 5)
  diff(observed, expected, numericOption, 
       reference)
}
# Example 1: diff() by name
namedDiff(0.00054857990924+1.007276466879,
           1.007276466879,1,"LOGSCALED")

##
# The low level difference function. Minimal error 
# checking.
##
diff <- function(observed = 1E-16, expected = 1E-16, 
                 fitOption = 2, reference = 1E-16){
  fitOption<-as.numeric(fitOption)
  observed <-as.numeric(observed)
  expected <-as.numeric(expected)
  reference<-as.numeric(reference)
  numeratorDiff<- (observed - expected)
  setit = 1
  if(fitOption == 1){
    # Classic percent difference if result * 100
    reference<-mean(observed, expected)
  } else if(fitOption == 2){
    # Classic mass difference
    reference<-expected
  } else if(fitOption == 3){
    # Log difference
    return(log(observed)-log(expected))
  } else if(fitOption == 5){
    # Log Chamberlain difference
    if(numeratorDiff<0) setit <- -1
    return(setit * (log(reference+abs(numeratorDiff)) - 
                      log(reference)))
  } else if(fitOption > 5) print("Option Out of Range")
  # Chamberlain difference where reference != expected
  # otherwise, its a classic or classic mass 
  # difference.
  numeratorDiff/reference
}


##
# Low level function for solving for the reference
# needed to get a given difference.
##
invertDiff<-function(observed = 1E-16, 
                     expected = 1E-16, 
                     difference = 1E-16,
                     scale=c("PPM","Percent",
                             "None", "PPT")){
  # Check input
  scale <- match.arg(toupper(scale), 
           c("PPM","PPT","PERCENT","NONE"))
  observed  <- as.numeric(observed)
  expected  <- as.numeric(expected)
  difference<- as.numeric(difference)
  if(observed == 0) observed <- 1E-16
  if(expected == 0) expected <- 1E-16
  if(difference == 0) difference<-1E-16
  
  # Grab numerator difference
  newDiff   <- (observed - expected)
  scaledNewDiff<- switch(scale,
                         PPM  =    newDiff*1E6,
                         PPT  =    newDiff*1E9,
                         PERCENT = newDiff*100,
                         NONE =    newDiff)
  scaledNewDiff/difference
}
# Example 1: Proton + Electron vs. Proton
#            as a "difference" of 1 unit with
#            no scaling.
invertDiff(0.00054857990924+1.007276466879,
           1.007276466879,1,"None")
# Example 2: Proton + Electron vs. Proton
#            as a "difference" of 1 unit and
#            with PPM scaling.
invertDiff(0.00054857990924+1.007276466879,
           1.007276466879,1,"PPM")
# Example 3: Proton + Electron vs. Proton
#            as a "difference" of 10 units and
#            with PPM scaling.
invertDiff(0.00054857990924+1.007276466879,
           1.007276466879,10,"PPT")
