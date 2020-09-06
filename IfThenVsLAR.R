### https://drive.google.com/uc?id=1AZ-s1EgZXs4M_XF3YYEaKjjMMvRQ7-h8&export=download
# Keith Chamberalain
# Playing with Contrasts & LAR
###
require(dplyr)
# Download from URL 2 min-ish (link valid as of 05-Sep-20)
urltext <- paste("https://drive.google.com/",
                 "uc?id=1AZ-s1EgZXs4M_XF3YYEaKjjMMvRQ7",
                 "-h8&export=download", sep="")
downloaded <- FALSE
# Set to working directory of folder containing downloaded file
# < 5s
if(downloaded) urltext <- setwd("~/Data/Recode")

# Import data
system.time({
  ### Scan Header Row
  header.names<-scan(file=urltext, what=character(), sep=",", nlines=1)
  
  ### Resolve scan()'s what argument
  whatList<-as.list(header.names)
  whatList[[1]]<-numeric()
  whatList[[2]]<-whatList[[3]]<-character()
  for(i in 4:9){
    whatList[[i]]<-integer()
  }
  names(whatList)<-header.names
  
  ### run scan() to read data
  df<-scan(file=urltext, what=whatList, sep=",", nlines=3e6, skip=1)
  df<-data.frame(df)},
  gcFirst=TRUE
)
# <98s 
summary(df)
# Setup contrasts
nn<-c("Female","Male","Not-ID")
cc1<-as.integer(c(-1,1,0))
cc2<-as.integer(c(-1,-1,2))
names(cc1)<-names(cc2)<-nn


###
# Manually Set Contrasts Variables
# 1). Vectorized If/Then (here using Case/When)
###
# Pre-allocate
system.time({
  df$FvM    <- df$gender2orthog1
  df$FMvNot <- df$gender2orthog1
  df[,"FvM"]    <- case_when(df[,"gender2"]=="female" ~ cc1["Female"],
                             df[,"gender2"]=="male"   ~ cc1["Male"],
                             df[,"gender2"]=="other"  ~ cc1["Not-ID"])
  df[,"FMvNot"] <- case_when(df[,"gender2"]=="female" ~ cc2["Female"],
                             df[,"gender2"]=="male"   ~ cc2["Male"],
                             df[,"gender2"]=="other"  ~ cc2["Not-ID"])
  }, gcFirst=TRUE)
paste(mean(c(0.31,0.31,0.31)), "s; n=3")



###
# Manually Set Contrasts Variables
# 2). Vectorized LAR
###
system.time({
  df$FvM    <- df$gender2orthog1
  df$FMvNot <- df$gender2orthog1
  df[,"FvM"]   <- cc1["Female"]*(df[,"gender2"]=="female")+
                  cc1["Male"]  * (df[,"gender2"]=="male") +
                  cc1["Not-ID"]* (df[,"gender2"]=="other")
  df[,"FMvNot"]<- cc2["Female"]*(df[,"gender2"]=="female")+
                  cc2["Male"]  * (df[,"gender2"]=="male") +
                  cc2["Not-ID"]* (df[,"gender2"]=="other")
  }, gcFirst=TRUE)
paste(mean(c(0.07,0.06,0.09)), "s; n=3")
