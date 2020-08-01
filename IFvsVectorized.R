###
# Keith Chamberalain
# Recode Examples in R
# 14-Jul-2020
###
# PURPOSE
# 1. To demonstrate recoding using non-vectorized if(), else(), 
# else if(), the vectorized case_when(), the vectorized formula 
# expression, the vectorized ifelse() and the formula object C()
# contrasts. 
#
# 2. To profile the system time differenes in each scenario.
#
# 3. To show the limitations of formula contrasts in fitting. 
###



###
# PRELIM & DECLARATIONS
###
require("dplyr")
require("car")
setwd("~/Data/Recode")



# First Run Notes:
# Dummy codes & Contrasts AUTO CASTED AS DOUBLES!!!!
# IF/ELSE/ELSE IF TOOK 20hr37min48.01s, THESE WEREN'T int()s
# ALSO: Auto object.size()=122.1Mb, not 76.3Mb as all int()s
# dc<-c(male=1, female=0) # Original casted as typeof() = double
# Use as.integer() to cast to int()
###
# Create Dummy Codes, Contrasts & Names
###
dc <-as.integer(c(1,0))
dc1<-as.integer(c(1,0,0))
dc2<-as.integer(c(0,1,0))
cc <-as.integer(c(1,-1))
cc1<-as.integer(c(-1,0,1))
cc2<-as.integer(c(-1,2,-1))
n1<-c("male", "female")
n2<-c("male","other","female")
names(dc)<-names(cc)<-n1
names(dc1)<-names(dc2)<-names(cc1)<-names(cc2)<-n2
 


###
# CREATE DATA
###
long<-rep(x=c("male","female"), times=1e6)
(len<-length(long)) # 2e6
ccode<-vector(mode="integer", length=len) # Preallocate
long2<-c(rep(c("male","other","female"), times=floor(len/3)),
         c("male","other"))
y1<-rnorm(n=len,mean=500, sd=60)
y2<-rnorm(n=len,mean=600, sd=60)
y<-vector(mode="numeric", length=len) # Preallocate
df<-data.frame(gender=long, gender2=long2, dummy_code=ccode, 
               gender2dummy1=ccode, gender2dummy2=ccode, 
               orthog_code=ccode, gender2orthog1=ccode, 
               gender2orthog2=ccode)
for(i in seq(from=1, to=len, by=2)){
  y[i]<-y1[i]
  y[i+1]<-y2[i]
}
df<-cbind(y, df) # add y
cat(unclass(object.size(df))/1024^2,"Mb")  # 76.29675 Mb




###
# SAVE to file for easy access later.
# CLEANUP & RELOAD.
###
ndf<-names(df)
whatList<-as.list(ndf) # for scan()
whatList[[1]]<-numeric()
whatList[[2]]<-whatList[[3]]<-character()
for(i in 4:9){
  whatList[[i]]<-integer()
}
names(whatList)<-ndf

### PROFILE write.csv()
system.time(
  write.csv(x=df, file="Recode.csv", row.names=FALSE),
  gcFirst=TRUE) # 11.3s

### PROFILE write.table()
system.time(
  write.table(x=df, file="Recode.csv", quote=FALSE, sep=",", 
              row.names=FALSE, col.names=TRUE),
  gcFirst=TRUE) # 11.0s

### CLEANUP
rm(list=ls()) # cleanup environment/start over
gc() # garbage collection




###
# REREAD DATA FROM FILE
###
### PROFILE scan() header & contents
system.time({
  header.names<-scan(file="Recode.csv", what=character(), sep=",", nlines=1)
  whatList<-as.list(header.names)
  whatList[[1]]<-numeric()
  whatList[[2]]<-whatList[[3]]<-character()
  for(i in 4:9){
    whatList[[i]]<-integer()
  }
  names(whatList)<-header.names
  df<-scan(file="Recode.csv", what=whatList, sep=",", nlines=3e6, skip=1)
  df<-data.frame(df)},
  gcFirst=TRUE) # 2.6s

###
# Object Checks
###
typeof(dc); typeof(dc1); typeof(dc2)
typeof(cc); typeof(cc1); typeof(cc2)
for (i in 1:9){
  print(paste(i, typeof(df[,i])))
}

###
# PROFILE if(), else(), else if(): unvectorized
###
cat("\nDummy Code, if/else, Unvectorized:")
i<-1
system.time(
  for(i in 1:len){
    if(df[i,"gender"]=="male"){
      df[i,"dummy_code"]<-dc["male"]
      df[i,"orthog_code"]<-cc["male"]
    }else{
      df[i,"dummy_code"]<-dc["female"]
      df[i,"orthog_code"]<-cc["female"]
    }
    if(df[i,"gender2"]=="male"){
      df[i,"gender2dummy1"]<-dc1["male"]
      df[i,"gender2dummy2"]<-dc2["male"]
      df[i,"gender2orthog1"]<-cc1["male"]
      df[i,"gender2orthog2"]<-cc2["male"]
    }else if(df[i,"gender2"]=="other"){
      df[i,"gender2dummy1"]<-dc1["other"]
      df[i,"gender2dummy2"]<-dc2["other"]
      df[i,"gender2orthog1"]<-cc1["other"]
      df[i,"gender2orthog2"]<-cc2["other"]
    }else{
      df[i,"gender2dummy1"]<-dc1["female"]
      df[i,"gender2dummy2"]<-dc2["female"]
      df[i,"gender2orthog1"]<-cc1["female"]
      df[i,"gender2orthog2"]<-cc2["female"]
    }
  },
  gcFirst = TRUE)


###
# PROFILE Case_When 
###
# Reopen data
system.time({
  df<-scan(file="Recode.csv", what=whatList, sep=",", nlines=3e6, 
           skip=1)
  df<-data.frame(df)},
  gcFirst=TRUE) # 2.6s
# Call vectorized operation 0.86s-1.2s
system.time({
  df[,"dummy_code"]<-case_when(
    df[,"gender"]  == "male"  ~ dc["male"],
    df[,"gender"]  == "female"~ dc["female"])
  df[,"gender2dummy1"]<-case_when(
    df[,"gender2"] == "male"  ~ dc1["male"],
    df[,"gender2"] == "other" ~ dc1["other"],
    df[,"gender2"] == "female"~ dc1["female"])
  df[,"gender2dummy2"]<-case_when(
    df[,"gender2"] == "male"  ~ dc2["male"],
    df[,"gender2"] == "other" ~ dc2["other"],
    df[,"gender2"] == "female"~ dc2["female"])
  df[,"orthog_code"]<-case_when(
    df[,"gender"]  == "male"  ~ cc["male"],
    df[,"gender"]  == "female"~ cc["female"])
  df[,"gender2orthog1"]<-case_when(
    df[,"gender2"] == "male"  ~ cc1["male"],
    df[,"gender2"] == "other" ~ cc1["other"],
    df[,"gender2"] == "female"~ cc1["female"])
  df[,"gender2orthog2"]<-case_when(
    df[,"gender2"] == "male"  ~ cc2["male"],
    df[,"gender2"] == "other" ~ cc2["other"],
    df[,"gender2"] == "female"~ cc2["female"]
    )},
  gcFirst = TRUE)
paste(mean(c(0.86,0.93,0.92)), "s; n=3")









###
# PROFILE Formulas
###
# Reopen the raw datafile
system.time({
  df<-scan(file="Recode.csv", what=whatList, sep=",", nlines=3e6, 
           skip=1)
  df<-data.frame(df)},
  gcFirst=TRUE) # 2.7s

system.time({
  df[,"dummy_code"]<-dc["male"]*(df[,"gender"]=="male") +
                     dc["female"]*(df[,"gender"]=="female")
  df[,"gender2dummy1"]<-dc1["male"]*(df[,"gender2"]=="male") + 
                        dc1["other"]*(df[,"gender2"]=="other") +
                        dc1["female"]*(df[,"gender2"]=="female")
  df[,"gender2dummy2"]<-dc2["male"]*(df[,"gender2"]=="male") +
                        dc2["other"]*(df[,"gender2"]=="other") +
                        dc2["female"]*(df[,"gender2"]=="female")
  df[,"orthog_code"]<-cc["male"]*(df[,"gender"]=="male") +
                      cc["female"]*(df[,"gender"]=="female")
  df[,"gender2orthog1"]<-cc1["male"]*(df[,"gender2"]=="male") + 
                         cc1["other"]*(df[,"gender2"]=="other") +
                         cc1["female"]*(df[,"gender2"]=="female")
  df[,"gender2orthog2"]<-cc2["male"]*(df[,"gender2"]=="male") +
                         cc2["other"]*(df[,"gender2"]=="other") +
                         cc2["female"]*(df[,"gender2"]=="female")},
  gcFirst = TRUE)
paste(mean(c(0.19, 0.18, 0.19)), "s; n=3")


###
# PROFILE model fits DUMMY CODES
###
system.time({
  attach(df)
  m1<-lm(y~gender)
  print(summary(m1))
  print(Anova(m1, type="III"))
  detach(df)},
  gcFirst=TRUE)
print(paste("mean using factor(dummy code) = ",
            round(mean(c(1.64,1.45,1.43)),2),"s; n=3"))#1.51s
# 
system.time({
  attach(df)
  m1b<-lm(y~dummy_code)
  print(summary(m1b))
  Anova(m1b, type="III")
  detach(df)},
  gcFirst=TRUE)
print(paste("mean using calculated(dummy code) = ",
            round(mean(c(1.39, 1.38, 1.44)),2),"s; n=3")) #1.40s
#
system.time({
  attach(df)
  m2<-lm(y~gender2)
  print(summary(m2))
  print(Anova(m2, type="III"))
  detach(df)},
  gcFirst=TRUE)
print(paste("mean using factor(3way DC) = ",
            round(mean(c(1.52, 1.49, 1.52)),2),"s; n=3"))#1.51s
print(paste("Correlation between dummy code 1 and 2 in gender2: ", 
            cor(dc1, dc2)))
#
system.time({
  attach(df)
  m2b<-lm(y~gender2dummy1+gender2dummy2)
  print(summary(m2b))
  print(Anova(m2b))
  detach(df)},
  gcFirst=TRUE)
print(paste("mean using calculated(3way DC) = ",
            round(mean(c(1.55, 1.72, 1.56)),2),"s; n=3"))#1.61s
print(paste("Correlation between dummy code 1 and 2: ", 
            cor(dc1, dc2)))

###
# PROFILE model fits: CONTRAST CODES
###
contrasts(df[,"gender"])<-sort(cc)
contrasts(df[,"gender"])
system.time({
  attach(df)
  m1<-lm(y~gender)
  print(summary(m1))
  print(Anova(m1, type="III"))
  detach(df)},
  gcFirst=TRUE)
print(paste("mean using factor(contrast) = ",
            round(mean(c(1.44,1.41,1.51)),2),"s; n=3"))#1.45s
# 
system.time({
  attach(df)
  m1b<-lm(y~orthog_code)
  print(summary(m1b))
  Anova(m1b, type="III")
  detach(df)},
  gcFirst=TRUE)
print(paste("mean using calculated(dummy code) = ",
            round(mean(c(1.39, 1.35, 1.42)),2),"s; n=3")) #1.39s
#
contrasts(df[,"gender2"])<-matrix(c(cc1[order(names(cc1))],
                                    cc2[order(names(cc2))]), 
                                  nrow=3, byrow=TRUE)
contrasts(df[,"gender2"])
system.time({
  attach(df)
  m2<-lm(y~gender2)
  print(summary(m2))
  print(Anova(m2, type="III"))
  detach(df)},
  gcFirst=TRUE)
print(paste("mean using factor(3way contrasts) = ",
            round(mean(c(1.49,1.45,1.49)),2),"s; n=3"))#1.48s
print(paste("Correlation between orthog code 1 and 2 in gender2:", 
            cor(cc1, cc2)))
#
system.time({
  attach(df)
  m2b<-lm(y~gender2dummy1+gender2dummy2)
  print(summary(m2b))
  print(Anova(m2b))
  detach(df)},
  gcFirst=TRUE)
print(paste("mean using calculated(3way DC) = ",
            round(mean(c(1.55, 1.72, 1.56)),2),"s; n=3"))#1.61s
print(paste("Correlation between dummy code 1 and 2: ", 
            cor(dc1, dc2)))
