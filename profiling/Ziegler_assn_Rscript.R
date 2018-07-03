rm(list=ls())

getwd()
setwd("$/Downloads")
min_2009=read.csv("IBM.AssPan.09PUdataset.csv",header=T)
str(min_2009)
min_2009=min_2009[min_2009$pop==27,]
str(min_2009)

min_2010=read.csv("IBM.AssPan.Ky21.10PUdataset.csv",header=T)
str(min_2010)
min_2010=min_2010[min_2010$pop==27,]
str(min_2010)

head(min_2010)
tail(min_2010)

#write.csv(min_2009,file="$/Box/Ziegler_Assn_2009_subset.csv")
#write.csv(min_2010,file="$/Box/Ziegler_Assn_2010_subset.csv")


# Changed the column name in 2009 set from entity_id to PlotID to append using rbind
#names(min_2009)
#names(min_2010)

#colnames(min_2009)[14]="PlotID"

mindata=rbind(min_2009, min_2010)

str(mindata)

attach(mindata)

mindata_sorted=mindata[order(ICP.plate,pedigree),]

head(mindata_sorted)


write.csv(mindata_sorted,"$/Desktop/Ziegler_Combined_sorted.csv")

min_com=read.csv("/Users/rkastooriramamurth2/Desktop/Ziegler_Combined_sorted.csv",header = T)

min_com=min_com[,-c(1:4,6:13,15,17)]
head(min_com)




head(min_com)
str(min_com)

names(min_com)
attach(min_com)

#test1=aggregate(min_com,by=list(sample,PlotID,pedigree), FUN="mean")


#test2=test1[,-c(4:7,9:20)]


#?aggregate
#min_mean=aggregate(min_com[,c(14,16,18:38)],list(min_com$PlotID),mean)
#str(min_mean)


#head (min_mean)
#rm (min_mean)

########################################################

library(lme4)
install.packages("lme4")
?lmer

#st=read.csv("$/Documents/zmhmp/starch_test.csv",header=T,na.strings = "NaN")
#head(st)
#names(st)

summary(test2)
attach(test2)
h1=hist(test2$B11,col=""gold"", main="Histogram of Boron accross 2009 and 2010", xlab=""Concentration (ppm)" (ppm)",font.lab=2)


##############

names(test2[,6:25])

par(mfrow=c(2,3))

h10 <- hist(test2$B11,col="gold", main="Histogram of Boron accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h11 <- hist(test2$Na23,col="gold", main="Histogram of Sodium accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h12 <- hist(test2$Mg25,col="gold", main="Histogram of Magnesium accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h13 <- hist(test2$Al27,col="gold", main="Histogram of Aluminium accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h14 <- hist(test2$P31,col="gold", main="Histogram of Phosphorus accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h15 <- hist(test2$S34,col="gold", main="Histogram of Sulfur accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

par(mfrow=c(2,3))

h16 <- hist(test2$K39,col="gold", main="Histogram of Potassium accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h17 <- hist(test2$Ca43,col="gold", main="Histogram of Calcium accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h18 <- hist(test2$Mn55,col="gold", main="Histogram of Manganese accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h19 <- hist(test2$Fe57,col="gold", main="Histogram of Iron accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h1 <- hist(test2$Co59,col="gold", main="Histogram of Cobalt accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h20 <- hist(test2$Ni60,col="gold", main="Histogram of Nickel accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

par(mfrow=c(2,3))
h2 <- hist(test2$Cu65,col="gold", main="Histogram of Copper accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h3 <- hist(test2$Zn66,col="gold", main="Histogram of Zinc accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h4 <- hist(test2$As75,col="gold", main="Histogram of Arsenic accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h5 <- hist(test2$Se82,col="gold", main="Histogram of Selenium accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h6 <- hist(test2$Rb85,col="gold", main="Histogram of Rubidium accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h7 <- hist(test2$Sr88,col="gold", main="Histogram of Strontium accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

par(mfrow=c(2,3))
h8 <- hist(test2$Mo98,col="gold", main="Histogram of Molybdenum accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h9 <- hist(test2$Cd111,col="gold", main="Histogram of Cadmium accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
################

#?hist
#attach(test2)

par(mfrow=c(2,3))
boxplot(test2$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Boron concentration by year", col="pink",font=2,font.lab=2)
boxplot(test2$Na23~Year, xlab="Year", ylab="Concentration (ppm)",main="Sodium concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Mg25~Year, xlab="Year", ylab="Concentration (ppm)",main="Magnesium concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Al27~Year, xlab="Year", ylab="Concentration (ppm)",main="Aluminium concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$P31~Year, xlab="Year", ylab="Concentration (ppm)",main="Phosphorus concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$S34~Year, xlab="Year", ylab="Concentration (ppm)",main="Sulfur concentration by year",col="pink",font=2,font.lab=2)

par(mfrow=c(2,3))
boxplot(test2$K39~Year, xlab="Year", ylab="Concentration (ppm)",main="Potassium concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Ca43~Year, xlab="Year", ylab="Concentration (ppm)",main="Calcium concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Mn55~Year, xlab="Year", ylab="Concentration (ppm)",main="Manganese concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Fe57~Year, xlab="Year", ylab="Concentration (ppm)",main="Iron concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Co59~Year, xlab="Year", ylab="Concentration (ppm)",main="Cobalt concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Ni60~Year, xlab="Year", ylab="Concentration (ppm)",main="Nickel concentration by year",col="pink",font=2,font.lab=2)

par(mfrow=c(2,3))
boxplot(test2$Cu65~Year, xlab="Year", ylab="Concentration (ppm)",main="Copper concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Zn66~Year, xlab="Year", ylab="Concentration (ppm)",main="Zinc concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$As75~Year, xlab="Year", ylab="Concentration (ppm)",main="Arsenic concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Se82~Year, xlab="Year", ylab="Concentration (ppm)",main="Selenium concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Rb85~Year, xlab="Year", ylab="Concentration (ppm)",main="Rubidium concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Sr88~Year, xlab="Year", ylab="Concentration (ppm)",main="Strontium concentration by year",col="pink",font=2,font.lab=2)

par(mfrow=c(2,3))
boxplot(test2$Mo98~Year, xlab="Year", ylab="Concentration (ppm)",main="Molybdenum concentration by year",col="pink",font=2,font.lab=2)
boxplot(test2$Cd111~Year, xlab="Year", ylab="Concentration (ppm)",main="Cadmium concentration by year",col="pink",font=2,font.lab=2)


names(min_com[,-c(1:3)])
#####################################


attach(min_com)
Boron=as.numeric(B11)
Line=as.factor(pedigree)
Year=as.factor(Year)

library(lme4)

#Linear model for variance components
Boronvarcomp=lmer(min_com$B11~(1|Line)+(1|Year)+(1|Line:Year))
T1=summary(Boronvarcomp)

ls(T1)

T1$

#anova(starchvarcomp)

b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Boron concentration by year", col="pink",font=2,font.lab=2)
  h10 <- hist(min_com$B11,col="gold", main="Histogram of Boron accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
ls(b10)
Boronmodel=lmer(min_com$B11~(1|Line)+(1|Year)+(1|Line:Year)) 
  
boronBLUP=ranef(Boronmodel)
str(boronBLUP)

boronlineBLUP=boronBLUP$Line

str(boronlineBLUP)

write.csv(boronlineBLUP,"~/Desktop/boronlineBLUP.csv")

T1lineBLUP=boronlineBLUP[,1]
hist(T1lineBLUP,col="brown")

lmean=tapply(min_com$B11,min_com$pedigree,na.rm=T,mean)



#hT1=var(Line)/[var(Line)+var(Line:Year)/2+var(RESIDUAL)/2]
BoronlineBLUP<- T1lineBLUP

?plot


heritability_of_boron=0.096076/(0.096076+(0.183598/2)+(0.551732/2))
plot(boronlineBLUP,lmean, col=c("blue","red"))
#lines(BoronlineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

##use ggplot to further check difference by year (since it looks bimodal)
library(ggplot2)
install.packages("ggplot2")

p1=ggplot(min_com,aes=(BoronlineBLUP,lmean))

str(boronlineBLUP)
#####################################################


#boxplots with raw data
par(mfrow=c(2,3))
boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Boron concentration by year", col="pink",font=2,font.lab=2)
boxplot(min_com$Na23~Year, xlab="Year", ylab="Concentration (ppm)",main="Sodium concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Mg25~Year, xlab="Year", ylab="Concentration (ppm)",main="Magnesium concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Al27~Year, xlab="Year", ylab="Concentration (ppm)",main="Aluminium concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$P31~Year, xlab="Year", ylab="Concentration (ppm)",main="Phosphorus concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$S34~Year, xlab="Year", ylab="Concentration (ppm)",main="Sulfur concentration by year",col="pink",font=2,font.lab=2)

par(mfrow=c(2,3))
boxplot(min_com$K39~Year, xlab="Year", ylab="Concentration (ppm)",main="Potassium concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Ca43~Year, xlab="Year", ylab="Concentration (ppm)",main="Calcium concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Mn55~Year, xlab="Year", ylab="Concentration (ppm)",main="Manganese concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Fe57~Year, xlab="Year", ylab="Concentration (ppm)",main="Iron concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Co59~Year, xlab="Year", ylab="Concentration (ppm)",main="Cobalt concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Ni60~Year, xlab="Year", ylab="Concentration (ppm)",main="Nickel concentration by year",col="pink",font=2,font.lab=2)

par(mfrow=c(2,3))
boxplot(min_com$Cu65~Year, xlab="Year", ylab="Concentration (ppm)",main="Copper concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Zn66~Year, xlab="Year", ylab="Concentration (ppm)",main="Zinc concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$As75~Year, xlab="Year", ylab="Concentration (ppm)",main="Arsenic concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Se82~Year, xlab="Year", ylab="Concentration (ppm)",main="Selenium concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Rb85~Year, xlab="Year", ylab="Concentration (ppm)",main="Rubidium concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Sr88~Year, xlab="Year", ylab="Concentration (ppm)",main="Strontium concentration by year",col="pink",font=2,font.lab=2)

par(mfrow=c(2,3))
boxplot(min_com$Mo98~Year, xlab="Year", ylab="Concentration (ppm)",main="Molybdenum concentration by year",col="pink",font=2,font.lab=2)
boxplot(min_com$Cd111~Year, xlab="Year", ylab="Concentration (ppm)",main="Cadmium concentration by year",col="pink",font=2,font.lab=2)


names(min_com[,-c(1:3)])


str(min_com$Al27)

#min_com[min_com$Al27>=5]<-NA

#method1:outlier detection

#source("https://goo.gl/4mthoF")

#ol_Al27=outlierKD(min_com, min_com$Al27)

#method2 outlier detection
#findOutlier<- function(min_com, cutoff = 3) {
  ## Calculate the sd
 # sds <- apply(min_com, 2, sd, na.rm = TRUE)
  ## Identify the cells with value greater than cutoff * sd (column wise)
#  result <- mapply(function(d, s) {
 #   which(d > cutoff * s)
 # }, min_com, sds)
#  result
#}

#outliers <- findOutlier(min_com)
#outliers

#summary(outliers)

min_com1 <- min_com
length(min_com1)
str(min_com1)
summary(min_com1[,-c(1:4)])

attach(min_com1)
head(B11)

min_com2=subset(min_com1,Al27 <=5)    

min_com2=subset(min_com2,Co59 <=0.05)
                
min_com2=subset(min_com2,Ni60 <=1.5)

min_com2=subset(min_com2,As75 <=0.1)

min_com2=subset(min_com2,Se82 <=100)

min_com2=subset(min_com2,Cd111<=0.4)

min_com2=subset(min_com2,P31<=7000)

str(min_com2)

head(min_com2)

#which(min_com2$Al27>5)

#summary(min_com2[,-c(1:4)])

#min_com1$Al27[which(min_com1$Al27 >= 5),]= NA

#which(min_com1$Al27 >= 5)


summary(min_com1)
rm(min_com1)

temp


outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}
par(mfrow=c(1,1))
##########################################
#Boron_outlier_check

outlierKD_B11 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for B11", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_B11 (min_com,B11)

########################
#Na23_outlier_check

outlierKD_Na23 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Na23", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Na23 (min_com,Na23)


################

#Mg25_outlier_check

outlierKD_Mg25 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Mg25", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Mg25 (min_com,Mg25)

##############################
#Al27_outlier_check

outlierKD_Al27 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Al27", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Al27 (min_com,Al27)

####################

#P31_outlier_check

outlierKD_P31 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for P31", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_P31 (min_com,P31)


##################
#S34_outlier_check

outlierKD_S34 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for S34", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_S34 (min_com,S34)


############################


#K39_outlier_check

outlierKD_K39 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for K39", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_K39 (min_com,K39)

#check if this is really important
min_com[c(89,939,1131,1132,1752,1911,1910),]


#summary(min_com1)

#########################################
#Ca43_outlier_check

outlierKD_Ca43 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Ca43", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Ca43 (min_com,Ca43)

#########################################
#Mn55_outlier_check

outlierKD_Mn55 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Mn55", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Mn55 (min_com,Mn55)

#########################################
#Fe57_outlier_check

outlierKD_Fe57 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Fe57", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Fe57 (min_com,Fe57)

########################################
#Co59_outlier_check

outlierKD_Co59 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Co59", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Co59 (min_com,Co59)

############################
#Ni60_outlier_check

outlierKD_Ni60 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Ni60", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Ni60 (min_com,Ni60)

###########################
#Cu65_outlier_check

outlierKD_Cu65 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Cu65", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Cu65 (min_com,Cu65)

######################

#Zn66_outlier_check

outlierKD_Zn66 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Zn66", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Zn66 (min_com,Zn66)

#####################

#As75_outlier_check

outlierKD_As75 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for As75", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_As75 (min_com,As75)

###################

#Se82_outlier_check

outlierKD_Se82 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Se82", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Se82 (min_com,Se82)

###################

#Rb85_outlier_check

outlierKD_Rb85 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Rb85", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Rb85 (min_com,Rb85)

###################

#Sr88_outlier_check

outlierKD_Sr88 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Sr88", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Sr88 (min_com,Sr88)

###################

#Mo98_outlier_check

outlierKD_Mo98 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Mo98", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Mo98 (min_com,Mo98)

####################

#Cd111_outlier_check

outlierKD_Cd111 <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name,coef = 3)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for Cd111", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}


outlierKD_Cd111 (min_com,Cd111)

###################







####################



###check if we can customize data for NAs for clean_up using following code
boxplot.stats(Mn55,coef = 3)$out
which(Mn55>13)
mn=which(Mn55>13)
min_com1[mn,]

names(min_com[-c(1:4)])
ls(n)
n[,-c(1:4)]

write.csv(n,file="/Users/rkastooriramamurth2/Desktop/Mineral_names.csv")
getwd()

attach(min_com1)

head(min_com)

###########################
#Re-do whole analysis using min_com1 data to allow customization of the outliers
#all outlier values
boxplot.stats(B11,coef = 3)$out
boxplot.stats(Na23,coef = 3)$out
boxplot.stats(Mg25,coef = 3)$out
boxplot.stats(Al27,coef = 3)$out
boxplot.stats(P31,coef = 3)$out
boxplot.stats(S34,coef = 3)$out
boxplot.stats(K39,coef = 3)$out
boxplot.stats(Ca43,coef = 3)$out
boxplot.stats(Mn55,coef = 3)$out
boxplot.stats(Fe57,coef = 3)$out
boxplot.stats(Co59,coef = 3)$out
boxplot.stats(Ni60,coef = 3)$out
boxplot.stats(Cu65,coef = 3)$out
boxplot.stats(Zn66,coef = 3)$out
boxplot.stats(As75,coef = 3)$out
boxplot.stats(Se82,coef = 3)$out
boxplot.stats(Rb85,coef = 3)$out
boxplot.stats(Sr88,coef = 3)$out
boxplot.stats(Mo98,coef = 3)$out
boxplot.stats(Cd111,coef = 3)$out


#obtain range of the outliers
range(boxplot.stats(B11,coef = 3)$out)
min(boxplot.stats(Na23,coef = 3)$out)
min(boxplot.stats(Mg25,coef = 3)$out)
min(boxplot.stats(Al27,coef = 3)$out)
min(boxplot.stats(P31,coef = 3)$out)
min(boxplot.stats(S34,coef = 3)$out)
min(boxplot.stats(K39,coef = 3)$out)
min(boxplot.stats(Ca43,coef = 3)$out)
min(boxplot.stats(Mn55,coef = 3)$out)
min(boxplot.stats(Fe57,coef = 3)$out)
min(boxplot.stats(Co59,coef = 3)$out)
min(boxplot.stats(Ni60,coef = 3)$out)
min(boxplot.stats(Cu65,coef = 3)$out)
min(boxplot.stats(Zn66,coef = 3)$out)
min(boxplot.stats(As75,coef = 3)$out)
min(boxplot.stats(Se82,coef = 3)$out)
min(boxplot.stats(Rb85,coef = 3)$out)
min(boxplot.stats(Sr88,coef = 3)$out)
min(boxplot.stats(Mo98,coef = 3)$out)
min(boxplot.stats(Cd111,coef = 3)$out)


#Find which lines are above the minimum
z1<-which(B11>7.5)
z2<-which(Na23>10.2)
z3<-which(Mg25>300)
z4<-which(Al27>2.08)
z5<-which(P31>860)
z6<-which(S34>1991)
z7<-which(K39>6800)
z8<-which(Ca43>188)
z9<-which(Mn55>13.57)
z10<-which(Fe57>37.8)
z11<-which(Co59>0.003)
z12<-which(Ni60>0.73)
z13<-which(Cu65>6.19)
z14<-which(Zn66>47)
z15<-which(As75>0.04)
z16<-which(Se82>4.44)
z17<-which(Rb85>5.18)
z18<-which(Sr88>0.57)
z19<-which(Mo98>0.86)
z20<-which(Cd111>0.11)


z21=rbind(c(
  z1,
  z2,
  z3,
  z4,
  z5,
  z6,
  z7,
  z8,
  z9,
  z10,
  z11,
  z12,
  z13,
  z14,
  z15,
  z16,
  z17,
  z18,
  z19,
  z20))


summary(z21)



warnings()
##test
z1<-which(B11>7.5)
z1<-which(Na23>10.2)
z1<-which(Mg25>300)
z1<-which(Al27>2.08)
z1<-which(P31>860)
z1<-which(S34>1991)
z1<-which(K39>6800)
z1<-which(Ca43>188)
z1<-which(Mn55>13.57)
z1<-which(Fe57>37.8)
z1<-which(Co59>0.003)
z1<-which(Ni60>0.73)
z1<-which(Cu65>6.19)
z1<-which(Zn66>47)
z1<-which(As75>0.04)
z1<-which(Se82>4.44)
z1<-which(Rb85>5.18)
z1<-which(Sr88>0.57)
z1<-which(Mo98>0.86)
z1<-which(Cd111>0.11)

z2=min_com1[z1,]

z1
z2
z3

write.csv(z2,file="/Users/rkastooriramamurth2/Desktop/Outliers_all_min_combined.csv")

############
getwd()
setwd(/Users/rkastooriramamurth2)
pdf("boxplots_IQR*3.pdf")

############
#############################################
#Re-plot boxplots with Clean data (filtered for outliers IQR*3)
##############################################
par(mfrow=c(2,3))
boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Boron concentration \nby year", col="pink",font=2,font.lab=2)
boxplot(min_com$Na23~Year, xlab="Year", ylab="Concentration (ppm)",main="Sodium concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Mg25~Year, xlab="Year", ylab="Concentration (ppm)",main="Magnesium concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Al27~Year, xlab="Year", ylab="Concentration (ppm)",main="Aluminium concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$P31~Year, xlab="Year", ylab="Concentration (ppm)",main="Phosphorus concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$S34~Year, xlab="Year", ylab="Concentration (ppm)",main="Sulfur concentration \nby year",col="pink",font=2,font.lab=2)

par(mfrow=c(2,3))
boxplot(min_com$K39~Year, xlab="Year", ylab="Concentration (ppm)",main="Potassium concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Ca43~Year, xlab="Year", ylab="Concentration (ppm)",main="Calcium concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Mn55~Year, xlab="Year", ylab="Concentration (ppm)",main="Manganese concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Fe57~Year, xlab="Year", ylab="Concentration (ppm)",main="Iron concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Co59~Year, xlab="Year", ylab="Concentration (ppm)",main="Cobalt concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Ni60~Year, xlab="Year", ylab="Concentration (ppm)",main="Nickel concentration \nby year",col="pink",font=2,font.lab=2)

par(mfrow=c(2,3))
boxplot(min_com$Cu65~Year, xlab="Year", ylab="Concentration (ppm)",main="Copper concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Zn66~Year, xlab="Year", ylab="Concentration (ppm)",main="Zinc concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$As75~Year, xlab="Year", ylab="Concentration (ppm)",main="Arsenic concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Se82~Year, xlab="Year", ylab="Concentration (ppm)",main="Selenium concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Rb85~Year, xlab="Year", ylab="Concentration (ppm)",main="Rubidium concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Sr88~Year, xlab="Year", ylab="Concentration (ppm)",main="Strontium concentration \nby year",col="pink",font=2,font.lab=2)

par(mfrow=c(2,3))
boxplot(min_com$Mo98~Year, xlab="Year", ylab="Concentration (ppm)",main="Molybdenum concentration \nby year",col="pink",font=2,font.lab=2)
boxplot(min_com$Cd111~Year, xlab="Year", ylab="Concentration (ppm)",main="Cadmium concentration \nby year",col="pink",font=2,font.lab=2)

dev.off()


####################
#histograms with with Clean data (filtered for outliers IQR*3)
pdf("histograms_IQR*3.pdf")
#####################

par(mfrow=c(2,3))

h10 <- hist(min_com$B11,col="gold", main="Histogram of Boron \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h11 <- hist(min_com$Na23,col="gold", main="Histogram of Sodium \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h12 <- hist(min_com$Mg25,col="gold", main="Histogram of Magnesium \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h13 <- hist(min_com$Al27,col="gold", main="Histogram of Aluminium \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h14 <- hist(min_com$P31,col="gold", main="Histogram of Phosphorus \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h15 <- hist(min_com$S34,col="gold", main="Histogram of Sulfur \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

par(mfrow=c(2,3))

h16 <- hist(min_com$K39,col="gold", main="Histogram of Potassium \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h17 <- hist(min_com$Ca43,col="gold", main="Histogram of Calcium \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h18 <- hist(min_com$Mn55,col="gold", main="Histogram of Manganese \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h19 <- hist(min_com$Fe57,col="gold", main="Histogram of Iron \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h1 <- hist(min_com$Co59,col="gold", main="Histogram of Cobalt \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h20 <- hist(min_com$Ni60,col="gold", main="Histogram of Nickel \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

par(mfrow=c(2,3))
h2 <- hist(min_com$Cu65,col="gold", main="Histogram of Copper \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h3 <- hist(min_com$Zn66,col="gold", main="Histogram of Zinc \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h4 <- hist(min_com$As75,col="gold", main="Histogram of Arsenic \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h5 <- hist(min_com$Se82,col="gold", main="Histogram of Selenium \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h6 <- hist(min_com$Rb85,col="gold", main="Histogram of Rubidium \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h7 <- hist(min_com$Sr88,col="gold", main="Histogram of Strontium \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

par(mfrow=c(2,3))
h8 <- hist(min_com$Mo98,col="gold", main="Histogram of Molybdenum \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
h9 <- hist(min_com$Cd111,col="gold", main="Histogram of Cadmium \naccross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)
dev.off()
############################################
#variance components, heritability and blups
###############################################################
#Change column names to copy it to the terminal and help loop it
###############################################################
attach(min_com)
Line=as.factor(pedigree)
Year=as.factor(Year)

colnames(min_com[,-c(3:24)])<- c("Time","line")

colnames(min_com[,-c(1:4)]) <- c("c1",	"c2",	"c3",	"c4",	"c5",	"c6",	"c7",	"c8",	"c9",	"c10",	"c11",	"c12",	"c13",	"c14",	"c15",	"c16",	"c17",	"c18",	"c19",	"c20")



library(lme4)

#Linear model for variance components
c1_varcomp=lmer(min_com$B11~ Line + (1|Year)+(1|Line:Year))
summary(c1_varcomp)
anova(c1_)
#anova(starchvarcomp)
  
#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="c1_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of c1_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

c1_model=lmer(min_com$B11~(1|Line)+(1|Year)+(1|Line:Year)) 

c1_BLUP=ranef(c1_model)
str(c1_BLUP)

c1_lineBLUP=c1_BLUP$Line

str(c1_lineBLUP)

write.csv(c1_lineBLUP,"~/Box/Ziegler_set/c1_lineBLUP.csv")

T1lineBLUP=c1_lineBLUP[,1]

c1__BLUP <- T1lineBLUP
par(mfrow=c(1,1))
hist(c1__BLUP,col="brown")

lmean=tapply(min_com$B11,min_com$pedigree,na.rm=T,mean)

? function



#hT1=var(Line)/[var(Line)+var(Line:Year)/2+var(RESIDUAL)/2]
c1_lineBLUP<- T1lineBLUP

?plot


h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
h_B11

plot(c1_lineBLUP,lmean, col=c("blue","red"))
#lines(c1_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)


write.csv(min_com,file="min_com_rm_otliers_3xIQR.csv")

getwd()


zie_282 <- read.csv("min_com_rm_otliers_3xIQR.csv",header=T,na.strings = NA)
summary(zie_282)
str(zie_282)

min_com <-zie_282
#############################################################################
#BLUPS and heritability
#############################################################################

#Linear model for variance components

Line<- min_com$pedigree
attach(min_com)
Boron_varcomp=lmer(min_com$B11~(1|Line)+(1|Year)+(1|Line:Year))
summary(Boron_varcomp)
#anova(Boron_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Boron_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Boron_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Boron_model=lmer(min_com$B11~(1|Line)+(1|Year)+(1|Line:Year)) 


Boron_BLUP=ranef(Boron_model)
str(Boron_BLUP)

Boron_lineBLUP=Boron_BLUP$Line

str(Boron_lineBLUP)

c1 <- write.csv(Boron_lineBLUP,"~/Box/Ziegler_set/Boron_lineBLUP_rm_outlier.csv")

Boron_lineBLUP =Boron_lineBLUP[,1]

pdf("Boron_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Boron_lineBLUP,col="brown")

lmean=tapply(min_com$B11,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
?anova
#h_Boron=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Boron_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Boron_lineBLUP,lmean, col=c("blue","red"))
#lines(Boron_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()

getwd()

################
plot(min_com[,-c(1:4)])
################

Sodium<-min_com$Na23
Magnesium<-min_com$Mg25
Aluminium<-min_com$Al27
Phosphorus<-min_com$P31
Sulfur<-min_com$S34
Potassium<-min_com$K39
Calcium<-min_com$Ca43
Manganese<-min_com$Mn55
Iron<-min_com$Fe57
Cobalt<-min_com$Co59
Nickel<-min_com$Ni60
Copper<-min_com$Cu65
Zinc<-min_com$Zn66
Arsenic<-min_com$As75
Selenium<-min_com$Se82
Rubidium<-min_com$Rb85
Strontium<-min_com$Sr88
Molybdenum<-min_com$Mo98
Cadmium<-min_com$Cd111


###################################
#Sodium_BLUPs
###################################
Sodium_varcomp=lmer(min_com$Na23~(1|Line)+(1|Year)+(1|Line:Year))
summary(Sodium_varcomp)
#anova(Sodium_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Sodium_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Sodium_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Sodium_model=lmer(min_com$Na23~(1|Line)+(1|Year)+(1|Line:Year)) 


Sodium_BLUP=ranef(Sodium_model)
str(Sodium_BLUP)

Sodium_lineBLUP=Sodium_BLUP$Line

str(Sodium_lineBLUP)

c2=write.csv(Sodium_lineBLUP,"~/Box/Ziegler_set/Sodium_lineBLUP_rm_outlier.csv")

Sodium_lineBLUP =Sodium_lineBLUP[,1]

pdf("Sodium_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Sodium_lineBLUP,col="brown")

lmean=tapply(min_com$Na23,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Sodium=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Sodium_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Sodium_lineBLUP,lmean, col=c("blue","red"))
#lines(Sodium_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()

###################################
#Magnesium_BLUPs
###################################
Magnesium_varcomp=lmer(min_com$Mg25~(1|Line)+(1|Year)+(1|Line:Year))
summary(Magnesium_varcomp)
#anova(Magnesium_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Magnesium_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Magnesium_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Magnesium_model=lmer(min_com$Mg25~(1|Line)+(1|Year)+(1|Line:Year)) 


Magnesium_BLUP=ranef(Magnesium_model)
str(Magnesium_BLUP)

Magnesium_lineBLUP=Magnesium_BLUP$Line

str(Magnesium_lineBLUP)

c3=write.csv(Magnesium_lineBLUP,"~/Box/Ziegler_set/Magnesium_lineBLUP_rm_outlier.csv")

Magnesium_lineBLUP =Magnesium_lineBLUP[,1]

pdf("Magnesium_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Magnesium_lineBLUP,col="brown")

lmean=tapply(min_com$Mg25,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Magnesium=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Magnesium_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Magnesium_lineBLUP,lmean, col=c("blue","red"))
#lines(Magnesium_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()

##########################
###################################
#Aluminium_BLUPs
###################################
Aluminium_varcomp=lmer(min_com$Al27~(1|Line)+(1|Year)+(1|Line:Year))
summary(Aluminium_varcomp)
#anova(Aluminium_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Aluminium_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Aluminium_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Aluminium_model=lmer(min_com$Al27~(1|Line)+(1|Year)+(1|Line:Year)) 


Aluminium_BLUP=ranef(Aluminium_model)
str(Aluminium_BLUP)

Aluminium_lineBLUP=Aluminium_BLUP$Line

str(Aluminium_lineBLUP)

c4=write.csv(Aluminium_lineBLUP,"~/Box/Ziegler_set/Aluminium_lineBLUP_rm_outlier.csv")

Aluminium_lineBLUP =Aluminium_lineBLUP[,1]

pdf("Aluminium_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Aluminium_lineBLUP,col="brown")

lmean=tapply(min_com$Al27,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Aluminium=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Aluminium_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Aluminium_lineBLUP,lmean, col=c("blue","red"))
#lines(Aluminium_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()

##########################
#Phosphorus BLUPs
#########################

Phosphorus_varcomp=lmer(min_com$P31~(1|Line)+(1|Year)+(1|Line:Year))
summary(Phosphorus_varcomp)
#anova(Phosphorus_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Phosphorus_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Phosphorus_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Phosphorus_model=lmer(min_com$P31~(1|Line)+(1|Year)+(1|Line:Year)) 


Phosphorus_BLUP=ranef(Phosphorus_model)
str(Phosphorus_BLUP)

Phosphorus_lineBLUP=Phosphorus_BLUP$Line

str(Phosphorus_lineBLUP)

c4=write.csv(Phosphorus_lineBLUP,"~/Box/Ziegler_set/Phosphorus_lineBLUP_rm_outlier.csv")

Phosphorus_lineBLUP =Phosphorus_lineBLUP[,1]

pdf("Phosphorus_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Phosphorus_lineBLUP,col="brown")

lmean=tapply(min_com$P31,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Phosphorus=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Phosphorus_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Phosphorus_lineBLUP,lmean, col=c("blue","red"))
#lines(Phosphorus_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()

##########################
#Sulfur BLUPs
#########################

Sulfur_varcomp=lmer(min_com$S34~(1|Line)+(1|Year)+(1|Line:Year))
summary(Sulfur_varcomp)
#anova(Sulfur_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Sulfur_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Sulfur_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Sulfur_model=lmer(min_com$S34~(1|Line)+(1|Year)+(1|Line:Year)) 


Sulfur_BLUP=ranef(Sulfur_model)
str(Sulfur_BLUP)

Sulfur_lineBLUP=Sulfur_BLUP$Line

str(Sulfur_lineBLUP)

c4=write.csv(Sulfur_lineBLUP,"~/Box/Ziegler_set/Sulfur_lineBLUP_rm_outlier.csv")

Sulfur_lineBLUP =Sulfur_lineBLUP[,1]

pdf("Sulfur_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Sulfur_lineBLUP,col="brown")

lmean=tapply(min_com$S34,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Sulfur=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Sulfur_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Sulfur_lineBLUP,lmean, col=c("blue","red"))
#lines(Sulfur_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()

##########################
#Potassium BLUPs
#########################

Potassium_varcomp=lmer(min_com$K39~(1|Line)+(1|Year)+(1|Line:Year))
summary(Potassium_varcomp)
#anova(Potassium_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Potassium_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Potassium_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Potassium_model=lmer(min_com$K39~(1|Line)+(1|Year)+(1|Line:Year)) 


Potassium_BLUP=ranef(Potassium_model)
str(Potassium_BLUP)

Potassium_lineBLUP=Potassium_BLUP$Line

str(Potassium_lineBLUP)

c4=write.csv(Potassium_lineBLUP,"~/Box/Ziegler_set/Potassium_lineBLUP_rm_outlier.csv")

Potassium_lineBLUP =Potassium_lineBLUP[,1]

pdf("Potassium_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Potassium_lineBLUP,col="brown")

lmean=tapply(min_com$K39,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Potassium=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Potassium_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Potassium_lineBLUP,lmean, col=c("blue","red"))
#lines(Potassium_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()

##########################
#Calcium BLUPs
#########################

Calcium_varcomp=lmer(min_com$Ca43~(1|Line)+(1|Year)+(1|Line:Year))
summary(Calcium_varcomp)
#anova(Calcium_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Calcium_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Calcium_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Calcium_model=lmer(min_com$Ca43~(1|Line)+(1|Year)+(1|Line:Year)) 


Calcium_BLUP=ranef(Calcium_model)
str(Calcium_BLUP)

Calcium_lineBLUP=Calcium_BLUP$Line

str(Calcium_lineBLUP)

c4=write.csv(Calcium_lineBLUP,"~/Box/Ziegler_set/Calcium_lineBLUP_rm_outlier.csv")

Calcium_lineBLUP =Calcium_lineBLUP[,1]

pdf("Calcium_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Calcium_lineBLUP,col="brown")

lmean=tapply(min_com$Ca43,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Calcium=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Calcium_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Calcium_lineBLUP,lmean, col=c("blue","red"))
#lines(Calcium_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


##########################
#Manganese BLUPs
#########################

Manganese_varcomp=lmer(min_com$Mn55~(1|Line)+(1|Year)+(1|Line:Year))
summary(Manganese_varcomp)
#anova(Manganese_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Manganese_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Manganese_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Manganese_model=lmer(min_com$Mn55~(1|Line)+(1|Year)+(1|Line:Year)) 


Manganese_BLUP=ranef(Manganese_model)
str(Manganese_BLUP)

Manganese_lineBLUP=Manganese_BLUP$Line

str(Manganese_lineBLUP)

c4=write.csv(Manganese_lineBLUP,"~/Box/Ziegler_set/Manganese_lineBLUP_rm_outlier.csv")

Manganese_lineBLUP =Manganese_lineBLUP[,1]

pdf("Manganese_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Manganese_lineBLUP,col="brown")

lmean=tapply(min_com$Mn55,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Manganese=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Manganese_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Manganese_lineBLUP,lmean, col=c("blue","red"))
#lines(Manganese_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


##########################
#Iron BLUPs
#########################

Iron_varcomp=lmer(min_com$Fe57~(1|Line)+(1|Year)+(1|Line:Year))
summary(Iron_varcomp)
#anova(Iron_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Iron_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Iron_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Iron_model=lmer(min_com$Fe57~(1|Line)+(1|Year)+(1|Line:Year)) 


Iron_BLUP=ranef(Iron_model)
str(Iron_BLUP)

Iron_lineBLUP=Iron_BLUP$Line

str(Iron_lineBLUP)

c4=write.csv(Iron_lineBLUP,"~/Box/Ziegler_set/Iron_lineBLUP_rm_outlier.csv")

Iron_lineBLUP =Iron_lineBLUP[,1]

pdf("Iron_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Iron_lineBLUP,col="brown")

lmean=tapply(min_com$Fe57,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Iron=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Iron_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Iron_lineBLUP,lmean, col=c("blue","red"))
#lines(Iron_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


##########################
#Cobalt BLUPs
#########################
attach(zie_282)
Cobalt_varcomp=lmer(min_com$Co59~(1|Line)+(1|Year)+(1|Line:Year))
summary(Cobalt_varcomp)
#anova(Cobalt_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Cobalt_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Cobalt_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Cobalt_model=lmer(min_com$Co59~(1|Line)+(1|Year)+(1|Line:Year)) 


Cobalt_BLUP=ranef(Cobalt_model)
str(Cobalt_BLUP)

Cobalt_lineBLUP=Cobalt_BLUP$Line

str(Cobalt_lineBLUP)

c4=write.csv(Cobalt_lineBLUP,"~/Box/Ziegler_set/Cobalt_lineBLUP_rm_outlier.csv")

Cobalt_lineBLUP =Cobalt_lineBLUP[,1]

pdf("Cobalt_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Cobalt_lineBLUP,col="brown")

#lmean=tapply(min_com$Co59,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Cobalt=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Cobalt_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Cobalt_lineBLUP,lmean2, col=c("blue","red"))
#lines(Cobalt_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()




##########################
#Nickel BLUPs
#########################
#attach(zie_282)
#min_com <- zie_282
Nickel_varcomp=lmer(min_com$Ni60~(1|Line)+(1|Year)+(1|Line:Year))
summary(Nickel_varcomp)
#anova(Nickel_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Nickel_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Nickel_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Nickel_model=lmer(min_com$Ni60~(1|Line)+(1|Year)+(1|Line:Year)) 


Nickel_BLUP=ranef(Nickel_model)
str(Nickel_BLUP)

Nickel_lineBLUP=Nickel_BLUP$Line

str(Nickel_lineBLUP)

c4=write.csv(Nickel_lineBLUP,"~/Box/Ziegler_set/Nickel_lineBLUP_rm_outlier.csv")

Nickel_lineBLUP =Nickel_lineBLUP[,1]

pdf("Nickel_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Nickel_lineBLUP,col="brown")

#lmean=tapply(min_com$Ni60,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Nickel=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Nickel_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Nickel_lineBLUP,lmean, col=c("blue","red"))
#lines(Nickel_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


library(lme4)

##########################
#Copper BLUPs
#########################
#attach(zie_282)
#min_com <- zie_282
#summary(min_com)

Line <- min_com$pedigree
Copper_varcomp=lmer(min_com$Cu65~(1|Line)+(1|Year)+(1|Line:Year))
summary(Copper_varcomp)
#anova(Copper_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Copper_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Copper_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Copper_model=lmer(min_com$Cu65~(1|Line)+(1|Year)+(1|Line:Year)) 


Copper_BLUP=ranef(Copper_model)
str(Copper_BLUP)

Copper_lineBLUP=Copper_BLUP$Line

str(Copper_lineBLUP)

c4=write.csv(Copper_lineBLUP,"~/Box/Ziegler_set/Copper_lineBLUP_rm_outlier.csv")

Copper_lineBLUP =Copper_lineBLUP[,1]

pdf("Copper_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Copper_lineBLUP,col="brown")

lmean=tapply(min_com$Cu65,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Copper=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Copper_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Copper_lineBLUP,lmean, col=c("blue","red"))
#lines(Copper_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


#library(lme4)

##########################
#Zinc BLUPs
#########################
#attach(zie_282)
#min_com <- zie_282
#summary(min_com)

Line <- min_com$pedigree
Zinc_varcomp=lmer(min_com$Zn66~(1|Line)+(1|Year)+(1|Line:Year))
summary(Zinc_varcomp)
#anova(Zinc_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Zinc_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Zinc_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Zinc_model=lmer(min_com$Zn66~(1|Line)+(1|Year)+(1|Line:Year)) 


Zinc_BLUP=ranef(Zinc_model)
str(Zinc_BLUP)

Zinc_lineBLUP=Zinc_BLUP$Line

str(Zinc_lineBLUP)

c4=write.csv(Zinc_lineBLUP,"~/Box/Ziegler_set/Zinc_lineBLUP_rm_outlier.csv")

Zinc_lineBLUP =Zinc_lineBLUP[,1]

pdf("Zinc_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Zinc_lineBLUP,col="brown")

lmean=tapply(min_com$Zn66,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Zinc=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Zinc_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Zinc_lineBLUP,lmean, col=c("blue","red"))
#lines(Zinc_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


#library(lme4)

##########################
#Arsenic BLUPs
#########################
#attach(zie_282)
#min_com <- zie_282
#summary(min_com)

Line <- min_com$pedigree
Arsenic_varcomp=lmer(min_com$As75~(1|Line)+(1|Year)+(1|Line:Year))
summary(Arsenic_varcomp)
#anova(Arsenic_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Arsenic_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Arsenic_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Arsenic_model=lmer(min_com$As75~(1|Line)+(1|Year)+(1|Line:Year)) 


Arsenic_BLUP=ranef(Arsenic_model)
str(Arsenic_BLUP)

Arsenic_lineBLUP=Arsenic_BLUP$Line

str(Arsenic_lineBLUP)

c4=write.csv(Arsenic_lineBLUP,"~/Box/Ziegler_set/Arsenic_lineBLUP_rm_outlier.csv")

Arsenic_lineBLUP =Arsenic_lineBLUP[,1]

pdf("Arsenic_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Arsenic_lineBLUP,col="brown")

lmean=tapply(min_com$As75,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Arsenic=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Arsenic_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Arsenic_lineBLUP,lmean, col=c("blue","red"))
#lines(Arsenic_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


#library(lme4)


##########################
#Selenium BLUPs
#########################
#attach(zie_282)
#min_com <- zie_282
#summary(min_com)

Line <- min_com$pedigree
Selenium_varcomp=lmer(min_com$Se82~(1|Line)+(1|Year)+(1|Line:Year))
summary(Selenium_varcomp)
#anova(Selenium_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Selenium_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Selenium_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Selenium_model=lmer(min_com$Se82~(1|Line)+(1|Year)+(1|Line:Year)) 


Selenium_BLUP=ranef(Selenium_model)
str(Selenium_BLUP)

Selenium_lineBLUP=Selenium_BLUP$Line

str(Selenium_lineBLUP)

c4=write.csv(Selenium_lineBLUP,"~/Box/Ziegler_set/Selenium_lineBLUP_rm_outlier.csv")

Selenium_lineBLUP =Selenium_lineBLUP[,1]

pdf("Selenium_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Selenium_lineBLUP,col="brown")

lmean=tapply(min_com$Se82,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Selenium=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Selenium_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Selenium_lineBLUP,lmean, col=c("blue","red"))
#lines(Selenium_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


#library(lme4)



##########################
#Rubidium BLUPs
#########################
#attach(zie_282)
#min_com <- zie_282
#summary(min_com)

Line <- min_com$pedigree
Rubidium_varcomp=lmer(min_com$Rb85~(1|Line)+(1|Year)+(1|Line:Year))
summary(Rubidium_varcomp)
#anova(Rubidium_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Rubidium_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Rubidium_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Rubidium_model=lmer(min_com$Rb85~(1|Line)+(1|Year)+(1|Line:Year)) 


Rubidium_BLUP=ranef(Rubidium_model)
str(Rubidium_BLUP)

Rubidium_lineBLUP=Rubidium_BLUP$Line

str(Rubidium_lineBLUP)

c4=write.csv(Rubidium_lineBLUP,"~/Box/Ziegler_set/Rubidium_lineBLUP_rm_outlier.csv")

Rubidium_lineBLUP =Rubidium_lineBLUP[,1]

pdf("Rubidium_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Rubidium_lineBLUP,col="brown")

lmean=tapply(min_com$Rb85,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Rubidium=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Rubidium_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Rubidium_lineBLUP,lmean, col=c("blue","red"))
#lines(Rubidium_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


#library(lme4)



##########################
#Strontium BLUPs
#########################
#attach(zie_282)
#min_com <- zie_282
#summary(min_com)

Line <- min_com$pedigree
Strontium_varcomp=lmer(min_com$Sr88~(1|Line)+(1|Year)+(1|Line:Year))
summary(Strontium_varcomp)
#anova(Strontium_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Strontium_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Strontium_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Strontium_model=lmer(min_com$Sr88~(1|Line)+(1|Year)+(1|Line:Year)) 


Strontium_BLUP=ranef(Strontium_model)
str(Strontium_BLUP)

Strontium_lineBLUP=Strontium_BLUP$Line

str(Strontium_lineBLUP)

c4=write.csv(Strontium_lineBLUP,"~/Box/Ziegler_set/Strontium_lineBLUP_rm_outlier.csv")

Strontium_lineBLUP =Strontium_lineBLUP[,1]

pdf("Strontium_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Strontium_lineBLUP,col="brown")

lmean=tapply(min_com$Sr88,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Strontium=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Strontium_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Strontium_lineBLUP,lmean, col=c("blue","red"))
#lines(Strontium_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


#library(lme4)





##########################
#Molybdenum BLUPs
#########################
#attach(zie_282)
#min_com <- zie_282
#summary(min_com)

Line <- min_com$pedigree
Molybdenum_varcomp=lmer(min_com$Mo98~(1|Line)+(1|Year)+(1|Line:Year))
summary(Molybdenum_varcomp)
#anova(Molybdenum_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Molybdenum_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Molybdenum_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Molybdenum_model=lmer(min_com$Mo98~(1|Line)+(1|Year)+(1|Line:Year)) 


Molybdenum_BLUP=ranef(Molybdenum_model)
str(Molybdenum_BLUP)

Molybdenum_lineBLUP=Molybdenum_BLUP$Line

str(Molybdenum_lineBLUP)

c4=write.csv(Molybdenum_lineBLUP,"~/Box/Ziegler_set/Molybdenum_lineBLUP_rm_outlier.csv")

Molybdenum_lineBLUP =Molybdenum_lineBLUP[,1]

pdf("Molybdenum_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Molybdenum_lineBLUP,col="brown")

lmean=tapply(min_com$Mo98,min_com$pedigree,na.rm=T,mean)

#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Molybdenum=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Molybdenum_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Molybdenum_lineBLUP,lmean, col=c("blue","red"))
#lines(Molybdenum_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


#library(lme4)






##########################
#Cadmium BLUPs
#########################
#attach(zie_282)
#min_com <- zie_282
#summary(min_com)

Line <- min_com$pedigree
Cadmium_varcomp=lmer(min_com$Cd111~(1|Line)+(1|Year)+(1|Line:Year))
summary(Cadmium_varcomp)
#anova(Cadmium_)
#anova(starchvarcomp)

#b10=boxplot(min_com$B11~Year, xlab="Year", ylab="Concentration (ppm)", main="Cadmium_ concentration by year", col="pink",font=2,font.lab=2)
#h10 <- hist(min_com$B11,col="gold", main="Histogram of Cadmium_ accross 2009 and 2010", xlab="Concentration (ppm)",font.lab=2)

Cadmium_model=lmer(min_com$Cd111~(1|Line)+(1|Year)+(1|Line:Year)) 


Cadmium_BLUP=ranef(Cadmium_model)
str(Cadmium_BLUP)

Cadmium_lineBLUP=Cadmium_BLUP$Line

str(Cadmium_lineBLUP)

c4=write.csv(Cadmium_lineBLUP,"~/Box/Ziegler_set/Cadmium_lineBLUP_rm_outlier.csv")

Cadmium_lineBLUP =Cadmium_lineBLUP[,1]

pdf("Cadmium_lineBLUP_without_outliers.pdf")
par(mfrow=c(2,1))
hist(Cadmium_lineBLUP,col="brown")

lmean=tapply(min_com$Cd111,min_com$pedigree,na.rm=T,mean)
lmean=lmean[!is.na(lmean)]
summary(lmean)
#anova(lmer(min_com$B11~ Line+ Year+(1|Line:Year)))
#?anova
#h_Cadmium=var(Line)/(var(Line)+var(Line:Year)/2+var(RESIDUAL)/2)

#Cadmium_lineBLUP<- T1lineBLUP

?plot


#h_B11=0.108835/(0.108835+(0.118349/2)+(0.416725/2))
#h_B11

plot(Cadmium_lineBLUP,lmean, col=c("blue","red"))
#lines(Cadmium_lineBLUP,lmean,pch=18,col="black",lty=2)
legend("topleft",legend=c("BLUP","line_mean"),col=c("blue","red"), pch=1,cex=1)

dev.off()


#library(lme4)


class(Cadmium_varcomp)

262.5/35



#############################
#heritability estimations
#############################
var(pedigree)

f1 = function(x){
  var(pedigree)/[var(pedigree)+var(pedigree:Year)/2+var(RESIDUAL)/2]
  return(sum(x))
}

apply(min_com, 2, f1)


#install.packages("sommer")
#library(sommer)                 

#data(min_com)
#head(min_com)

#ans1 <- mmer2(y~1,
 #             random = ~pedigree + Year + pedigree:Year,

  #            data=min_com, silent = TRUE)
#suma <- summary(ans1)

#######################################
#Extract var comp to a text file
#####################################
sink("Variance_components_rm_outliers.txt")
summary(Aluminium_varcomp)
summary(Arsenic_varcomp)
summary(Boron_varcomp)
summary(Cadmium_varcomp)
summary(Calcium_varcomp)
summary(Cobalt_varcomp)
summary(Copper_varcomp)
summary(Iron_varcomp)
summary(Magnesium_varcomp)
summary(Manganese_varcomp)
summary(Molybdenum_varcomp)
summary(Nickel_varcomp)
summary(Phosphorus_varcomp)
summary(Potassium_varcomp)
summary(Rubidium_varcomp)
summary(Selenium_varcomp)
summary(Sodium_varcomp)
summary(Strontium_varcomp)
summary(Sulfur_varcomp)
summary(Zinc_varcomp)

sink()


??pairwise.complete.obs


install.packages("corrplot")
library(corrplot)
zie_282=zie_282[,-c(1:5)]

pdf(file="Ziegler_corr_na.or.comp.pdf")
df2 <- cor(zie_282,use = "na.or.complete")
corrplot(df2, method="shade",shade.col=NA, tl.col="black", tl.srt=45)
dev.off()

df3=min_com

df3 <- cor(min_com[,-c(1:18)],use = "na.or.complete")
corrplot(df3, method="shade",shade.col=NA, tl.col="black", tl.srt=45)
#summary(min_com[,-c(1:18)])


corr_coef=round(df2,digits=2)

write.csv(corr_coef,file="Correlation_coefficients_Ziegler.csv")

round(df2,digits=2)



c1 <-lmer(zie_282$Na23~Line+(1|Year)+(1|Line:Year)) 

c2 <-lmer(zie_282$Mg25~Line+(1|Year)+(1|Line:Year)) 

#######################################################
#Get BLUE for all traits
#######################################################

blues.rb <- function(traits, dat = ".") {
  b<- as.data.frame(fixef(lmer(paste0(traits, "~ 0 + Line + (1|Year) + 
                                      (1|Line:Year)"), data = dat)))
}


effectvar <- names(zie_282) %in% 
  c( "X","Year","pedigree","PlotID","SampleWeight")

names(effectvar)


traits <- colnames(zie_282[ ,!effectvar])

summary(zie_282)
                          
traits
zie_282=zie_282[,-c(1,2)]

summary(zie_282)

Line=as.factor(zie_282$pedigree)

blues2015<- as.data.frame(lapply(traits, blues.rb, dat = zie_282))

names(blues2015)

head(blues2015)

write.csv(blues2015,file="BLUE_mineral_ziegler.csv")


blue <-read.csv("BLUE_mineral_ziegler.csv", header=T)

pheno282 <- read.csv("pheno_set282.csv",skip = 1,header=T,na.strings=NA)

setwd("~/Documents/projects/zmhmp")
getwd()

?setwd()


pheno <- merge(blue,pheno282)
head(pheno282)
install.packages("compare")
library(compare)

#comparison <- compare(blue,pheno282,allowAll = T)
#head(comparison$tM)

#?compare

#blue$included_pheno282 <-TRUE

#pheno282$included_blue <-TRUE

#res=merge(blue,pheno282,all=T)

#length(unique(pheno282[1,]))

#pheno282[1,] <- as.factor(pheno282[1,])

#any(pheno282[,2]==blue[,1])

#pheno282[,2]         
#blue[,1]
colnames(pheno282)
colnames(blue)

####For merging panzea data to ziegler data this works. However, I want to get the BLUEs for panzea data before merging.
pheno=merge(blue,pheno282,by.x = "X", by.y="X.Trait.",all.x=TRUE)


str(pheno282)


tail(pheno282)


install.packages("reshape2")
library(reshape2)
str(Plant_height)
attach(Plant_height)
?recast


#subset plant ht
pheno282 <- read.csv("pheno_set282.csv",skip = 1,header=T,na.strings=NA)

Plant_height=pheno282[,-c(1,13:37)]
head(Plant_height)


head(Plant_height)


pheno282[1,]

head(pheno282)

pht=melt(Plant_height,id="X.Header.name.env.")

head(pht)
tail(pht)

#pheno282_test <- read.csv("pheno_set282.csv",header=T,na.strings=NA)
#names(pheno282_test)


#subset starch
Starch=pheno282[,-c(1,3:12,19:37)]
head(Starch)
starch=melt(Starch,id="X.Header.name.env.")
head(starch)
tail(starch)

#subset protein

protein=pheno282[,-c(1,3:18,25:37)]

head(protein)
protein=melt(protein,id="X.Header.name.env.")
head(protein)
tail(protein)

# subset NIRoil
oil=pheno282[,-c(1,3:24,31:37)]

head(oil)
oil=melt(oil,id="X.Header.name.env.")
head(oil)
tail(oil)

#subset 20Kernel weight


kw=pheno282[,-c(1,3:30)]

head(kw)
kw=melt(kw,id="X.Header.name.env.")
head(kw)
tail(kw)


getwd()
setwd("/Users/rkastooriramamurth2/Documents/projects/zmhmp/")
all_pheno <- read.csv("all_pheno282_panzea.csv",skip=1,header=T,na.strings = "NaN")
head(all_pheno)
names(all_pheno)

#yield traits ear row number, fill%, kernal volume, ear weight

KRN <-all_pheno[1:286,c(1,248:255)]
head(KRN)
tail(KRN)
KRN=melt(KRN,id="X.Header.name.env.")

#chk <- all_pheno[1:289,]
#tail(chk)

Ear_weight <-all_pheno[1:286,c(1,262:269)]
head(Ear_weight)
tail(Ear_weight)
Ear_weight=melt(Ear_weight,id="X.Header.name.env.")








