#Assignment #1# 

##The following functions investigate features of observations from several files-each file containing a data frame 
#with nitrate and sulfate levels on different days for a particular air quality sensor. ##

#The "pollutantmean" function calculates the mean of airquality observations over a selected number of air quality sensors 
pollutantmean=function(directory,pollutant,id=1:332){
  fileList=list.files(path=directory,pattern=".csv",full.names=TRUE) #directory is "C://Users//I864933//Desktop//specdata"
  values=numeric() #empty vector with numeric entries , retains the information 
  for(i in id){
    data=read.csv(file=fileList[i]) #get the data from the ith file 
    values=c(values,data[[pollutant]]) 
  }
  mean(values,na.rm=TRUE) #remove NA values 
}

#The "complete" function counts the number of complete entries in each of the air qwuality sensor files 
complete=function(directory,id=1:332){
  fileList=list.files(path=directory,pattern=".csv",full.names=TRUE)
  nobs=numeric()
  ids=numeric()
  for(i in id){
   data=read.csv(file=fileList[i]) 
   nobs=c(nobs,sum(complete.cases(data))) 
   ids=c(ids,i)
  }
  df=data.frame(ids,nobs)
  df
} 


#The "corr" function calculates correlation between sulfate and nitrate levels for sensors with at least one complete observation-ie both sulfate and nitrate level record
corr=function(directory,threshold=0){
  fileList=list.files(path=directory,pattern=".csv",full.names=TRUE)
  corrs=numeric() #vector to store correlations
  for(i in 1:332){
    data=read.csv(file=fileList[i])
    if(sum(complete.cases(data))>threshold){
    corrs=c(corrs,cor(data[["nitrate"]],data[["sulfate"]],use="complete.obs"))
    } else {
    corrs=c(corrs)
    }
  }
 corrs 
}

