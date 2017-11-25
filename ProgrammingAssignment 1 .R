#Assignment #1# 

#Skills list#

#Skill 1: Append or add values to a vector#

nums=c(2,20,43,21)

dups=numeric() #initialize an empty numeric vector

for(num in nums){
 dups=c(dups,num) #takes dups and adds each element of nums onto dups
}

#Skill 2: Access a column of a data frame
#data[["sulfate]] gives the sulfate column of the data frame
#mean(data[["sulfate]],na.rm=TRUE) gives the mean of the sulfate column 


pollutantmean=function(directory,pollutant,id=1:332){
  fileList=list.files(path=directory,pattern=".csv",full.names=TRUE) #directory is "C://Users//I864933//Desktop//specdata"
  values=numeric() #empty vector ewith numeric entries , retains the information 
  for(i in id){
    data=read.csv(file=fileList[i]) #get the data from the ith file 
    values=c(values,data[[pollutant]]) #allows us to save previously 
  }
  mean(values,na.rm=TRUE) #calculates the mean of all values of a certain pollutant over multiple files
}

#Counts the number of complete entries in files 
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


#calculates correlations between observations 
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

cr=corr("C://Users//I864933//Desktop//specdata",2000)
n=length(cr)
cr=corr("C://Users//I864933//Desktop//specdata",1000)
cr=sort(cr)

print(c(n,round(cr,4)))