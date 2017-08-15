#Estimating average total snow per year,mu, for East Coast City 
library(LearnBayes)
#data: y_{i} is the snowfall total observed in year i 
y=c(38.6,42.4,57.5,40.5,51.7,67.1,33.4,60.9,64.1,40.1,40.7,6.4)
ybar=mean(y)
#sampling distribution: (y_{i}|mu) ~ Normal(mu,sigma=10)
#prior: a range from 20 to 70. 

#Construct a mesh over the possible values for mu-500 values equally spaced on [20,70]

mu_mesh_vals=seq(from=20,to=70,length=500)

#compute the prior over this mesh of values
midpts=seq(from=15,to=65,by=10) #midpoints of histogram prior  
prior=c(.10,.15,.25,.25,.15,.10)
prior=histprior(mu_mesh_vals,midpts,prior) #prior values

#get the sampling distribution function over the mesh-normal likelihood 

likelihood=dnorm(mu_mesh_vals,mean=ybar,sd=10,log=FALSE)

#posterior is the product of the prior and sampling dsistribution over the mesh of mu values

posterior=prior*likelihood 
posterior=posterior/sum(posterior) #normalize

#take a sample from the posterior on the grid to get samples of the posterior-draws from posterior-ie mu values
ps=sample(mu_mesh_vals,replace=TRUE,prob=posterior)

#plot simulated mu values-posterior sample

plot(ps,pch=19,main="Mean Annual Snowfall",ylab="Mean Amt(In)",xlab="Sample Number")
abline(a=27.00902,b=0,col="red")
abline(a=62.67285,b=0,col="red")

#create inference on the posterior-95% credibility interval for mu:

cred_int_.95=quantile(ps,c(.025,.975))


#Let's construct a posterior predictive sampling scheme for new draws of data of ten observations:
  #Draw p* samples from g(mu*|y)
  #Draw y samples from the sampling distribution:N(mu=mu*,sigma=10)

  posterior_pred=function(len,data){ #choose the length of the mesh-ie the number of samples taken 
    mesh=seq(20,70,length=len)
    midpts=seq(from=15,to=65,by=10) 
    prior=c(.10,.15,.25,.25,.15,.10)
    prior=histprior(mesh,midpts,prior)
    likelihood=dnorm(mesh,mean=mean(data),sd=10,log=FALSE) #data can be a vecotr of any length 
    posterior=prior*likelihood 
    posterior=posterior/sum(posterior)
    post_sample=sample(mesh,replace=TRUE,prob=posterior)
    #mu*s are the ps, which get fed into the sampling distribution to simulate those draws 
    #predictive draws-sample size of twelve predicted observations for each posterior mu* value 
    pred=vector("list",length=length(post_sample))
    for (i in 1:length(post_sample)){
      pred[[i]]=rnorm(12,mean=post_sample[i],sd=10)
       }
    plot(pred[[1]],main="Posterior Predictive Samples",xlab="Observation",ylab="Value")
    for(i in 1:length(post_sample)){
      lines(pred[[i]],col=i)
      }
    return(pred)
    }
  
#run to get using the actual snow data given
  
length=100000
data=c(38.6,42.4,57.5,40.5,51.7,67.1,33.4,60.9,64.1,40.1,40.7,6.4)
ys=posterior_pred(length,data)
ys=unlist(ys)
ys_round=round(ys)
freq=table(ys_round)
ys_val=as.integer(names(freq))
prob=freq/sum(freq)
post_pred_distr=cbind(ys_val,prob)

#credibility interval for the posterior predictive distribution that covers 95% of observed values. 
covprob=.95
discint(post_pred_distr,covprob)