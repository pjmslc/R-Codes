#Read in Credit Card Data Set from Website

cred_dat=read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric",header=FALSE)
cred_cat=read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data",header=FALSE)
cred_hist=as.character(cred_cat[,3])
#remove the 'A' form the front of entries and make them numeric
cred_hist=sapply(strsplit(cred_hist, split='A', fixed=TRUE), function(x) (x[2]))
cred_hist=as.numeric(cred_hist)
cred_dat[,3]=cred_hist-30

cred_purpose=as.character(cred_cat[,4])
cred_purpose=sapply(strsplit(cred_purpose, split='A', fixed=TRUE), function(x) (x[2]))
cred_purpose=as.numeric(cred_purpose)
cred_dat[,4]=cred_purpose-40

cred_guarantor=as.character(cred_cat[,10])
cred_guarantor=sapply(strsplit(cred_guarantor, split='0', fixed=TRUE), function(x) (x[2]))
cred_guarantor=as.numeric(cred_guarantor)
cred_dat[,10]=cred_guarantor


cred_amt=cred_cat[,5]
cred_dat[,5]=cred_amt

cred_dat[,13]=cred_cat[,13]

cred_installments=as.character(cred_cat[,14])
cred_installments=sapply(strsplit(cred_installments, split='4', fixed=TRUE), function(x) (x[2]))
cred_dat[,14]=as.numeric(cred_installments)

cred_housing=as.character(cred_cat[,15])
cred_housing=sapply(strsplit(cred_housing, split='5', fixed=TRUE), function(x) (x[2]))
cred_dat[,15]=as.numeric(cred_housing)

cred_dat[,16]=cred_cat[,16]

cred_job=as.character(cred_cat[,17])
cred_job=sapply(strsplit(cred_job, split='7', fixed=TRUE), function(x) (x[2]))
cred_dat[,17]=as.numeric(cred_job)

cred_dat[,18]=cred_cat[,18]

cred_phone=as.character(cred_cat[,19])
cred_phone=sapply(strsplit(cred_phone, split='9', fixed=TRUE), function(x) (x[2]))
cred_dat[,19]=as.numeric(cred_phone)

cred_foreign=as.character(cred_cat[,20])
cred_foreign=sapply(strsplit(cred_foreign, split='0', fixed=TRUE), function(x) (x[2]))
cred_dat[,20]=as.numeric(cred_foreign)

cred_dat=cred_dat[,c(1:20,25)]
cred_dat=data.frame(cred_dat)
cred_dat_matrix=as.matrix(cred_dat,ncol=21)
#colnames(cred_dat_matrix)=c("checking_status","duration_mos","cred_hist","purpose","cred_amt","savings_account","duration_empl","installment_rate","personal_status","guarantor","duration_residence","property_types","age","other_installment","housing","no_exist_credits","job","number_liable","telephone_register","foreigner","purchase_good_bad")


#normalize every column of the data in cred_dat_matrix:


#split into test and training data as inputs to the algorithm 
data_train=cred_dat_matrix[1:800,c(2,5,8,11,13,16,21)]
data_test=cred_dat_matrix[801:1000,c(2,5,8,11,13,16)]
k=2

#K_nearest neighbors algorithm for a binary choice variable-Good Purchase(1) or Bad Purchase(2)

#training data must be read in a matrix (byrow=T) such that the first k-1 columns are the predictors and the kth column are the response variable values
#test data have same number of columns as X, and any number of rows (read in using byrow=T) for correct computation.

K_nn=function(data_train,k,data_test){
  
  #parses the training data and separates out final column as reponse variable Y
  X=data_train[,1:(dim(data_train)[2])-1] #3X3 matrix 
  Y=matrix(data_train[,dim(data_train)[2]],ncol=1,nrow=dim(data_train)[1])  
 
   #read in the test data as a matrix with same number of columns as training data
  data_test=matrix(data_test,byrow=T,ncol=dim(X)[2])
  #set up the distance matrix to collect distances between each test data point and each training observation
  distance=matrix(ncol=dim(data_test)[1],nrow=dim(X)[1])
  
  #set up matrix to store distances between test_obs and rows
    #-each column holds the distances between a particular test_obs and training data observation
    #-each row corresponds to the respective distance between that test-observation in the column and the corresponding row of X
  for(j in 1:dim(data_test)[1]){
     for (i in 1:dim(X)[1]){ 
        distance[i,j]=dist(rbind(data_test[j,],X[i,]),method='minkowski')
     }
  }
#matrix for y-points selected-each test point corresponds to a column, k y outputs that are closest to that test point are the vlaues in the column 
Y_vals=matrix(nrow=k,ncol=dim(distance)[2])
#vector calculating the probability of the belonging of each test point to group Y==0 
groups=c()
#for each column of distances, order the distances and choose indices of Y that correespond to the indices of rows with k smallest distance values form the test points 
  for(h in 1:dim(distance)[2]){
    Y_vals[,h]=Y[order(distance[,h])[1:k]]
    groups[h]=sum(Y_vals[,h]==2)/k
  }
#classify the group to which test point in column h belongs by summing over the column entries and dividing by the row dimension of y-ie k
return(groups) #first entry corresponds to first test-point, second to second test point, etc. 

}

#calculate k nearest neighbors using credit card data:
 result=K_nn(data_train,k,data_test)

