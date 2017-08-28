set.seed(123)

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

cred_dat_preds=cred_dat[,c(1:20)]


cred_dat_preds=data.frame(cred_dat_preds)
cred_dat_preds_matrix=as.matrix(cred_dat_preds,ncol=20)

cred_dat_matrix_centered=matrix(nrow=100,ncol=20)
#standardize by column of matrix
for (h in 1:20){
cred_dat_preds_matrix_centered[,h]=(cred_dat_preds_matrix[,h]-mean(cred_dat_preds_matrix[,h]))/sd(cred_dat_preds_matrix[,h])
}

cred_dat_matrix_centered=cbind(cred_dat_preds_matrix_centered,cred_dat[,21])

colnames(cred_dat_matrix_centered)=c("checking_status","duration_mos","cred_hist","purpose","cred_amt","savings_account","duration_empl","installment_rate","personal_status","guarantor","duration_residence","property_types","age","other_installment","housing","no_exist_credits","job","number_liable","telephone_register","foreigner","credit")


#split into test and training data as inputs to the algorithm 
data_train=cred_dat_matrix_centered[1:800,c(2,5,8,11,13,16,21)]
data_test=cred_dat_matrix_centered[801:1000,c(2,5,8,11,13,16)]
k=3

#K_nearest neighbors algorithm to determine credit of 200 "new" purchasers -1 is good credit and 0 is bad credit 

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
  y_values=c()
  #for each column of distances, order the distances and choose indices of Y that correespond to the indices of rows with k smallest distance values form the test points 
  for(h in 1:dim(distance)[2]){
    Y_vals[,h]=Y[order(distance[,h])[1:k]]
    groups[h]=sum(Y_vals[,h]==1)/k
    if(groups[h] >=.50){
      y_values[h]=1
    }
    else{
      y_values[h]=0
    } 
  
  }
  #classify the group to which test point in column h belongs by summing over the column entries and dividing by the row dimension of y-ie k
  return(y_values) #first entry corresponds to first test-point, second to second test point, etc. 
  
}

#calculate k nearest neighbors using credit card data:
result=K_nn(data_train,k,data_test)

#The error rate is then calculated via:

error_rate=sum(result-cred_dat_matrix_centered[801:1000,21])/dim(data_test)[1]

####ERROR RATE OF .12 is quite low!!!###


#using the built-in R functions for clustering the credit data:
#K-means clustering algorithm:
#(1) Specify the desired number of clusters: K
#(2)Random assign each data observation (row of the matrix) to one of the K-clusters
#(3)Compute the cluster centroids-mean, median, etc.
#(4) Reassign each observation (row of the data matrix) to the closest centroid so to minimize the within cluster sum of squared distances form the centroid
#(5) Recompute cluster centroids

#consider the within group sum of squares(distance between cluster centroid(mean) and each observation) for solutions with two to fifteen clusters 

credit_data=na.omit(cred_dat_matrix_centered)
wss <- (nrow(credit_data)-1)*sum(apply(credit_data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(credit_data, 
                                    centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within Cluster SS")

#want a solution that minimizes the within groups sums of squares but does not want to create so many groups/clusters that it is difficult to interprret those clusters meanings
#The plot shows four clusters is probably a good negotiation between interpretability and reduced within group sum of squares
#Eight clusters does a better job at minimizing sum of squares but whether or not it reflects actual data features is questionable 
#Four clusters shows up as being quite good as well-and probably more interpretable

fit_eight=kmeans(credit_data,8) #eight cluster solution 

aggregate(credit_data,by=list(fit_eight$cluster),FUN=mean) #cluster_means for eight cluster solution 

#group.1 checking_status duration_mos  cred_hist     purpose     cred_amt savings_account duration_empl installment_rate
#1       1       0.3888161  -0.17128023 -0.0860693 -0.06319406 -0.187472994      0.38377335   -0.11007101      0.453175279
#2       2      -0.1274877  -0.78667214  0.1379759 -0.14848500 -0.411414031     -0.13388806    0.37064233     -0.262244950
#3       3       0.5326140   0.09254536  0.9050860 -0.09587656 -0.025993186     -0.01399595    0.06474988     -0.369923457
#4       4      -0.1295584   0.04821162 -0.3228521  0.33008362 -0.007824365      0.14126179    0.08500098     -0.008210884
#5       5      -0.3689502  -0.38222785 -0.5136085 -0.11543054 -0.382870924     -0.52353248    0.01026090     -0.780950600
#6       6      -0.2545401  -0.26127829 -0.4438843 -0.12175843 -0.318846392     -0.43169079   -0.34125100      0.406424489
#7       7      -0.1009830  -0.39166371  0.9371079 -0.11766956 -0.289835020      0.54290872    0.15252512      0.747473610
#8       8      -0.3834675   1.85862297 -0.4643021  0.38065593  1.982709888     -0.01289321    0.07745184     -0.012159858
#personal_status   guarantor duration_residence property_types         age other_installment     housing no_exist_credits
#1       0.2246302 -0.22915132          0.4174358    -0.69578594  0.52630754         0.4174358  0.31518559      -0.69578594
#2      -0.6847314  0.62683811          0.1456599    -0.03135285 -0.02357900         0.1456599 -0.33693241      -0.03135285
#3      -0.1781683 -0.13129727          0.3529624     1.03752182 -0.33509150         0.3529624 -0.03314285       1.03752182
#4       0.2096011  0.15438360         -2.3295741    -0.01482288  0.04883865        -2.3295741  0.14834908      -0.01482288
#5      -0.5022729  0.32328455          0.3324889    -0.65567122 -0.49896349         0.3324889  0.13364358      -0.65567122
#6      -0.2535275 -0.11148459          0.4475981    -0.41869733 -0.73029676         0.4475981 -1.74866041      -0.41869733
#7       0.1828208 -0.15700027          0.2338432     1.63246379  1.10711923         0.2338432  0.43481222       1.63246379
#8       0.8418374 -0.06114747          0.2517451    -0.26723315  0.01122452         0.2517451  0.62898674      -0.26723315
#      job          number_liable     tele_register foreigner credit
#1  0.255603460    -0.1336728         0.30410759 -0.1959163 0.8324873
#2 -0.575603099     0.4158008        -0.59658431  5.0991176 0.7500000
#3  0.137192427    -0.1658814         0.06662456 -0.1959163 0.8987342
#4 -0.008510375     0.2407924         0.02049627 -0.1959163 0.7265625
#5 -0.345821710    -0.1004070        -0.51219312 -0.1959163 0.9774011
#6 -0.218067726    -0.2507136        -0.26229245 -0.1959163 0.0000000
#7 -0.250912633     0.3728398         0.13443516 -0.1959163 0.6600000
#8  0.630019396     0.1533513         0.57075980 -0.1401791 0.5052632


#Create a hierarchical clustering algorithm:

#dist_matrix=dist(cred_dat, method = "euclidean") # distance matrix
#fit=hclust(dist_matrix, method="ward.D2") #hierarchical cluster
#plot(fit) 
#groups=cutree(fit, k=8) # cut tree into 5 clusters
#rect.hclust(fit, k=8, border="blue")