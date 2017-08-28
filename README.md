# K_NN.R(In Progress)
  
  1000 German records of credit card purchases-each by a different individual (n=1000) available from:
  https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data
 
  I recoded all categorical variables as numerical variables with categories corresponding to unique integer values  in my R script because the numerically coded values available from:
  https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric
  
  did not make sense in the context of the categorically implied values coded in the original data. 
 
 According to the documentation accompanying the dataset found here:
 https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.doc
 
 I coded the variables as follows:
 
  Dependent variable: 
    The one response variable is coded as follows:
    * "Credit" -does the person pay off the purchase? 
      yes=1, no=0 
  
  Independent Variables: 
    The 20 predictor variables are coded as follows: 
      * "checking status" -status of existing checking account (categorical)
      * "duration_mos": duration of purchase in months (numerical-integer)
      * "cred_hist": credit history  (categorical)
      * "purpose": type of good being purchased (categorical)
      * "credit_amt": credit card amount (numerical)
      * "savings_account": amount in bonds and savings in binned dollar ranges (categorical)
      * "duration_empl": how long the individual has been employed at his or her current job in binned ranges (categorical)
      * "installment_rate": interest rate as a percent of disposible income (numerical)
      * "personal status" : gender and marital status (categorical)
      * "guarantor" : debtor, coaplicant or guarantor in purcahse (categorical)
      * "duration_residence":  how long has the person lived at current residence in years (numerical-integer)
      * "proprty_types": 
      
 Training Data:
  First 800 rows of the data 
 
 Test Data:
  Last 200 rows of the data 
  
  
  
 #Results
 K=3 yields a minimum classification error rate of 12%, which is quite low. In other words, 88% of the time, the algorithm correctly classifies the purchaser as having good credit when he has good credit and bad credit when he/she actually has bad credit. 
 
 The K-means clustering algorithm settled on classfying the 1000 debtors into 8 different types based on minimizing the amount of variation within the clusters of debtors-coded as wss in the script. 
 
 There is always a tradeoff between the number of clusters to form and the interpretability of the clusters. Increasing the number of clusters decreases the variation within groups but has no interpretability because at some point every debtor has his or her own category! This is why I settled on the 8 cluster solution because increasing the number of clusters from the point decreases model performance as seen in the graph  (in the PDF"Clusters" stored in this responsitory) which plots the within group sums of squares as a function of the nuber of clusters. 
 
 Additionally, if you look at the values of each variable for each cluster (in the large commented section of the R code) you will notice a distinct and expected trend between those who are less likely to repay their purchase and those who are more likely to repay. 
 


 



