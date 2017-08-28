# R-Codes
K_nn.R
  
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
 K=3 yields a minimum classification error rate of 12%, which is quite low. In other words, 88% of the time, the algorithm correctly classifies the purchaser as having good credit when he has good credit and bad credit when he actually has bad credit. 
 


  #CODE
  The file K_nn.R contains the raw hand coded function (K_nn) for executing both a K-nearest neighbors classification   algorithm for any dataset coded as a matrix and split into training data and test data.
  
  The file also contains a K-means clustering of the same dataset 
