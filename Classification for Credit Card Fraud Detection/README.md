# Data Source
```
https://www.kaggle.com/mlg-ulb/creditcardfraud
```

# Goal
Build a model that can accurately predict when a fraudulent transaction takes place. Due to the heavy class imbalance, we will be measuring accuracy of the model using the Area under the Precision Recall Curve (AUPRC), or the precision score.

# Results
Random Forest Classifier creates best model.

# Tasks Performed

Preprocessed highly imbalanced dataset
Used SMOTE, under & hybrid sampling on dataset

Ran 5 feature selection algorithm

Ran 5 classifiers Random Forest, Simple Logistic, J48, Naïve Bayes, Neural Networks

# Tools Used
R,    Weka,   JMP

# Refer
Detailed Report for description
Summary Report for method

# Minor points to check always
Keep training data and test data independent.

Training-Test data breakup is 75-25 or 80-20 depends on tuples and class distribution. We are using 70-30.

For unbalanced dataset sampling is performed.

Testing always done on original data not sampled data.

# Description
The datasets contains transactions made by credit cards in September 2013 by European cardholders.

This dataset presents transactions that occurred in two days, where we have 492 frauds out of 284,807 transactions. The dataset is highly unbalanced, the positive class (frauds) account for 0.172% of all transactions.

It contains only numerical input variables, which are the result of a PCA transformation. Due to confidentiality issues, the original features and more background information about the data cannot be provided.

Features V1, V2, ..., V28 are the principal components obtained with PCA, the only features which have not been transformed with PCA are 'Time' and 'Amount'. Feature 'Time' contains the seconds elapsed between each transaction and the first transaction in the dataset. The feature 'Amount' is the transaction Amount, this feature can be used for example-dependant cost-senstive learning.

Feature 'Class' is the response variable and it takes value 1 (false transaction) in case of fraud and 0 otherwise.

