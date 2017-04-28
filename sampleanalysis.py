#cd "D:\Users\Pc\Documents\Python"

import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import statsmodels.api as sm
import externalfunction as exef

# Data import
df = pd.read_csv("http://www.ats.ucla.edu/stat/data/binary.csv") # reads the dataframe from ucla

# renaming the df coloumns
df.columns = ["admit", "gre", "gpa", "prestige"]

print(df.head())
print(df.describe())

#df.loc[df.admit == 2, "admit"] = 0

######## Tables #########

print(pd.crosstab(df.admit, df.prestige))

###### Plotting ######

df.hist()
plt.show()

###### Making dummy variables #####

dummy_ranks = pd.get_dummies(df.prestige, prefix= "prestige")
dummy_ranks.head()

# data frame with dummy variables
cols_to_keep = ["admit", "gre", "gpa"]
data = df[cols_to_keep].join(dummy_ranks.loc[:, "prestige_2":])
data["intercept"] = 1
print(data.head())

###### STATISTICAL MODEL #######
print "_"*20 + "\n"
print "Fitting the logistic regression model..."

train_cols = data.columns[1:]
result = sm.Logit(data.admit, data[train_cols]).fit()

print result.summary()

#### ODDS RATIOS ####
print "=" * 50 + "\nGiving a look to the Odds ratios"

exef.OR().OddsRatio(result)
