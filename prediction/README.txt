Two datasets: GSS subset and NLSY subset. Within each folder, the following sub-folders
exist:

1) Data -- in this folder, the code for creating the cleaned subsets from the original 
data is presented. For eg: NLSY -> Data -> nlsymakedata.R  has the code for creating 
the cleaned dataset, NLSYcleaned.csv and the lists of categorical and ordinal columns,
NLSYcategoricals.csv and NLSYordinals.csv

From the cleaned data, 9 datasets are created where 10%, 20%, … 90% datapoints are held out. For eg: NLSY10.csv, NLSY20.csv, … , NLSY90.csv. These sparse datasets are fixed for the remainder of the “prediction” analysis.


2) Amelia -- this folder contains:
a) the code for completing these datasets using Amelia.  
b) the resulting completed datasets (20 for each sparse dataset)

3) MICE -- this folder contains:
a) the code for generating datasets missing 10%, 20%, ... 90%  random datapoints 
and then completing these datasets using MICE.  
b) the resulting completed datasets (20 for each sparse dataset)

4) Rank -- this folder contains:
a) the code for generating datasets missing 10%, 20%, ... 90%  random datapoints 
and then completing these datasets using LowRankModels and constraining Rank.  
b) the resulting completed datasets (1 for each sparse dataset)

5) Trace -- this folder contains:
a) the code for generating datasets missing 10%, 20%, ... 90%  random datapoints 
and then completing these datasets using LowRankModels and constraining Trace norm. 
b) the resulting completed datasets (1 for each sparse dataset)


6) Comparisons -- this folder contains:
a) code for comparing the losses from all 4 methods
b) figures and tables summarizing the comparisons
