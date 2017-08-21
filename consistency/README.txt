Two datasets: GSS subset and NLSY subset. Within each folder, the following sub-folders
exist:

1) Data -- in this folder, the code for creating the cleaned subsets from the original 
data is presented. For eg: NLSY -> Data -> nlsymakedata.R  has the code for creating 
the cleaned dataset, NLSYcleaned.csv and the lists of categorical and ordinal columns,
NLSYcategoricals.csv and NLSYordinals.csv

2) Amelia -- this folder contains:
a) the code for generating datasets missing 10%, 20%, ... 90%  random datapoints 
and then completing these datasets using Amelia.  (20 for each sparsity level)  
b) the resulting completed datasets (20 for each sparsity level)
c) the summarized losses from these datasets in amelia_consistency.RData

3) MICE -- this folder contains:
a) the code for generating datasets missing 10%, 20%, ... 90%  random datapoints 
and then completing these datasets using MICE.  (20 for each sparsity level) 
b) the resulting completed datasets (20 for each sparsity level)
c) the summarized losses from these datasets in mice_consistency.RData

4) Rank -- this folder contains:
a) the code for generating datasets missing 10%, 20%, ... 90%  random datapoints 
and then completing these datasets using LowRankModels and constraining Rank.  (20 for each sparsity level)  
b) the resulting completed datasets (20 for each sparsity level)
c) the summarized losses from these datasets in rank_consistency.RData

5) Trace -- this folder contains:
a) the code for generating datasets missing 10%, 20%, ... 90%  random datapoints 
and then completing these datasets using LowRankModels and constraining Trace norm.  (20 for each sparsity level) 
b) the resulting completed datasets (20 for each sparsity level)
c) the summarized losses from these datasets in trace_consistency.RData


6) Comparisons -- this folder contains:
a) code for summarizing losses from different completed matrices from all 4 methods
b) code for comparing the losses from all 4 methods
c) figures and tables summarizing the comparisons
