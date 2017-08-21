install.packages("Amelia", repos='http://cran.us.r-project.org')
#install.packages("Amelia", repos="http://r.iq.harvard.edu", type = "source")
library("Amelia")

df = read.csv("GSScleaned.csv")[, -1]
noms = read.csv("GSScategoricals.csv")[, -1]
ords = read.csv("GSSordinals.csv")[, -1]

rho = 0.6

xdf = c(1:(dim(df)[1]*dim(df)[2]))
ndf = nrow(df)
DF = df

sdf1 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf1)){
	ind = sdf1[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF[r,c] = NA}	


a = amelia(DF, m = 1, noms = noms, ords = ords, p2s = 2, tolerance = 0.001) 

am1 = as.data.frame(a$imputations[1])

save(am1, sdf1, file = "amelia_GSS60.RData" )



