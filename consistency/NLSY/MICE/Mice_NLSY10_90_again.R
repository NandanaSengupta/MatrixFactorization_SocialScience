# MICE

rm(list = ls())
library("mice")


#### NLSY10 
rho = 0.1
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/Data")

df = read.csv("NLSYcleaned.csv")[, -1]
xdf = c(1:(dim(df)[1]*dim(df)[2]))
ndf = nrow(df)
DF1 = df
DF2 = df
DF3 = df
DF4 = df
DF5 = df
DF6 = df
DF7 = df
DF8 = df
DF9 = df
DF10 = df

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/MICE/again")

sdf1 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf1)){
	ind = sdf1[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF1[r,c] = NA}	
		
imp1  = mice(DF1, m = 1, pred = quickpred(DF1), maxit = 20)


sdf2 = sample(xdf, round(rho*length(xdf)))
for ( i in 1:length(sdf2)){
	ind = sdf2[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF2[r,c] = NA}	
		
imp2  = mice(DF2, m = 1, pred = quickpred(DF2), maxit = 20)



sdf3 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf3)){
	ind = sdf3[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF3[r,c] = NA}	
		
imp3  = mice(DF3, m = 1, pred = quickpred(DF3), maxit = 20)


sdf4 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf4)){
	ind = sdf4[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF4[r,c] = NA}	
		
imp4  = mice(DF4, m = 1, pred = quickpred(DF4), maxit = 20)


sdf5 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf5)){
	ind = sdf5[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF5[r,c] = NA}	
		
imp5  = mice(DF5, m = 1, pred = quickpred(DF5), maxit = 20)


sdf6 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf6)){
	ind = sdf6[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF6[r,c] = NA}	
		
imp6  = mice(DF6, m = 1, pred = quickpred(DF6), maxit = 20)



sdf7 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf7)){
	ind = sdf7[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF7[r,c] = NA}	
		
imp7  = mice(DF7, m = 1, pred = quickpred(DF7), maxit = 20)


sdf8 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf8)){
	ind = sdf8[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF8[r,c] = NA}	
		
imp8  = mice(DF8, m = 1, pred = quickpred(DF8), maxit = 20)



sdf9 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf9)){
	ind = sdf9[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF9[r,c] = NA}	
		
imp9  = mice(DF9, m = 1, pred = quickpred(DF9), maxit = 20)



sdf10 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf10)){
	ind = sdf10[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF10[r,c] = NA}	
		
imp10  = mice(DF10, m = 1, pred = quickpred(DF10), maxit = 20)


W1 = complete(imp1, action = 1)
W2 = complete(imp2, action = 1)
W3 = complete(imp3, action = 1)
W4 = complete(imp4, action = 1)
W5 = complete(imp5, action = 1)
W6 = complete(imp6, action = 1)
W7 = complete(imp7, action = 1)
W8 = complete(imp8, action = 1)
W9 = complete(imp9, action = 1)
W10 = complete(imp10, action = 1)


save(W1, sdf1,  file = "MICE_NLSY10_1.RData" )
save(W2, sdf2, file = "MICE_NLSY10_2.RData" )
save(W3, sdf3, file = "MICE_NLSY10_3.RData" )
save(W4, sdf4, file = "MICE_NLSY10_4.RData" )
save(W5, sdf5, file = "MICE_NLSY10_5.RData" )
save(W6, sdf6, file = "MICE_NLSY10_6.RData" )
save(W7, sdf7, file = "MICE_NLSY10_7.RData" )
save(W8, sdf8, file = "MICE_NLSY10_8.RData" )
save(W9, sdf9, file = "MICE_NLSY10_9.RData" )
save(W10, sdf10, file = "MICE_NLSY10_10.RData" )



rm(list = ls())


# rho = 20

rho = 0.2
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/Data")

df = read.csv("NLSYcleaned.csv")[, -1]
xdf = c(1:(dim(df)[1]*dim(df)[2]))
ndf = nrow(df)
DF1 = df
DF2 = df
DF3 = df
DF4 = df
DF5 = df
DF6 = df
DF7 = df
DF8 = df
DF9 = df
DF10 = df

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/MICE/again")

sdf1 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf1)){
	ind = sdf1[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF1[r,c] = NA}	
		
imp1  = mice(DF1, m = 1, pred = quickpred(DF1), maxit = 20)


sdf2 = sample(xdf, round(rho*length(xdf)))
for ( i in 1:length(sdf2)){
	ind = sdf2[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF2[r,c] = NA}	
		
imp2  = mice(DF2, m = 1, pred = quickpred(DF2), maxit = 20)



sdf3 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf3)){
	ind = sdf3[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF3[r,c] = NA}	
		
imp3  = mice(DF3, m = 1, pred = quickpred(DF3), maxit = 20)


sdf4 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf4)){
	ind = sdf4[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF4[r,c] = NA}	
		
imp4  = mice(DF4, m = 1, pred = quickpred(DF4), maxit = 20)


sdf5 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf5)){
	ind = sdf5[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF5[r,c] = NA}	
		
imp5  = mice(DF5, m = 1, pred = quickpred(DF5), maxit = 20)


sdf6 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf6)){
	ind = sdf6[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF6[r,c] = NA}	
		
imp6  = mice(DF6, m = 1, pred = quickpred(DF6), maxit = 20)



sdf7 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf7)){
	ind = sdf7[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF7[r,c] = NA}	
		
imp7  = mice(DF7, m = 1, pred = quickpred(DF7), maxit = 20)


sdf8 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf8)){
	ind = sdf8[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF8[r,c] = NA}	
		
imp8  = mice(DF8, m = 1, pred = quickpred(DF8), maxit = 20)



sdf9 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf9)){
	ind = sdf9[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF9[r,c] = NA}	
		
imp9  = mice(DF9, m = 1, pred = quickpred(DF9), maxit = 20)



sdf10 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf10)){
	ind = sdf10[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF10[r,c] = NA}	
		
imp10  = mice(DF10, m = 1, pred = quickpred(DF10), maxit = 20)


W1 = complete(imp1, action = 1)
W2 = complete(imp2, action = 1)
W3 = complete(imp3, action = 1)
W4 = complete(imp4, action = 1)
W5 = complete(imp5, action = 1)
W6 = complete(imp6, action = 1)
W7 = complete(imp7, action = 1)
W8 = complete(imp8, action = 1)
W9 = complete(imp9, action = 1)
W10 = complete(imp10, action = 1)


save(W1, sdf1,  file = "MICE_NLSY20_1.RData" )
save(W2, sdf2, file = "MICE_NLSY20_2.RData" )
save(W3, sdf3, file = "MICE_NLSY20_3.RData" )
save(W4, sdf4, file = "MICE_NLSY20_4.RData" )
save(W5, sdf5, file = "MICE_NLSY20_5.RData" )
save(W6, sdf6, file = "MICE_NLSY20_6.RData" )
save(W7, sdf7, file = "MICE_NLSY20_7.RData" )
save(W8, sdf8, file = "MICE_NLSY20_8.RData" )
save(W9, sdf9, file = "MICE_NLSY20_9.RData" )
save(W10, sdf10, file = "MICE_NLSY20_10.RData" )



rm(list = ls())

# rho = 30

rho = 0.3
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/Data")

df = read.csv("NLSYcleaned.csv")[, -1]
xdf = c(1:(dim(df)[1]*dim(df)[2]))
ndf = nrow(df)
DF1 = df
DF2 = df
DF3 = df
DF4 = df
DF5 = df
DF6 = df
DF7 = df
DF8 = df
DF9 = df
DF10 = df

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/MICE/again")

sdf1 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf1)){
	ind = sdf1[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF1[r,c] = NA}	
		
imp1  = mice(DF1, m = 1, pred = quickpred(DF1), maxit = 20)


sdf2 = sample(xdf, round(rho*length(xdf)))
for ( i in 1:length(sdf2)){
	ind = sdf2[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF2[r,c] = NA}	
		
imp2  = mice(DF2, m = 1, pred = quickpred(DF2), maxit = 20)



sdf3 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf3)){
	ind = sdf3[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF3[r,c] = NA}	
		
imp3  = mice(DF3, m = 1, pred = quickpred(DF3), maxit = 20)


sdf4 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf4)){
	ind = sdf4[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF4[r,c] = NA}	
		
imp4  = mice(DF4, m = 1, pred = quickpred(DF4), maxit = 20)


sdf5 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf5)){
	ind = sdf5[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF5[r,c] = NA}	
		
imp5  = mice(DF5, m = 1, pred = quickpred(DF5), maxit = 20)


sdf6 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf6)){
	ind = sdf6[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF6[r,c] = NA}	
		
imp6  = mice(DF6, m = 1, pred = quickpred(DF6), maxit = 20)



sdf7 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf7)){
	ind = sdf7[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF7[r,c] = NA}	
		
imp7  = mice(DF7, m = 1, pred = quickpred(DF7), maxit = 20)


sdf8 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf8)){
	ind = sdf8[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF8[r,c] = NA}	
		
imp8  = mice(DF8, m = 1, pred = quickpred(DF8), maxit = 20)



sdf9 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf9)){
	ind = sdf9[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF9[r,c] = NA}	
		
imp9  = mice(DF9, m = 1, pred = quickpred(DF9), maxit = 20)



sdf10 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf10)){
	ind = sdf10[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF10[r,c] = NA}	
		
imp10  = mice(DF10, m = 1, pred = quickpred(DF10), maxit = 20)


W1 = complete(imp1, action = 1)
W2 = complete(imp2, action = 1)
W3 = complete(imp3, action = 1)
W4 = complete(imp4, action = 1)
W5 = complete(imp5, action = 1)
W6 = complete(imp6, action = 1)
W7 = complete(imp7, action = 1)
W8 = complete(imp8, action = 1)
W9 = complete(imp9, action = 1)
W10 = complete(imp10, action = 1)


save(W1, sdf1,  file = "MICE_NLSY30_1.RData" )
save(W2, sdf2, file = "MICE_NLSY30_2.RData" )
save(W3, sdf3, file = "MICE_NLSY30_3.RData" )
save(W4, sdf4, file = "MICE_NLSY30_4.RData" )
save(W5, sdf5, file = "MICE_NLSY30_5.RData" )
save(W6, sdf6, file = "MICE_NLSY30_6.RData" )
save(W7, sdf7, file = "MICE_NLSY30_7.RData" )
save(W8, sdf8, file = "MICE_NLSY30_8.RData" )
save(W9, sdf9, file = "MICE_NLSY30_9.RData" )
save(W10, sdf10, file = "MICE_NLSY30_10.RData" )



rm(list = ls())

# rho = 40

rho = 0.4
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/Data")

df = read.csv("NLSYcleaned.csv")[, -1]
xdf = c(1:(dim(df)[1]*dim(df)[2]))
ndf = nrow(df)
DF1 = df
DF2 = df
DF3 = df
DF4 = df
DF5 = df
DF6 = df
DF7 = df
DF8 = df
DF9 = df
DF10 = df

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/MICE/again")

sdf1 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf1)){
	ind = sdf1[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF1[r,c] = NA}	
		
imp1  = mice(DF1, m = 1, pred = quickpred(DF1), maxit = 20)


sdf2 = sample(xdf, round(rho*length(xdf)))
for ( i in 1:length(sdf2)){
	ind = sdf2[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF2[r,c] = NA}	
		
imp2  = mice(DF2, m = 1, pred = quickpred(DF2), maxit = 20)



sdf3 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf3)){
	ind = sdf3[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF3[r,c] = NA}	
		
imp3  = mice(DF3, m = 1, pred = quickpred(DF3), maxit = 20)


sdf4 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf4)){
	ind = sdf4[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF4[r,c] = NA}	
		
imp4  = mice(DF4, m = 1, pred = quickpred(DF4), maxit = 20)


sdf5 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf5)){
	ind = sdf5[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF5[r,c] = NA}	
		
imp5  = mice(DF5, m = 1, pred = quickpred(DF5), maxit = 20)


sdf6 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf6)){
	ind = sdf6[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF6[r,c] = NA}	
		
imp6  = mice(DF6, m = 1, pred = quickpred(DF6), maxit = 20)



sdf7 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf7)){
	ind = sdf7[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF7[r,c] = NA}	
		
imp7  = mice(DF7, m = 1, pred = quickpred(DF7), maxit = 20)


sdf8 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf8)){
	ind = sdf8[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF8[r,c] = NA}	
		
imp8  = mice(DF8, m = 1, pred = quickpred(DF8), maxit = 20)



sdf9 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf9)){
	ind = sdf9[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF9[r,c] = NA}	
		
imp9  = mice(DF9, m = 1, pred = quickpred(DF9), maxit = 20)



sdf10 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf10)){
	ind = sdf10[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF10[r,c] = NA}	
		
imp10  = mice(DF10, m = 1, pred = quickpred(DF10), maxit = 20)


W1 = complete(imp1, action = 1)
W2 = complete(imp2, action = 1)
W3 = complete(imp3, action = 1)
W4 = complete(imp4, action = 1)
W5 = complete(imp5, action = 1)
W6 = complete(imp6, action = 1)
W7 = complete(imp7, action = 1)
W8 = complete(imp8, action = 1)
W9 = complete(imp9, action = 1)
W10 = complete(imp10, action = 1)



save(W1, sdf1,  file = "MICE_NLSY40_1.RData" )
save(W2, sdf2, file = "MICE_NLSY40_2.RData" )
save(W3, sdf3, file = "MICE_NLSY40_3.RData" )
save(W4, sdf4, file = "MICE_NLSY40_4.RData" )
save(W5, sdf5, file = "MICE_NLSY40_5.RData" )
save(W6, sdf6, file = "MICE_NLSY40_6.RData" )
save(W7, sdf7, file = "MICE_NLSY40_7.RData" )
save(W8, sdf8, file = "MICE_NLSY40_8.RData" )
save(W9, sdf9, file = "MICE_NLSY40_9.RData" )
save(W10, sdf10, file = "MICE_NLSY40_10.RData" )


rm(list = ls())


# rho = 50

rho = 0.5
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/Data")

df = read.csv("NLSYcleaned.csv")[, -1]
xdf = c(1:(dim(df)[1]*dim(df)[2]))
ndf = nrow(df)
DF1 = df
DF2 = df
DF3 = df
DF4 = df
DF5 = df
DF6 = df
DF7 = df
DF8 = df
DF9 = df
DF10 = df

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/MICE/again")

sdf1 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf1)){
	ind = sdf1[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF1[r,c] = NA}	
		
imp1  = mice(DF1, m = 1, pred = quickpred(DF1), maxit = 20)


sdf2 = sample(xdf, round(rho*length(xdf)))
for ( i in 1:length(sdf2)){
	ind = sdf2[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF2[r,c] = NA}	
		
imp2  = mice(DF2, m = 1, pred = quickpred(DF2), maxit = 20)



sdf3 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf3)){
	ind = sdf3[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF3[r,c] = NA}	
		
imp3  = mice(DF3, m = 1, pred = quickpred(DF3), maxit = 20)


sdf4 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf4)){
	ind = sdf4[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF4[r,c] = NA}	
		
imp4  = mice(DF4, m = 1, pred = quickpred(DF4), maxit = 20)


sdf5 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf5)){
	ind = sdf5[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF5[r,c] = NA}	
		
imp5  = mice(DF5, m = 1, pred = quickpred(DF5), maxit = 20)


sdf6 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf6)){
	ind = sdf6[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF6[r,c] = NA}	
		
imp6  = mice(DF6, m = 1, pred = quickpred(DF6), maxit = 20)



sdf7 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf7)){
	ind = sdf7[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF7[r,c] = NA}	
		
imp7  = mice(DF7, m = 1, pred = quickpred(DF7), maxit = 20)


sdf8 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf8)){
	ind = sdf8[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF8[r,c] = NA}	
		
imp8  = mice(DF8, m = 1, pred = quickpred(DF8), maxit = 20)



sdf9 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf9)){
	ind = sdf9[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF9[r,c] = NA}	
		
imp9  = mice(DF9, m = 1, pred = quickpred(DF9), maxit = 20)



sdf10 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf10)){
	ind = sdf10[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF10[r,c] = NA}	
		
imp10  = mice(DF10, m = 1, pred = quickpred(DF10), maxit = 20)


W1 = complete(imp1, action = 1)
W2 = complete(imp2, action = 1)
W3 = complete(imp3, action = 1)
W4 = complete(imp4, action = 1)
W5 = complete(imp5, action = 1)
W6 = complete(imp6, action = 1)
W7 = complete(imp7, action = 1)
W8 = complete(imp8, action = 1)
W9 = complete(imp9, action = 1)
W10 = complete(imp10, action = 1)

save(W1, sdf1,  file = "MICE_NLSY50_1.RData" )
save(W2, sdf2, file = "MICE_NLSY50_2.RData" )
save(W3, sdf3, file = "MICE_NLSY50_3.RData" )
save(W4, sdf4, file = "MICE_NLSY50_4.RData" )
save(W5, sdf5, file = "MICE_NLSY50_5.RData" )
save(W6, sdf6, file = "MICE_NLSY50_6.RData" )
save(W7, sdf7, file = "MICE_NLSY50_7.RData" )
save(W8, sdf8, file = "MICE_NLSY50_8.RData" )
save(W9, sdf9, file = "MICE_NLSY50_9.RData" )
save(W10, sdf10, file = "MICE_NLSY50_10.RData" )



rm(list = ls())


# rho = 60
rho = 0.6
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/Data")

df = read.csv("NLSYcleaned.csv")[, -1]
xdf = c(1:(dim(df)[1]*dim(df)[2]))
ndf = nrow(df)
DF1 = df
DF2 = df
DF3 = df
DF4 = df
DF5 = df
DF6 = df
DF7 = df
DF8 = df
DF9 = df
DF10 = df

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/MICE/again")

sdf1 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf1)){
	ind = sdf1[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF1[r,c] = NA}	
		
imp1  = mice(DF1, m = 1, pred = quickpred(DF1), maxit = 20)


sdf2 = sample(xdf, round(rho*length(xdf)))
for ( i in 1:length(sdf2)){
	ind = sdf2[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF2[r,c] = NA}	
		
imp2  = mice(DF2, m = 1, pred = quickpred(DF2), maxit = 20)



sdf3 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf3)){
	ind = sdf3[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF3[r,c] = NA}	
		
imp3  = mice(DF3, m = 1, pred = quickpred(DF3), maxit = 20)


sdf4 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf4)){
	ind = sdf4[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF4[r,c] = NA}	
		
imp4  = mice(DF4, m = 1, pred = quickpred(DF4), maxit = 20)


sdf5 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf5)){
	ind = sdf5[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF5[r,c] = NA}	
		
imp5  = mice(DF5, m = 1, pred = quickpred(DF5), maxit = 20)


sdf6 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf6)){
	ind = sdf6[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF6[r,c] = NA}	
		
imp6  = mice(DF6, m = 1, pred = quickpred(DF6), maxit = 20)



sdf7 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf7)){
	ind = sdf7[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF7[r,c] = NA}	
		
imp7  = mice(DF7, m = 1, pred = quickpred(DF7), maxit = 20)


sdf8 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf8)){
	ind = sdf8[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF8[r,c] = NA}	
		
imp8  = mice(DF8, m = 1, pred = quickpred(DF8), maxit = 20)



sdf9 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf9)){
	ind = sdf9[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF9[r,c] = NA}	
		
imp9  = mice(DF9, m = 1, pred = quickpred(DF9), maxit = 20)



sdf10 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf10)){
	ind = sdf10[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF10[r,c] = NA}	
		
imp10  = mice(DF10, m = 1, pred = quickpred(DF10), maxit = 20)


W1 = complete(imp1, action = 1)
W2 = complete(imp2, action = 1)
W3 = complete(imp3, action = 1)
W4 = complete(imp4, action = 1)
W5 = complete(imp5, action = 1)
W6 = complete(imp6, action = 1)
W7 = complete(imp7, action = 1)
W8 = complete(imp8, action = 1)
W9 = complete(imp9, action = 1)
W10 = complete(imp10, action = 1)


save(W1, sdf1,  file = "MICE_NLSY60_1.RData" )
save(W2, sdf2, file = "MICE_NLSY60_2.RData" )
save(W3, sdf3, file = "MICE_NLSY60_3.RData" )
save(W4, sdf4, file = "MICE_NLSY60_4.RData" )
save(W5, sdf5, file = "MICE_NLSY60_5.RData" )
save(W6, sdf6, file = "MICE_NLSY60_6.RData" )
save(W7, sdf7, file = "MICE_NLSY60_7.RData" )
save(W8, sdf8, file = "MICE_NLSY60_8.RData" )
save(W9, sdf9, file = "MICE_NLSY60_9.RData" )
save(W10, sdf10, file = "MICE_NLSY60_10.RData" )


rm(list = ls())



# rho = 70
rho = 0.7
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/Data")

df = read.csv("NLSYcleaned.csv")[, -1]
xdf = c(1:(dim(df)[1]*dim(df)[2]))
ndf = nrow(df)
DF1 = df
DF2 = df
DF3 = df
DF4 = df
DF5 = df
DF6 = df
DF7 = df
DF8 = df
DF9 = df
DF10 = df

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/MICE/again")

sdf1 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf1)){
	ind = sdf1[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF1[r,c] = NA}	
		
imp1  = mice(DF1, m = 1, pred = quickpred(DF1), maxit = 20)


sdf2 = sample(xdf, round(rho*length(xdf)))
for ( i in 1:length(sdf2)){
	ind = sdf2[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF2[r,c] = NA}	
		
imp2  = mice(DF2, m = 1, pred = quickpred(DF2), maxit = 20)



sdf3 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf3)){
	ind = sdf3[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF3[r,c] = NA}	
		
imp3  = mice(DF3, m = 1, pred = quickpred(DF3), maxit = 20)


sdf4 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf4)){
	ind = sdf4[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF4[r,c] = NA}	
		
imp4  = mice(DF4, m = 1, pred = quickpred(DF4), maxit = 20)


sdf5 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf5)){
	ind = sdf5[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF5[r,c] = NA}	
		
imp5  = mice(DF5, m = 1, pred = quickpred(DF5), maxit = 20)


sdf6 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf6)){
	ind = sdf6[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF6[r,c] = NA}	
		
imp6  = mice(DF6, m = 1, pred = quickpred(DF6), maxit = 20)



sdf7 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf7)){
	ind = sdf7[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF7[r,c] = NA}	
		
imp7  = mice(DF7, m = 1, pred = quickpred(DF7), maxit = 20)


sdf8 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf8)){
	ind = sdf8[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF8[r,c] = NA}	
		
imp8  = mice(DF8, m = 1, pred = quickpred(DF8), maxit = 20)



sdf9 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf9)){
	ind = sdf9[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF9[r,c] = NA}	
		
imp9  = mice(DF9, m = 1, pred = quickpred(DF9), maxit = 20)



sdf10 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf10)){
	ind = sdf10[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF10[r,c] = NA}	
		
imp10  = mice(DF10, m = 1, pred = quickpred(DF10), maxit = 20)


W1 = complete(imp1, action = 1)
W2 = complete(imp2, action = 1)
W3 = complete(imp3, action = 1)
W4 = complete(imp4, action = 1)
W5 = complete(imp5, action = 1)
W6 = complete(imp6, action = 1)
W7 = complete(imp7, action = 1)
W8 = complete(imp8, action = 1)
W9 = complete(imp9, action = 1)
W10 = complete(imp10, action = 1)


save(W1, sdf1,  file = "MICE_NLSY70_1.RData" )
save(W2, sdf2, file = "MICE_NLSY70_2.RData" )
save(W3, sdf3, file = "MICE_NLSY70_3.RData" )
save(W4, sdf4, file = "MICE_NLSY70_4.RData" )
save(W5, sdf5, file = "MICE_NLSY70_5.RData" )
save(W6, sdf6, file = "MICE_NLSY70_6.RData" )
save(W7, sdf7, file = "MICE_NLSY70_7.RData" )
save(W8, sdf8, file = "MICE_NLSY70_8.RData" )
save(W9, sdf9, file = "MICE_NLSY70_9.RData" )
save(W10, sdf10, file = "MICE_NLSY70_10.RData" )



rm(list = ls())


# rho = 80 
rho = 0.8
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/Data")

df = read.csv("NLSYcleaned.csv")[, -1]
xdf = c(1:(dim(df)[1]*dim(df)[2]))
ndf = nrow(df)
DF1 = df
DF2 = df
DF3 = df
DF4 = df
DF5 = df
DF6 = df
DF7 = df
DF8 = df
DF9 = df
DF10 = df

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/MICE/again")

sdf1 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf1)){
	ind = sdf1[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF1[r,c] = NA}	
		
imp1  = mice(DF1, m = 1, pred = quickpred(DF1), maxit = 20)


sdf2 = sample(xdf, round(rho*length(xdf)))
for ( i in 1:length(sdf2)){
	ind = sdf2[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF2[r,c] = NA}	
		
imp2  = mice(DF2, m = 1, pred = quickpred(DF2), maxit = 20)



sdf3 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf3)){
	ind = sdf3[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF3[r,c] = NA}	
		
imp3  = mice(DF3, m = 1, pred = quickpred(DF3), maxit = 20)


sdf4 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf4)){
	ind = sdf4[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF4[r,c] = NA}	
		
imp4  = mice(DF4, m = 1, pred = quickpred(DF4), maxit = 20)


sdf5 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf5)){
	ind = sdf5[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF5[r,c] = NA}	
		
imp5  = mice(DF5, m = 1, pred = quickpred(DF5), maxit = 20)


sdf6 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf6)){
	ind = sdf6[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF6[r,c] = NA}	
		
imp6  = mice(DF6, m = 1, pred = quickpred(DF6), maxit = 20)



sdf7 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf7)){
	ind = sdf7[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF7[r,c] = NA}	
		
imp7  = mice(DF7, m = 1, pred = quickpred(DF7), maxit = 20)


sdf8 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf8)){
	ind = sdf8[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF8[r,c] = NA}	
		
imp8  = mice(DF8, m = 1, pred = quickpred(DF8), maxit = 20)



sdf9 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf9)){
	ind = sdf9[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF9[r,c] = NA}	
		
imp9  = mice(DF9, m = 1, pred = quickpred(DF9), maxit = 20)



sdf10 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf10)){
	ind = sdf10[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF10[r,c] = NA}	
		
imp10  = mice(DF10, m = 1, pred = quickpred(DF10), maxit = 20)


W1 = complete(imp1, action = 1)
W2 = complete(imp2, action = 1)
W3 = complete(imp3, action = 1)
W4 = complete(imp4, action = 1)
W5 = complete(imp5, action = 1)
W6 = complete(imp6, action = 1)
W7 = complete(imp7, action = 1)
W8 = complete(imp8, action = 1)
W9 = complete(imp9, action = 1)
W10 = complete(imp10, action = 1)



save(W1, sdf1,  file = "MICE_NLSY80_1.RData" )
save(W2, sdf2, file = "MICE_NLSY80_2.RData" )
save(W3, sdf3, file = "MICE_NLSY80_3.RData" )
save(W4, sdf4, file = "MICE_NLSY80_4.RData" )
save(W5, sdf5, file = "MICE_NLSY80_5.RData" )
save(W6, sdf6, file = "MICE_NLSY80_6.RData" )
save(W7, sdf7, file = "MICE_NLSY80_7.RData" )
save(W8, sdf8, file = "MICE_NLSY80_8.RData" )
save(W9, sdf9, file = "MICE_NLSY80_9.RData" )
save(W10, sdf10, file = "MICE_NLSY80_10.RData" )



rm(list = ls())

# rho = 90
rho = 0.9
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/Data")

df = read.csv("NLSYcleaned.csv")[, -1]
xdf = c(1:(dim(df)[1]*dim(df)[2]))
ndf = nrow(df)
DF1 = df
DF2 = df
DF3 = df
DF4 = df
DF5 = df
DF6 = df
DF7 = df
DF8 = df
DF9 = df
DF10 = df

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/MICE/again")

sdf1 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf1)){
	ind = sdf1[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF1[r,c] = NA}	
		
imp1  = mice(DF1, m = 1, pred = quickpred(DF1), maxit = 20)


sdf2 = sample(xdf, round(rho*length(xdf)))
for ( i in 1:length(sdf2)){
	ind = sdf2[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF2[r,c] = NA}	
		
imp2  = mice(DF2, m = 1, pred = quickpred(DF2), maxit = 20)



sdf3 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf3)){
	ind = sdf3[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF3[r,c] = NA}	
		
imp3  = mice(DF3, m = 1, pred = quickpred(DF3), maxit = 20)


sdf4 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf4)){
	ind = sdf4[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF4[r,c] = NA}	
		
imp4  = mice(DF4, m = 1, pred = quickpred(DF4), maxit = 20)


sdf5 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf5)){
	ind = sdf5[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF5[r,c] = NA}	
		
imp5  = mice(DF5, m = 1, pred = quickpred(DF5), maxit = 20)


sdf6 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf6)){
	ind = sdf6[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF6[r,c] = NA}	
		
imp6  = mice(DF6, m = 1, pred = quickpred(DF6), maxit = 20)



sdf7 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf7)){
	ind = sdf7[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF7[r,c] = NA}	
		
imp7  = mice(DF7, m = 1, pred = quickpred(DF7), maxit = 20)


sdf8 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf8)){
	ind = sdf8[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF8[r,c] = NA}	
		
imp8  = mice(DF8, m = 1, pred = quickpred(DF8), maxit = 20)



sdf9 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf9)){
	ind = sdf9[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF9[r,c] = NA}	
		
imp9  = mice(DF9, m = 1, pred = quickpred(DF9), maxit = 20)



sdf10 = sample(xdf, round(rho*length(xdf)))

for ( i in 1:length(sdf10)){
	ind = sdf10[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	DF10[r,c] = NA}	
		
imp10  = mice(DF10, m = 1, pred = quickpred(DF10), maxit = 20)


W1 = complete(imp1, action = 1)
W2 = complete(imp2, action = 1)
W3 = complete(imp3, action = 1)
W4 = complete(imp4, action = 1)
W5 = complete(imp5, action = 1)
W6 = complete(imp6, action = 1)
W7 = complete(imp7, action = 1)
W8 = complete(imp8, action = 1)
W9 = complete(imp9, action = 1)
W10 = complete(imp10, action = 1)



save(W1, sdf1,  file = "MICE_NLSY90_1.RData" )
save(W2, sdf2, file = "MICE_NLSY90_2.RData" )
save(W3, sdf3, file = "MICE_NLSY90_3.RData" )
save(W4, sdf4, file = "MICE_NLSY90_4.RData" )
save(W5, sdf5, file = "MICE_NLSY90_5.RData" )
save(W6, sdf6, file = "MICE_NLSY90_6.RData" )
save(W7, sdf7, file = "MICE_NLSY90_7.RData" )
save(W8, sdf8, file = "MICE_NLSY90_8.RData" )
save(W9, sdf9, file = "MICE_NLSY90_9.RData" )
save(W10, sdf10, file = "MICE_NLSY90_10.RData" )



rm(list = ls())



