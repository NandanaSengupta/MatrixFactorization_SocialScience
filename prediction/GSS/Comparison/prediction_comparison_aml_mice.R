# given dataframe df , list of categoricals cat1, imputed dataset IMP

library("xtable")

rm(list = ls())

###Calculating scaled loss given original data df.base, categorical columns cat1, imputed dataset IMP


loss = function(IMP, df.base, s, cat1, ord1, real1){

	l = 0
	lcol = rep(0, dim(IMP)[2])
	n = nrow(IMP)
	m = ncol(IMP)
df.base0 = df.base

# scale numeric & integer columns and attach scaled columns to categorical columns

df.scale.real = scale(df.base[, real1])
df.scale.ord = numeric()
for (o in 1: length(ord1)){
df.scale.ord = cbind(df.scale.ord, 	(df.base[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

df.base[,real1] = df.scale.real
df.base[,ord1] = df.scale.ord

IMP.scale.real = scale(IMP[, real1])
IMP.scale.ord = numeric()
for (o in 1: length(ord1)){
IMP.scale.ord = cbind(IMP.scale.ord, 	(IMP[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

IMP[,real1] = IMP.scale.real
IMP[,ord1] = IMP.scale.ord

# now we loop through all induced missing values and add up the errors

for (j in 1:length(s)){
	ind = s[j]
#	print(j)
	
# converting indices to matrix row and columns
	r = ((ind-1) %% n) + 1 
	c = floor((ind-1) / n) + 1
	
# for categorical columns loss is whether or not mis-classified  (also keeping track of number of factor levels here)

	if ( is.element(c,cat1)){
		l = l+ (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lind = (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lcol[c] = lcol[c]+ lind} 
	 

# for remaining columns loss is squared difference
		
    if (!is.element(c,cat1) ) {l = l + (df.base[r,c] - IMP[r,c])^2
    	lind = (df.base[r,c] - IMP[r,c])^2	
	lcol[c] = lcol[c] + lind}
	
		#print(c(j, c, r, lind, lcol[c], l))
		}	

	rm(lind)
	
# get back total loss, columnwise loss and checks for factor levels
	return(list(loss = l, lcol = lcol))}


# add factor levels to make comparisons possible if needed 

factorLevels <- function(IMP, df.base, cat1){
	for (k in 1: length(cat1)){
		levs = levels(df.base[, cat1[k]])
		IMP[, cat1[k]] = factor(IMP[, cat1[k]], levels = levs)
		print(c(k, cat1[k], sum(is.na(IMP[,cat1[k]]))))
		}
       
       IMPfin = IMP
       
       for (h in 1: length(cat1)){
       	g = length(levels(df.base[,cat1[h]]))
       	b = length(levels(IMPfin[, cat1[h]]))
       	print(g-b)}
      
 return(IMPfin)}

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

df.cv = read.csv("GSS10.csv")[,-1] #df with CV indices removed

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))

a= which(is.na(df.base))
b= which(is.na(df.cv))
s = b[!is.element(b,a)] # elements on which to do cv


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/AMELIA")

load('amelia_GSS10_fin.RData')
load('amelia_GSS10_fin_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/MICE")

load('MICE_GSS10.RData')
load('MICE_GSS10_copy.RData')


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparison")

# Amelia
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)
AML8= factorLevels(am8, df.base, cat1)
AML9= factorLevels(am9, df.base, cat1)
AML10= factorLevels(am10, df.base, cat1)
AML11= factorLevels(am11, df.base, cat1)
AML12= factorLevels(am12, df.base, cat1)
AML13= factorLevels(am13, df.base, cat1)
AML14= factorLevels(am14, df.base, cat1)
AML15= factorLevels(am15, df.base, cat1)
AML16= factorLevels(am16, df.base, cat1)
AML17= factorLevels(am17, df.base, cat1)
AML18= factorLevels(am18, df.base, cat1)
AML19= factorLevels(am19, df.base, cat1)
AML20= factorLevels(am20, df.base, cat1)



# MICE
MICE1 = factorLevels(W1, df.base, cat1)
MICE2 = factorLevels(W2, df.base, cat1)
MICE3 = factorLevels(W3, df.base, cat1)
MICE4 = factorLevels(W4, df.base, cat1)
MICE5 = factorLevels(W5, df.base, cat1)
MICE6 = factorLevels(W6, df.base, cat1)
MICE7 = factorLevels(W7, df.base, cat1)
MICE8 = factorLevels(W8, df.base, cat1)
MICE9 = factorLevels(W9, df.base, cat1)
MICE10 = factorLevels(W10, df.base, cat1)
MICE11 = factorLevels(W11, df.base, cat1)
MICE12 = factorLevels(W12, df.base, cat1)
MICE13 = factorLevels(W13, df.base, cat1)
MICE14 = factorLevels(W14, df.base, cat1)
MICE15 = factorLevels(W15, df.base, cat1)
MICE16 = factorLevels(W16, df.base, cat1)
MICE17 = factorLevels(W17, df.base, cat1)
MICE18 = factorLevels(W18, df.base, cat1)
MICE19 = factorLevels(W19, df.base, cat1)
MICE20 = factorLevels(W20, df.base, cat1)

# LOSS CALCULATIONS AMELIA AND MICE
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  

# saving

all.aml10 = l.aml
all.mice10 = l.mice

save(all.aml10, all.mice10, file = "aml_mice_GSS10.RData")

rm(list = ls())


######### 20


# given dataframe df , list of categoricals cat1, imputed dataset IMP

library("xtable")

rm(list = ls())

###Calculating scaled loss given original data df.base, categorical columns cat1, imputed dataset IMP


loss = function(IMP, df.base, s, cat1, ord1, real1){

	l = 0
	lcol = rep(0, dim(IMP)[2])
	n = nrow(IMP)
	m = ncol(IMP)
df.base0 = df.base

# scale numeric & integer columns and attach scaled columns to categorical columns

df.scale.real = scale(df.base[, real1])
df.scale.ord = numeric()
for (o in 1: length(ord1)){
df.scale.ord = cbind(df.scale.ord, 	(df.base[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

df.base[,real1] = df.scale.real
df.base[,ord1] = df.scale.ord

IMP.scale.real = scale(IMP[, real1])
IMP.scale.ord = numeric()
for (o in 1: length(ord1)){
IMP.scale.ord = cbind(IMP.scale.ord, 	(IMP[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

IMP[,real1] = IMP.scale.real
IMP[,ord1] = IMP.scale.ord

# now we loop through all induced missing values and add up the errors

for (j in 1:length(s)){
	ind = s[j]
#	print(j)
	
# converting indices to matrix row and columns
	r = ((ind-1) %% n) + 1 
	c = floor((ind-1) / n) + 1
	
# for categorical columns loss is whether or not mis-classified  (also keeping track of number of factor levels here)

	if ( is.element(c,cat1)){
		l = l+ (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lind = (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lcol[c] = lcol[c]+ lind} 
	 

# for remaining columns loss is squared difference
		
    if (!is.element(c,cat1) ) {l = l + (df.base[r,c] - IMP[r,c])^2
    	lind = (df.base[r,c] - IMP[r,c])^2	
	lcol[c] = lcol[c] + lind}
	
		#print(c(j, c, r, lind, lcol[c], l))
		}	

	rm(lind)
	
# get back total loss, columnwise loss and checks for factor levels
	return(list(loss = l, lcol = lcol))}


# add factor levels to make comparisons possible if needed 

factorLevels <- function(IMP, df.base, cat1){
	for (k in 1: length(cat1)){
		levs = levels(df.base[, cat1[k]])
		IMP[, cat1[k]] = factor(IMP[, cat1[k]], levels = levs)
		print(c(k, cat1[k], sum(is.na(IMP[,cat1[k]]))))
		}
       
       IMPfin = IMP
       
       for (h in 1: length(cat1)){
       	g = length(levels(df.base[,cat1[h]]))
       	b = length(levels(IMPfin[, cat1[h]]))
       	print(g-b)}
      
 return(IMPfin)}

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

df.cv = read.csv("GSS20.csv")[,-1] #df with CV indices removed

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))

a= which(is.na(df.base))
b= which(is.na(df.cv))
s = b[!is.element(b,a)] # elements on which to do cv


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/AMELIA")

load('amelia_GSS20_fin.RData')
load('amelia_GSS20_fin_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/MICE")

load('MICE_GSS20.RData')
load('MICE_GSS20_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparison")

# Amelia
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)
AML8= factorLevels(am8, df.base, cat1)
AML9= factorLevels(am9, df.base, cat1)
AML10= factorLevels(am10, df.base, cat1)
AML11= factorLevels(am11, df.base, cat1)
AML12= factorLevels(am12, df.base, cat1)
AML13= factorLevels(am13, df.base, cat1)
AML14= factorLevels(am14, df.base, cat1)
AML15= factorLevels(am15, df.base, cat1)
AML16= factorLevels(am16, df.base, cat1)
AML17= factorLevels(am17, df.base, cat1)
AML18= factorLevels(am18, df.base, cat1)
AML19= factorLevels(am19, df.base, cat1)
AML20= factorLevels(am20, df.base, cat1)



# MICE
MICE1 = factorLevels(W1, df.base, cat1)
MICE2 = factorLevels(W2, df.base, cat1)
MICE3 = factorLevels(W3, df.base, cat1)
MICE4 = factorLevels(W4, df.base, cat1)
MICE5 = factorLevels(W5, df.base, cat1)
MICE6 = factorLevels(W6, df.base, cat1)
MICE7 = factorLevels(W7, df.base, cat1)
MICE8 = factorLevels(W8, df.base, cat1)
MICE9 = factorLevels(W9, df.base, cat1)
MICE10 = factorLevels(W10, df.base, cat1)
MICE11 = factorLevels(W11, df.base, cat1)
MICE12 = factorLevels(W12, df.base, cat1)
MICE13 = factorLevels(W13, df.base, cat1)
MICE14 = factorLevels(W14, df.base, cat1)
MICE15 = factorLevels(W15, df.base, cat1)
MICE16 = factorLevels(W16, df.base, cat1)
MICE17 = factorLevels(W17, df.base, cat1)
MICE18 = factorLevels(W18, df.base, cat1)
MICE19 = factorLevels(W19, df.base, cat1)
MICE20 = factorLevels(W20, df.base, cat1)

# LOSS CALCULATIONS AMELIA AND MICE
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  


# saving
all.aml20 = l.aml
all.mice20 = l.mice

save(all.aml20, all.mice20, file = "aml_mice_GSS20.RData")

rm(list = ls())


######### 30




# given dataframe df , list of categoricals cat1, imputed dataset IMP

library("xtable")

rm(list = ls())

###Calculating scaled loss given original data df.base, categorical columns cat1, imputed dataset IMP


loss = function(IMP, df.base, s, cat1, ord1, real1){

	l = 0
	lcol = rep(0, dim(IMP)[2])
	n = nrow(IMP)
	m = ncol(IMP)
df.base0 = df.base

# scale numeric & integer columns and attach scaled columns to categorical columns

df.scale.real = scale(df.base[, real1])
df.scale.ord = numeric()
for (o in 1: length(ord1)){
df.scale.ord = cbind(df.scale.ord, 	(df.base[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

df.base[,real1] = df.scale.real
df.base[,ord1] = df.scale.ord

IMP.scale.real = scale(IMP[, real1])
IMP.scale.ord = numeric()
for (o in 1: length(ord1)){
IMP.scale.ord = cbind(IMP.scale.ord, 	(IMP[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

IMP[,real1] = IMP.scale.real
IMP[,ord1] = IMP.scale.ord

# now we loop through all induced missing values and add up the errors

for (j in 1:length(s)){
	ind = s[j]
#	print(j)
	
# converting indices to matrix row and columns
	r = ((ind-1) %% n) + 1 
	c = floor((ind-1) / n) + 1
	
# for categorical columns loss is whether or not mis-classified  (also keeping track of number of factor levels here)

	if ( is.element(c,cat1)){
		l = l+ (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lind = (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lcol[c] = lcol[c]+ lind} 
	 

# for remaining columns loss is squared difference
		
    if (!is.element(c,cat1) ) {l = l + (df.base[r,c] - IMP[r,c])^2
    	lind = (df.base[r,c] - IMP[r,c])^2	
	lcol[c] = lcol[c] + lind}
	
		#print(c(j, c, r, lind, lcol[c], l))
		}	

	rm(lind)
	
# get back total loss, columnwise loss and checks for factor levels
	return(list(loss = l, lcol = lcol))}


# add factor levels to make comparisons possible if needed 

factorLevels <- function(IMP, df.base, cat1){
	for (k in 1: length(cat1)){
		levs = levels(df.base[, cat1[k]])
		IMP[, cat1[k]] = factor(IMP[, cat1[k]], levels = levs)
		print(c(k, cat1[k], sum(is.na(IMP[,cat1[k]]))))
		}
       
       IMPfin = IMP
       
       for (h in 1: length(cat1)){
       	g = length(levels(df.base[,cat1[h]]))
       	b = length(levels(IMPfin[, cat1[h]]))
       	print(g-b)}
      
 return(IMPfin)}

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

df.cv = read.csv("GSS30.csv")[,-1] #df with CV indices removed

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))

a= which(is.na(df.base))
b= which(is.na(df.cv))
s = b[!is.element(b,a)] # elements on which to do cv


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/AMELIA")

load('amelia_GSS30_fin.RData')
load('amelia_GSS30_fin_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/MICE")

load('MICE_GSS30.RData')
load('MICE_GSS30_copy.RData')


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparison")

# Amelia
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)
AML8= factorLevels(am8, df.base, cat1)
AML9= factorLevels(am9, df.base, cat1)
AML10= factorLevels(am10, df.base, cat1)
AML11= factorLevels(am11, df.base, cat1)
AML12= factorLevels(am12, df.base, cat1)
AML13= factorLevels(am13, df.base, cat1)
AML14= factorLevels(am14, df.base, cat1)
AML15= factorLevels(am15, df.base, cat1)
AML16= factorLevels(am16, df.base, cat1)
AML17= factorLevels(am17, df.base, cat1)
AML18= factorLevels(am18, df.base, cat1)
AML19= factorLevels(am19, df.base, cat1)
AML20= factorLevels(am20, df.base, cat1)



# MICE
MICE1 = factorLevels(W1, df.base, cat1)
MICE2 = factorLevels(W2, df.base, cat1)
MICE3 = factorLevels(W3, df.base, cat1)
MICE4 = factorLevels(W4, df.base, cat1)
MICE5 = factorLevels(W5, df.base, cat1)
MICE6 = factorLevels(W6, df.base, cat1)
MICE7 = factorLevels(W7, df.base, cat1)
MICE8 = factorLevels(W8, df.base, cat1)
MICE9 = factorLevels(W9, df.base, cat1)
MICE10 = factorLevels(W10, df.base, cat1)
MICE11 = factorLevels(W11, df.base, cat1)
MICE12 = factorLevels(W12, df.base, cat1)
MICE13 = factorLevels(W13, df.base, cat1)
MICE14 = factorLevels(W14, df.base, cat1)
MICE15 = factorLevels(W15, df.base, cat1)
MICE16 = factorLevels(W16, df.base, cat1)
MICE17 = factorLevels(W17, df.base, cat1)
MICE18 = factorLevels(W18, df.base, cat1)
MICE19 = factorLevels(W19, df.base, cat1)
MICE20 = factorLevels(W20, df.base, cat1)

#MATRIX FAC
gfrm = trace
glrm = rank
GFRM = factorLevels(gfrm, df.base, cat1)
GLRM = factorLevels(glrm, df.base, cat1)


# LOSS CALCULATIONS AMELIA AND MICE
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  

all.aml30 = l.aml
all.mice30 = l.mice

save(all.aml30, all.mice30, file = "aml_mice_GSS30.RData")

rm(list = ls())


######### 40


# given dataframe df , list of categoricals cat1, imputed dataset IMP

library("xtable")

rm(list = ls())

###Calculating scaled loss given original data df.base, categorical columns cat1, imputed dataset IMP


loss = function(IMP, df.base, s, cat1, ord1, real1){

	l = 0
	lcol = rep(0, dim(IMP)[2])
	n = nrow(IMP)
	m = ncol(IMP)
df.base0 = df.base

# scale numeric & integer columns and attach scaled columns to categorical columns

df.scale.real = scale(df.base[, real1])
df.scale.ord = numeric()
for (o in 1: length(ord1)){
df.scale.ord = cbind(df.scale.ord, 	(df.base[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

df.base[,real1] = df.scale.real
df.base[,ord1] = df.scale.ord

IMP.scale.real = scale(IMP[, real1])
IMP.scale.ord = numeric()
for (o in 1: length(ord1)){
IMP.scale.ord = cbind(IMP.scale.ord, 	(IMP[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

IMP[,real1] = IMP.scale.real
IMP[,ord1] = IMP.scale.ord

# now we loop through all induced missing values and add up the errors

for (j in 1:length(s)){
	ind = s[j]
#	print(j)
	
# converting indices to matrix row and columns
	r = ((ind-1) %% n) + 1 
	c = floor((ind-1) / n) + 1
	
# for categorical columns loss is whether or not mis-classified  (also keeping track of number of factor levels here)

	if ( is.element(c,cat1)){
		l = l+ (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lind = (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lcol[c] = lcol[c]+ lind} 
	 

# for remaining columns loss is squared difference
		
    if (!is.element(c,cat1) ) {l = l + (df.base[r,c] - IMP[r,c])^2
    	lind = (df.base[r,c] - IMP[r,c])^2	
	lcol[c] = lcol[c] + lind}
	
		#print(c(j, c, r, lind, lcol[c], l))
		}	

	rm(lind)
	
# get back total loss, columnwise loss and checks for factor levels
	return(list(loss = l, lcol = lcol))}


# add factor levels to make comparisons possible if needed 

factorLevels <- function(IMP, df.base, cat1){
	for (k in 1: length(cat1)){
		levs = levels(df.base[, cat1[k]])
		IMP[, cat1[k]] = factor(IMP[, cat1[k]], levels = levs)
		print(c(k, cat1[k], sum(is.na(IMP[,cat1[k]]))))
		}
       
       IMPfin = IMP
       
       for (h in 1: length(cat1)){
       	g = length(levels(df.base[,cat1[h]]))
       	b = length(levels(IMPfin[, cat1[h]]))
       	print(g-b)}
      
 return(IMPfin)}

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

df.cv = read.csv("GSS40.csv")[,-1] #df with CV indices removed

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))

a= which(is.na(df.base))
b= which(is.na(df.cv))
s = b[!is.element(b,a)] # elements on which to do cv


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/AMELIA")

load('amelia_GSS40_fin.RData')
load('amelia_GSS40_fin_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/MICE")

load('MICE_GSS40.RData')
load('MICE_GSS40_copy.RData')


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparison")

# Amelia
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)
AML8= factorLevels(am8, df.base, cat1)
AML9= factorLevels(am9, df.base, cat1)
AML10= factorLevels(am10, df.base, cat1)
AML11= factorLevels(am11, df.base, cat1)
AML12= factorLevels(am12, df.base, cat1)
AML13= factorLevels(am13, df.base, cat1)
AML14= factorLevels(am14, df.base, cat1)
AML15= factorLevels(am15, df.base, cat1)
AML16= factorLevels(am16, df.base, cat1)
AML17= factorLevels(am17, df.base, cat1)
AML18= factorLevels(am18, df.base, cat1)
AML19= factorLevels(am19, df.base, cat1)
AML20= factorLevels(am20, df.base, cat1)



# MICE
MICE1 = factorLevels(W1, df.base, cat1)
MICE2 = factorLevels(W2, df.base, cat1)
MICE3 = factorLevels(W3, df.base, cat1)
MICE4 = factorLevels(W4, df.base, cat1)
MICE5 = factorLevels(W5, df.base, cat1)
MICE6 = factorLevels(W6, df.base, cat1)
MICE7 = factorLevels(W7, df.base, cat1)
MICE8 = factorLevels(W8, df.base, cat1)
MICE9 = factorLevels(W9, df.base, cat1)
MICE10 = factorLevels(W10, df.base, cat1)
MICE11 = factorLevels(W11, df.base, cat1)
MICE12 = factorLevels(W12, df.base, cat1)
MICE13 = factorLevels(W13, df.base, cat1)
MICE14 = factorLevels(W14, df.base, cat1)
MICE15 = factorLevels(W15, df.base, cat1)
MICE16 = factorLevels(W16, df.base, cat1)
MICE17 = factorLevels(W17, df.base, cat1)
MICE18 = factorLevels(W18, df.base, cat1)
MICE19 = factorLevels(W19, df.base, cat1)
MICE20 = factorLevels(W20, df.base, cat1)

#MATRIX FAC
gfrm = trace
glrm = rank
GFRM = factorLevels(gfrm, df.base, cat1)
GLRM = factorLevels(glrm, df.base, cat1)


# LOSS CALCULATIONS AMELIA AND MICE
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  

all.aml40 = l.aml
all.mice40 = l.mice

save(all.aml40, all.mice40, file = "aml_mice_GSS40.RData")
rm(list = ls())


######### 50


# given dataframe df , list of categoricals cat1, imputed dataset IMP

library("xtable")

rm(list = ls())

###Calculating scaled loss given original data df.base, categorical columns cat1, imputed dataset IMP


loss = function(IMP, df.base, s, cat1, ord1, real1){

	l = 0
	lcol = rep(0, dim(IMP)[2])
	n = nrow(IMP)
	m = ncol(IMP)
df.base0 = df.base

# scale numeric & integer columns and attach scaled columns to categorical columns

df.scale.real = scale(df.base[, real1])
df.scale.ord = numeric()
for (o in 1: length(ord1)){
df.scale.ord = cbind(df.scale.ord, 	(df.base[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

df.base[,real1] = df.scale.real
df.base[,ord1] = df.scale.ord

IMP.scale.real = scale(IMP[, real1])
IMP.scale.ord = numeric()
for (o in 1: length(ord1)){
IMP.scale.ord = cbind(IMP.scale.ord, 	(IMP[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

IMP[,real1] = IMP.scale.real
IMP[,ord1] = IMP.scale.ord

# now we loop through all induced missing values and add up the errors

for (j in 1:length(s)){
	ind = s[j]
#	print(j)
	
# converting indices to matrix row and columns
	r = ((ind-1) %% n) + 1 
	c = floor((ind-1) / n) + 1
	
# for categorical columns loss is whether or not mis-classified  (also keeping track of number of factor levels here)

	if ( is.element(c,cat1)){
		l = l+ (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lind = (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lcol[c] = lcol[c]+ lind} 
	 

# for remaining columns loss is squared difference
		
    if (!is.element(c,cat1) ) {l = l + (df.base[r,c] - IMP[r,c])^2
    	lind = (df.base[r,c] - IMP[r,c])^2	
	lcol[c] = lcol[c] + lind}
	
		#print(c(j, c, r, lind, lcol[c], l))
		}	

	rm(lind)
	
# get back total loss, columnwise loss and checks for factor levels
	return(list(loss = l, lcol = lcol))}


# add factor levels to make comparisons possible if needed 

factorLevels <- function(IMP, df.base, cat1){
	for (k in 1: length(cat1)){
		levs = levels(df.base[, cat1[k]])
		IMP[, cat1[k]] = factor(IMP[, cat1[k]], levels = levs)
		print(c(k, cat1[k], sum(is.na(IMP[,cat1[k]]))))
		}
       
       IMPfin = IMP
       
       for (h in 1: length(cat1)){
       	g = length(levels(df.base[,cat1[h]]))
       	b = length(levels(IMPfin[, cat1[h]]))
       	print(g-b)}
      
 return(IMPfin)}

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

df.cv = read.csv("GSS50.csv")[,-1] #df with CV indices removed

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))

a= which(is.na(df.base))
b= which(is.na(df.cv))
s = b[!is.element(b,a)] # elements on which to do cv


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/AMELIA")

load('amelia_GSS50_fin.RData')
load('amelia_GSS50_fin_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/MICE")

load('MICE_GSS50.RData')
load('MICE_GSS50_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparison")

# Amelia
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)
AML8= factorLevels(am8, df.base, cat1)
AML9= factorLevels(am9, df.base, cat1)
AML10= factorLevels(am10, df.base, cat1)
AML11= factorLevels(am11, df.base, cat1)
AML12= factorLevels(am12, df.base, cat1)
AML13= factorLevels(am13, df.base, cat1)
AML14= factorLevels(am14, df.base, cat1)
AML15= factorLevels(am15, df.base, cat1)
AML16= factorLevels(am16, df.base, cat1)
AML17= factorLevels(am17, df.base, cat1)
AML18= factorLevels(am18, df.base, cat1)
AML19= factorLevels(am19, df.base, cat1)
AML20= factorLevels(am20, df.base, cat1)



# MICE
MICE1 = factorLevels(W1, df.base, cat1)
MICE2 = factorLevels(W2, df.base, cat1)
MICE3 = factorLevels(W3, df.base, cat1)
MICE4 = factorLevels(W4, df.base, cat1)
MICE5 = factorLevels(W5, df.base, cat1)
MICE6 = factorLevels(W6, df.base, cat1)
MICE7 = factorLevels(W7, df.base, cat1)
MICE8 = factorLevels(W8, df.base, cat1)
MICE9 = factorLevels(W9, df.base, cat1)
MICE10 = factorLevels(W10, df.base, cat1)
MICE11 = factorLevels(W11, df.base, cat1)
MICE12 = factorLevels(W12, df.base, cat1)
MICE13 = factorLevels(W13, df.base, cat1)
MICE14 = factorLevels(W14, df.base, cat1)
MICE15 = factorLevels(W15, df.base, cat1)
MICE16 = factorLevels(W16, df.base, cat1)
MICE17 = factorLevels(W17, df.base, cat1)
MICE18 = factorLevels(W18, df.base, cat1)
MICE19 = factorLevels(W19, df.base, cat1)
MICE20 = factorLevels(W20, df.base, cat1)

#MATRIX FAC
gfrm = trace
glrm = rank
GFRM = factorLevels(gfrm, df.base, cat1)
GLRM = factorLevels(glrm, df.base, cat1)


# LOSS CALCULATIONS AMELIA AND MICE
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  


all.aml50 = l.aml
all.mice50 = l.mice

save(all.aml50, all.mice50, file = "aml_mice_GSS50.RData")

rm(list = ls())


######### 60


# given dataframe df , list of categoricals cat1, imputed dataset IMP

library("xtable")

rm(list = ls())

###Calculating scaled loss given original data df.base, categorical columns cat1, imputed dataset IMP


loss = function(IMP, df.base, s, cat1, ord1, real1){

	l = 0
	lcol = rep(0, dim(IMP)[2])
	n = nrow(IMP)
	m = ncol(IMP)
df.base0 = df.base

# scale numeric & integer columns and attach scaled columns to categorical columns

df.scale.real = scale(df.base[, real1])
df.scale.ord = numeric()
for (o in 1: length(ord1)){
df.scale.ord = cbind(df.scale.ord, 	(df.base[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

df.base[,real1] = df.scale.real
df.base[,ord1] = df.scale.ord

IMP.scale.real = scale(IMP[, real1])
IMP.scale.ord = numeric()
for (o in 1: length(ord1)){
IMP.scale.ord = cbind(IMP.scale.ord, 	(IMP[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

IMP[,real1] = IMP.scale.real
IMP[,ord1] = IMP.scale.ord

# now we loop through all induced missing values and add up the errors

for (j in 1:length(s)){
	ind = s[j]
#	print(j)
	
# converting indices to matrix row and columns
	r = ((ind-1) %% n) + 1 
	c = floor((ind-1) / n) + 1
	
# for categorical columns loss is whether or not mis-classified  (also keeping track of number of factor levels here)

	if ( is.element(c,cat1)){
		l = l+ (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lind = (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lcol[c] = lcol[c]+ lind} 
	 

# for remaining columns loss is squared difference
		
    if (!is.element(c,cat1) ) {l = l + (df.base[r,c] - IMP[r,c])^2
    	lind = (df.base[r,c] - IMP[r,c])^2	
	lcol[c] = lcol[c] + lind}
	
		#print(c(j, c, r, lind, lcol[c], l))
		}	

	rm(lind)
	
# get back total loss, columnwise loss and checks for factor levels
	return(list(loss = l, lcol = lcol))}


# add factor levels to make comparisons possible if needed 

factorLevels <- function(IMP, df.base, cat1){
	for (k in 1: length(cat1)){
		levs = levels(df.base[, cat1[k]])
		IMP[, cat1[k]] = factor(IMP[, cat1[k]], levels = levs)
		print(c(k, cat1[k], sum(is.na(IMP[,cat1[k]]))))
		}
       
       IMPfin = IMP
       
       for (h in 1: length(cat1)){
       	g = length(levels(df.base[,cat1[h]]))
       	b = length(levels(IMPfin[, cat1[h]]))
       	print(g-b)}
      
 return(IMPfin)}

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

df.cv = read.csv("GSS60.csv")[,-1] #df with CV indices removed

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))

a= which(is.na(df.base))
b= which(is.na(df.cv))
s = b[!is.element(b,a)] # elements on which to do cv


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/AMELIA")

load('amelia_GSS60_fin.RData')
load('amelia_GSS60_fin_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/MICE")

load('MICE_GSS60.RData')
load('MICE_GSS60_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparison")

# Amelia
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)
AML8= factorLevels(am8, df.base, cat1)
AML9= factorLevels(am9, df.base, cat1)
AML10= factorLevels(am10, df.base, cat1)
AML11= factorLevels(am11, df.base, cat1)
AML12= factorLevels(am12, df.base, cat1)
AML13= factorLevels(am13, df.base, cat1)
AML14= factorLevels(am14, df.base, cat1)
AML15= factorLevels(am15, df.base, cat1)
AML16= factorLevels(am16, df.base, cat1)
AML17= factorLevels(am17, df.base, cat1)
AML18= factorLevels(am18, df.base, cat1)
AML19= factorLevels(am19, df.base, cat1)
AML20= factorLevels(am20, df.base, cat1)



# MICE
MICE1 = factorLevels(W1, df.base, cat1)
MICE2 = factorLevels(W2, df.base, cat1)
MICE3 = factorLevels(W3, df.base, cat1)
MICE4 = factorLevels(W4, df.base, cat1)
MICE5 = factorLevels(W5, df.base, cat1)
MICE6 = factorLevels(W6, df.base, cat1)
MICE7 = factorLevels(W7, df.base, cat1)
MICE8 = factorLevels(W8, df.base, cat1)
MICE9 = factorLevels(W9, df.base, cat1)
MICE10 = factorLevels(W10, df.base, cat1)
MICE11 = factorLevels(W11, df.base, cat1)
MICE12 = factorLevels(W12, df.base, cat1)
MICE13 = factorLevels(W13, df.base, cat1)
MICE14 = factorLevels(W14, df.base, cat1)
MICE15 = factorLevels(W15, df.base, cat1)
MICE16 = factorLevels(W16, df.base, cat1)
MICE17 = factorLevels(W17, df.base, cat1)
MICE18 = factorLevels(W18, df.base, cat1)
MICE19 = factorLevels(W19, df.base, cat1)
MICE20 = factorLevels(W20, df.base, cat1)

#MATRIX FAC
gfrm = trace
glrm = rank
GFRM = factorLevels(gfrm, df.base, cat1)
GLRM = factorLevels(glrm, df.base, cat1)


# LOSS CALCULATIONS AMELIA AND MICE
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  

all.aml60 = l.aml
all.mice60 = l.mice

save(all.aml60, all.mice60, file = "aml_mice_GSS60.RData")

rm(list = ls())


######### 70

# given dataframe df , list of categoricals cat1, imputed dataset IMP

library("xtable")

rm(list = ls())

###Calculating scaled loss given original data df.base, categorical columns cat1, imputed dataset IMP


loss = function(IMP, df.base, s, cat1, ord1, real1){

	l = 0
	lcol = rep(0, dim(IMP)[2])
	n = nrow(IMP)
	m = ncol(IMP)
df.base0 = df.base

# scale numeric & integer columns and attach scaled columns to categorical columns

df.scale.real = scale(df.base[, real1])
df.scale.ord = numeric()
for (o in 1: length(ord1)){
df.scale.ord = cbind(df.scale.ord, 	(df.base[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

df.base[,real1] = df.scale.real
df.base[,ord1] = df.scale.ord

IMP.scale.real = scale(IMP[, real1])
IMP.scale.ord = numeric()
for (o in 1: length(ord1)){
IMP.scale.ord = cbind(IMP.scale.ord, 	(IMP[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

IMP[,real1] = IMP.scale.real
IMP[,ord1] = IMP.scale.ord

# now we loop through all induced missing values and add up the errors

for (j in 1:length(s)){
	ind = s[j]
#	print(j)
	
# converting indices to matrix row and columns
	r = ((ind-1) %% n) + 1 
	c = floor((ind-1) / n) + 1
	
# for categorical columns loss is whether or not mis-classified  (also keeping track of number of factor levels here)

	if ( is.element(c,cat1)){
		l = l+ (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lind = (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lcol[c] = lcol[c]+ lind} 
	 

# for remaining columns loss is squared difference
		
    if (!is.element(c,cat1) ) {l = l + (df.base[r,c] - IMP[r,c])^2
    	lind = (df.base[r,c] - IMP[r,c])^2	
	lcol[c] = lcol[c] + lind}
	
		#print(c(j, c, r, lind, lcol[c], l))
		}	

	rm(lind)
	
# get back total loss, columnwise loss and checks for factor levels
	return(list(loss = l, lcol = lcol))}


# add factor levels to make comparisons possible if needed 

factorLevels <- function(IMP, df.base, cat1){
	for (k in 1: length(cat1)){
		levs = levels(df.base[, cat1[k]])
		IMP[, cat1[k]] = factor(IMP[, cat1[k]], levels = levs)
		print(c(k, cat1[k], sum(is.na(IMP[,cat1[k]]))))
		}
       
       IMPfin = IMP
       
       for (h in 1: length(cat1)){
       	g = length(levels(df.base[,cat1[h]]))
       	b = length(levels(IMPfin[, cat1[h]]))
       	print(g-b)}
      
 return(IMPfin)}

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

df.cv = read.csv("GSS70.csv")[,-1] #df with CV indices removed

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))

a= which(is.na(df.base))
b= which(is.na(df.cv))
s = b[!is.element(b,a)] # elements on which to do cv


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/AMELIA")

load('amelia_GSS70_fin.RData')
load('amelia_GSS70_fin_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/MICE")

load('MICE_GSS70.RData')
load('MICE_GSS70_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparison")

# Amelia
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)
AML8= factorLevels(am8, df.base, cat1)
AML9= factorLevels(am9, df.base, cat1)
AML10= factorLevels(am10, df.base, cat1)
AML11= factorLevels(am11, df.base, cat1)
AML12= factorLevels(am12, df.base, cat1)
AML13= factorLevels(am13, df.base, cat1)
AML14= factorLevels(am14, df.base, cat1)
AML15= factorLevels(am15, df.base, cat1)
AML16= factorLevels(am16, df.base, cat1)
AML17= factorLevels(am17, df.base, cat1)
AML18= factorLevels(am18, df.base, cat1)
AML19= factorLevels(am19, df.base, cat1)
AML20= factorLevels(am20, df.base, cat1)



# MICE
MICE1 = factorLevels(W1, df.base, cat1)
MICE2 = factorLevels(W2, df.base, cat1)
MICE3 = factorLevels(W3, df.base, cat1)
MICE4 = factorLevels(W4, df.base, cat1)
MICE5 = factorLevels(W5, df.base, cat1)
MICE6 = factorLevels(W6, df.base, cat1)
MICE7 = factorLevels(W7, df.base, cat1)
MICE8 = factorLevels(W8, df.base, cat1)
MICE9 = factorLevels(W9, df.base, cat1)
MICE10 = factorLevels(W10, df.base, cat1)
MICE11 = factorLevels(W11, df.base, cat1)
MICE12 = factorLevels(W12, df.base, cat1)
MICE13 = factorLevels(W13, df.base, cat1)
MICE14 = factorLevels(W14, df.base, cat1)
MICE15 = factorLevels(W15, df.base, cat1)
MICE16 = factorLevels(W16, df.base, cat1)
MICE17 = factorLevels(W17, df.base, cat1)
MICE18 = factorLevels(W18, df.base, cat1)
MICE19 = factorLevels(W19, df.base, cat1)
MICE20 = factorLevels(W20, df.base, cat1)

#MATRIX FAC
gfrm = trace
glrm = rank
GFRM = factorLevels(gfrm, df.base, cat1)
GLRM = factorLevels(glrm, df.base, cat1)


# LOSS CALCULATIONS AMELIA AND MICE
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  

all.aml70 = l.aml
all.mice70 = l.mice

save(all.aml70, all.mice70, file = "aml_mice_GSS70.RData")

rm(list = ls())


######### 80


# given dataframe df , list of categoricals cat1, imputed dataset IMP

library("xtable")

rm(list = ls())

###Calculating scaled loss given original data df.base, categorical columns cat1, imputed dataset IMP


loss = function(IMP, df.base, s, cat1, ord1, real1){

	l = 0
	lcol = rep(0, dim(IMP)[2])
	n = nrow(IMP)
	m = ncol(IMP)
df.base0 = df.base

# scale numeric & integer columns and attach scaled columns to categorical columns

df.scale.real = scale(df.base[, real1])
df.scale.ord = numeric()
for (o in 1: length(ord1)){
df.scale.ord = cbind(df.scale.ord, 	(df.base[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

df.base[,real1] = df.scale.real
df.base[,ord1] = df.scale.ord

IMP.scale.real = scale(IMP[, real1])
IMP.scale.ord = numeric()
for (o in 1: length(ord1)){
IMP.scale.ord = cbind(IMP.scale.ord, 	(IMP[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

IMP[,real1] = IMP.scale.real
IMP[,ord1] = IMP.scale.ord

# now we loop through all induced missing values and add up the errors

for (j in 1:length(s)){
	ind = s[j]
#	print(j)
	
# converting indices to matrix row and columns
	r = ((ind-1) %% n) + 1 
	c = floor((ind-1) / n) + 1
	
# for categorical columns loss is whether or not mis-classified  (also keeping track of number of factor levels here)

	if ( is.element(c,cat1)){
		l = l+ (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lind = (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lcol[c] = lcol[c]+ lind} 
	 

# for remaining columns loss is squared difference
		
    if (!is.element(c,cat1) ) {l = l + (df.base[r,c] - IMP[r,c])^2
    	lind = (df.base[r,c] - IMP[r,c])^2	
	lcol[c] = lcol[c] + lind}
	
		#print(c(j, c, r, lind, lcol[c], l))
		}	

	rm(lind)
	
# get back total loss, columnwise loss and checks for factor levels
	return(list(loss = l, lcol = lcol))}


# add factor levels to make comparisons possible if needed 

factorLevels <- function(IMP, df.base, cat1){
	for (k in 1: length(cat1)){
		levs = levels(df.base[, cat1[k]])
		IMP[, cat1[k]] = factor(IMP[, cat1[k]], levels = levs)
		print(c(k, cat1[k], sum(is.na(IMP[,cat1[k]]))))
		}
       
       IMPfin = IMP
       
       for (h in 1: length(cat1)){
       	g = length(levels(df.base[,cat1[h]]))
       	b = length(levels(IMPfin[, cat1[h]]))
       	print(g-b)}
      
 return(IMPfin)}

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

df.cv = read.csv("GSS80.csv")[,-1] #df with CV indices removed

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))

a= which(is.na(df.base))
b= which(is.na(df.cv))
s = b[!is.element(b,a)] # elements on which to do cv


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/AMELIA")

load('amelia_GSS80_fin.RData')
load('amelia_GSS80_fin_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/MICE")

load('MICE_GSS80.RData')
load('MICE_GSS80_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparison")

# Amelia
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)
AML8= factorLevels(am8, df.base, cat1)
AML9= factorLevels(am9, df.base, cat1)
AML10= factorLevels(am10, df.base, cat1)
AML11= factorLevels(am11, df.base, cat1)
AML12= factorLevels(am12, df.base, cat1)
AML13= factorLevels(am13, df.base, cat1)
AML14= factorLevels(am14, df.base, cat1)
AML15= factorLevels(am15, df.base, cat1)
AML16= factorLevels(am16, df.base, cat1)
AML17= factorLevels(am17, df.base, cat1)
AML18= factorLevels(am18, df.base, cat1)
AML19= factorLevels(am19, df.base, cat1)
AML20= factorLevels(am20, df.base, cat1)



# MICE
MICE1 = factorLevels(W1, df.base, cat1)
MICE2 = factorLevels(W2, df.base, cat1)
MICE3 = factorLevels(W3, df.base, cat1)
MICE4 = factorLevels(W4, df.base, cat1)
MICE5 = factorLevels(W5, df.base, cat1)
MICE6 = factorLevels(W6, df.base, cat1)
MICE7 = factorLevels(W7, df.base, cat1)
MICE8 = factorLevels(W8, df.base, cat1)
MICE9 = factorLevels(W9, df.base, cat1)
MICE10 = factorLevels(W10, df.base, cat1)
MICE11 = factorLevels(W11, df.base, cat1)
MICE12 = factorLevels(W12, df.base, cat1)
MICE13 = factorLevels(W13, df.base, cat1)
MICE14 = factorLevels(W14, df.base, cat1)
MICE15 = factorLevels(W15, df.base, cat1)
MICE16 = factorLevels(W16, df.base, cat1)
MICE17 = factorLevels(W17, df.base, cat1)
MICE18 = factorLevels(W18, df.base, cat1)
MICE19 = factorLevels(W19, df.base, cat1)
MICE20 = factorLevels(W20, df.base, cat1)

#MATRIX FAC
gfrm = trace
glrm = rank
GFRM = factorLevels(gfrm, df.base, cat1)
GLRM = factorLevels(glrm, df.base, cat1)


# LOSS CALCULATIONS AMELIA AND MICE
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  

all.aml80 = l.aml
all.mice80 = l.mice

save(all.aml80, all.mice80, file = "aml_mice_GSS80.RData")

rm(list = ls())


######### 90


# given dataframe df , list of categoricals cat1, imputed dataset IMP

library("xtable")

rm(list = ls())

###Calculating scaled loss given original data df.base, categorical columns cat1, imputed dataset IMP


loss = function(IMP, df.base, s, cat1, ord1, real1){

	l = 0
	lcol = rep(0, dim(IMP)[2])
	n = nrow(IMP)
	m = ncol(IMP)
df.base0 = df.base

# scale numeric & integer columns and attach scaled columns to categorical columns

df.scale.real = scale(df.base[, real1])
df.scale.ord = numeric()
for (o in 1: length(ord1)){
df.scale.ord = cbind(df.scale.ord, 	(df.base[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

df.base[,real1] = df.scale.real
df.base[,ord1] = df.scale.ord

IMP.scale.real = scale(IMP[, real1])
IMP.scale.ord = numeric()
for (o in 1: length(ord1)){
IMP.scale.ord = cbind(IMP.scale.ord, 	(IMP[, ord1[o]] - min(df.base0[, ord1[o]] , na.rm = TRUE))/ (max(df.base0[, ord1[o]], na.rm = TRUE) - min(df.base0[, ord1[o]], na.rm = TRUE )))
}

IMP[,real1] = IMP.scale.real
IMP[,ord1] = IMP.scale.ord

# now we loop through all induced missing values and add up the errors

for (j in 1:length(s)){
	ind = s[j]
#	print(j)
	
# converting indices to matrix row and columns
	r = ((ind-1) %% n) + 1 
	c = floor((ind-1) / n) + 1
	
# for categorical columns loss is whether or not mis-classified  (also keeping track of number of factor levels here)

	if ( is.element(c,cat1)){
		l = l+ (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lind = (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lcol[c] = lcol[c]+ lind} 
	 

# for remaining columns loss is squared difference
		
    if (!is.element(c,cat1) ) {l = l + (df.base[r,c] - IMP[r,c])^2
    	lind = (df.base[r,c] - IMP[r,c])^2	
	lcol[c] = lcol[c] + lind}
	
		#print(c(j, c, r, lind, lcol[c], l))
		}	

	rm(lind)
	
# get back total loss, columnwise loss and checks for factor levels
	return(list(loss = l, lcol = lcol))}


# add factor levels to make comparisons possible if needed 

factorLevels <- function(IMP, df.base, cat1){
	for (k in 1: length(cat1)){
		levs = levels(df.base[, cat1[k]])
		IMP[, cat1[k]] = factor(IMP[, cat1[k]], levels = levs)
		print(c(k, cat1[k], sum(is.na(IMP[,cat1[k]]))))
		}
       
       IMPfin = IMP
       
       for (h in 1: length(cat1)){
       	g = length(levels(df.base[,cat1[h]]))
       	b = length(levels(IMPfin[, cat1[h]]))
       	print(g-b)}
      
 return(IMPfin)}

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

df.cv = read.csv("GSS90.csv")[,-1] #df with CV indices removed

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))

a= which(is.na(df.base))
b= which(is.na(df.cv))
s = b[!is.element(b,a)] # elements on which to do cv


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/AMELIA")

load('amelia_GSS90_fin.RData')
load('amelia_GSS90_fin_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/MICE")

load('MICE_GSS90.RData')
load('MICE_GSS90_copy.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/TRACE")

trace = read.csv("Trace_GSS90.csv")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/RANK")


rank = read.csv("Rank_GSS90.csv")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparison")

# Amelia
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)
AML8= factorLevels(am8, df.base, cat1)
AML9= factorLevels(am9, df.base, cat1)
AML10= factorLevels(am10, df.base, cat1)
AML11= factorLevels(am11, df.base, cat1)
AML12= factorLevels(am12, df.base, cat1)
AML13= factorLevels(am13, df.base, cat1)
AML14= factorLevels(am14, df.base, cat1)
AML15= factorLevels(am15, df.base, cat1)
AML16= factorLevels(am16, df.base, cat1)
AML17= factorLevels(am17, df.base, cat1)
AML18= factorLevels(am18, df.base, cat1)
AML19= factorLevels(am19, df.base, cat1)
AML20= factorLevels(am20, df.base, cat1)



# MICE
MICE1 = factorLevels(W1, df.base, cat1)
MICE2 = factorLevels(W2, df.base, cat1)
MICE3 = factorLevels(W3, df.base, cat1)
MICE4 = factorLevels(W4, df.base, cat1)
MICE5 = factorLevels(W5, df.base, cat1)
MICE6 = factorLevels(W6, df.base, cat1)
MICE7 = factorLevels(W7, df.base, cat1)
MICE8 = factorLevels(W8, df.base, cat1)
MICE9 = factorLevels(W9, df.base, cat1)
MICE10 = factorLevels(W10, df.base, cat1)
MICE11 = factorLevels(W11, df.base, cat1)
MICE12 = factorLevels(W12, df.base, cat1)
MICE13 = factorLevels(W13, df.base, cat1)
MICE14 = factorLevels(W14, df.base, cat1)
MICE15 = factorLevels(W15, df.base, cat1)
MICE16 = factorLevels(W16, df.base, cat1)
MICE17 = factorLevels(W17, df.base, cat1)
MICE18 = factorLevels(W18, df.base, cat1)
MICE19 = factorLevels(W19, df.base, cat1)
MICE20 = factorLevels(W20, df.base, cat1)

#MATRIX FAC
gfrm = trace
glrm = rank
GFRM = factorLevels(gfrm, df.base, cat1)
GLRM = factorLevels(glrm, df.base, cat1)


# LOSS CALCULATIONS AMELIA AND MICE
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  

all.aml90 = l.aml
all.mice90 = l.mice

save(all.aml90, all.mice90, file = "aml_mice_GSS90.RData")

rm(list = ls())


######### comparisons


####################### overall losses ###################################

rm(list = ls())

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))


df.cv = read.csv("GSS10.csv")[,-1] #df with CV indices removed
a= which(is.na(df.base))
b= which(is.na(df.cv))
s10 = length(b[!is.element(b,a)]) # elements on which to do cv
rm(df.cv, a, b)


df.cv = read.csv("GSS20.csv")[,-1] #df with CV indices removed
a= which(is.na(df.base))
b= which(is.na(df.cv))
s20 = length(b[!is.element(b,a)]) # elements on which to do cv
rm(df.cv, a, b)


df.cv = read.csv("GSS30.csv")[,-1] #df with CV indices removed
a= which(is.na(df.base))
b= which(is.na(df.cv))
s30 = length(b[!is.element(b,a)]) # elements on which to do cv
rm(df.cv, a, b)


df.cv = read.csv("GSS40.csv")[,-1] #df with CV indices removed
a= which(is.na(df.base))
b= which(is.na(df.cv))
s40 = length(b[!is.element(b,a)]) # elements on which to do cv
rm(df.cv, a, b)


df.cv = read.csv("GSS50.csv")[,-1] #df with CV indices removed
a= which(is.na(df.base))
b= which(is.na(df.cv))
s50 = length(b[!is.element(b,a)]) # elements on which to do cv
rm(df.cv, a, b)


df.cv = read.csv("GSS60.csv")[,-1] #df with CV indices removed
a= which(is.na(df.base))
b= which(is.na(df.cv))
s60 = length(b[!is.element(b,a)]) # elements on which to do cv
rm(df.cv, a, b)


df.cv = read.csv("GSS70.csv")[,-1] #df with CV indices removed
a= which(is.na(df.base))
b= which(is.na(df.cv))
s70 = length(b[!is.element(b,a)]) # elements on which to do cv
rm(df.cv, a, b)


df.cv = read.csv("GSS80.csv")[,-1] #df with CV indices removed
a= which(is.na(df.base))
b= which(is.na(df.cv))
s80 = length(b[!is.element(b,a)]) # elements on which to do cv
rm(df.cv, a, b)

df.cv = read.csv("GSS90.csv")[,-1] #df with CV indices removed
a= which(is.na(df.base))
b= which(is.na(df.cv))
s90 = length(b[!is.element(b,a)]) # elements on which to do cv
rm(df.cv, a, b)

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparison")

load('all_GSS10.RData')
load('all_GSS20.RData')
load('all_GSS30.RData')
load('all_GSS40.RData')
load('all_GSS50.RData')
load('all_GSS60.RData')
load('all_GSS70.RData')
load('all_GSS80.RData')
load('all_GSS90.RData')

load('aml_mice_GSS10.RData')
load('aml_mice_GSS20.RData')
load('aml_mice_GSS30.RData')
load('aml_mice_GSS40.RData')
load('aml_mice_GSS50.RData')
load('aml_mice_GSS60.RData')
load('aml_mice_GSS70.RData')
load('aml_mice_GSS80.RData')
load('aml_mice_GSS90.RData')





TR = rbind(l.tr10/s10, l.tr20/s20, l.tr30/s30, l.tr40/s40, l.tr50/s50, l.tr60/s60, l.tr70/s70, l.tr80/s80, l.tr90/s90)
RK = rbind(l.rnk10/s10, l.rnk20/s20, l.rnk30/s30, l.rnk40/s40, l.rnk50/s50, l.rnk60/s60, l.rnk70/s70, l.rnk80/s80, l.rnk90/s90)
MICE = rbind(l.mice10/s10, l.mice20/s20, l.mice30/s30, l.mice40/s40, l.mice50/s50, l.mice60/s60, l.mice70/s70, l.mice80/s80, l.mice90/s90)
AML = rbind(l.aml10/s10, l.aml20/s20, l.aml30/s30, l.aml40/s40, l.aml50/s50, l.aml60/s60, l.aml70/s70, l.aml80/s80, l.aml90/s90)
MC = c(all.mice10/s10, all.mice20/s20, all.mice30/s30, all.mice40/s40, all.mice50/s50, all.mice60/s60, all.mice70/s70, all.mice80/s80, all.mice90/s90 )
AM = c(all.aml10/s10, all.aml20/s20, all.aml30/s30, all.aml40/s40, all.aml50/s50, all.aml60/s60, all.aml70/s70, all.aml80/s80, all.aml90/s90 )



missing_prop = seq(10, 90, by = 10)
ymax = 1.05*max(c(max(TR, na.rm = TRUE), max(RK, na.rm = TRUE), max(MC, na.rm = TRUE), max(AM, na.rm = TRUE)))
ymin = 0.95*min(c(min(TR, na.rm = TRUE), min(RK, na.rm = TRUE), min(MC, na.rm = TRUE), min(AM, na.rm = TRUE))) 

#pdf("sparsity_gss_single.pdf")


check = c(rep(10, 20), rep(20, 20), rep(30, 20), rep(40, 20), rep(50, 20), rep(60, 20) , rep(70, 20), rep(80, 20), rep(90, 20)  )

checkaml = c(rep(10, 19), rep(20, 19), rep(30, 19), rep(40, 19), rep(50, 19), rep(60, 19) , rep(70, 19), rep(80, 19), rep(90, 19)  )



pdf("sparsity_gss_single.pdf")
boxplot(as.vector(AM) ~ check, ylim = c(ymin, ymax ), xlim = c(0.5, 9.5), main = "Prediction Error: GSS", xlab ="Percentage of entries missing", ylab = "Total Error Metric", boxlwd = 1, boxcol = "red", medlwd = 2, medcol = "black", whiskcol = "red", outcol = "red", staplecol = "red", col ="red")
boxplot(as.vector(MC) ~check,  col = "darkgreen", add= TRUE,  boxlwd = 1, boxcol = "darkgreen", medlwd = 2, medcol = "black", whiskcol = "darkgreen", outcol = "darkgreen", staplecol = "darkgreen")
lines(seq(1,9), TR, type = "b", col = rgb(0,0,1), pch = 19, lwd = 1.5)
lines(seq(1,9), RK, type = "b", col = "orange", pch = 19, lwd = 1.5)
lines(seq(1,9), AML, type = "b", col = rgb(1,0,0), pch = 19, lwd = 1.5)
lines(seq(1,9), MICE, type = "b", col = "darkgreen", pch = 19, lwd = 1.5)

legend("topleft", c("Amelia", "MICE", "Rank", "Trace"),col =  c("red", "darkgreen", "orange", "blue"), lty = 1, pch = 19 ,pt.bg = 'white')
dev.off()

