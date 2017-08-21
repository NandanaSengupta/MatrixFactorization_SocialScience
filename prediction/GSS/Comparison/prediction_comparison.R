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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/TRACE")

trace = read.csv("Trace_GSS10.csv")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/RANK")


rank = read.csv("Rank_GSS10.csv")

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

# matrix factorization 

l.tr= loss(GFRM,df.base,s,cat1,ord1, real1 )
l.rnk= loss(GLRM,df.base,s,cat1,ord1, real1 )

# saving

l.aml10 = mean(l.aml, na.rm = TRUE)
l.mice10 = mean(l.mice, na.rm = TRUE)

LAML10 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol, LOSS.AML5$lcol, LOSS.AML6$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol, LOSS.AML10$lcol, LOSS.AML11$lcol, LOSS.AML12$lcol, LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML15$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol, LOSS.AML19$lcol, LOSS.AML20$lcol)
LMICE10 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)
LOSSAML10 = apply(LAML10, 1, mean, na.rm = TRUE)
LOSSMICE10 = apply(LMICE10, 1, mean, na.rm = TRUE)

l.tr10 = l.tr$loss
l.rnk10 = l.rnk$loss
LOSSTRACE10 = l.tr$lcol
LOSSRANK10 = l.rnk$lcol


save(l.tr10, l.rnk10, l.aml10, l.mice10, LOSSAML10, LOSSMICE10, LOSSTRACE10,  LOSSRANK10 , file = "all_GSS10.RData")

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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/TRACE")

trace = read.csv("Trace_GSS20.csv")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/RANK")


rank = read.csv("Rank_GSS20.csv")

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

# matrix factorization 

l.tr= loss(GFRM,df.base,s,cat1,ord1, real1 )
l.rnk= loss(GLRM,df.base,s,cat1,ord1, real1 )

# saving

l.aml20 = mean(l.aml, na.rm = TRUE)
l.mice20 = mean(l.mice, na.rm = TRUE)

LAML20 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol, LOSS.AML5$lcol, LOSS.AML6$lcol, LOSS.AML7$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol, LOSS.AML11$lcol, LOSS.AML12$lcol, LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML15$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol, LOSS.AML19$lcol, LOSS.AML20$lcol)

LMICE20 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LOSSAML20 = apply(LAML20, 1, mean, na.rm = TRUE)
LOSSMICE20 = apply(LMICE20, 1, mean, na.rm = TRUE)

l.tr20 = l.tr$loss
l.rnk20 = l.rnk$loss
LOSSTRACE20 = l.tr$lcol
LOSSRANK20 = l.rnk$lcol


save(l.tr20, l.rnk20, l.aml20, l.mice20, LOSSAML20, LOSSMICE20, LOSSTRACE20,  LOSSRANK20 , file = "all_GSS20.RData")

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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/TRACE")

trace = read.csv("Trace_GSS30.csv")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/RANK")


rank = read.csv("Rank_GSS30.csv")

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

# matrix factorization 

l.tr= loss(GFRM,df.base,s,cat1,ord1, real1 )
l.rnk= loss(GLRM,df.base,s,cat1,ord1, real1 )

# saving

l.aml30 = mean(l.aml, na.rm = TRUE)
l.mice30 = mean(l.mice, na.rm = TRUE)

LAML30 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol, LOSS.AML6$lcol, LOSS.AML7$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol, LOSS.AML10$lcol, LOSS.AML12$lcol, LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol, LOSS.AML19$lcol, LOSS.AML20$lcol)

LMICE30 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LOSSAML30 = apply(LAML30, 1, mean, na.rm = TRUE)
LOSSMICE30 = apply(LMICE30, 1, mean, na.rm = TRUE)

l.tr30 = l.tr$loss
l.rnk30 = l.rnk$loss
LOSSTRACE30 = l.tr$lcol
LOSSRANK30 = l.rnk$lcol


save(l.tr30, l.rnk30, l.aml30, l.mice30, LOSSAML30, LOSSMICE30, LOSSTRACE30,  LOSSRANK30 , file = "all_GSS30.RData")

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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/TRACE")

trace = read.csv("Trace_GSS40.csv")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/RANK")


rank = read.csv("Rank_GSS40.csv")

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

# matrix factorization 

l.tr= loss(GFRM,df.base,s,cat1,ord1, real1 )
l.rnk= loss(GLRM,df.base,s,cat1,ord1, real1 )

# saving

l.aml40 = mean(l.aml, na.rm = TRUE)
l.mice40 = mean(l.mice, na.rm = TRUE)

LAML40 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol, LOSS.AML5$lcol, LOSS.AML6$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol, LOSS.AML10$lcol, LOSS.AML11$lcol, LOSS.AML12$lcol, LOSS.AML13$lcol,  LOSS.AML15$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol,  LOSS.AML20$lcol)

LMICE40 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LOSSAML40 = apply(LAML40, 1, mean, na.rm = TRUE)
LOSSMICE40 = apply(LMICE40, 1, mean, na.rm = TRUE)

l.tr40 = l.tr$loss
l.rnk40 = l.rnk$loss
LOSSTRACE40 = l.tr$lcol
LOSSRANK40 = l.rnk$lcol


save(l.tr40, l.rnk40, l.aml40, l.mice40, LOSSAML40, LOSSMICE40, LOSSTRACE40,  LOSSRANK40 , file = "all_GSS40.RData")

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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/TRACE")

trace = read.csv("Trace_GSS50.csv")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/RANK")


rank = read.csv("Rank_GSS50.csv")

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

# matrix factorization 

l.tr= loss(GFRM,df.base,s,cat1,ord1, real1 )
l.rnk= loss(GLRM,df.base,s,cat1,ord1, real1 )

# saving

l.aml50 = mean(l.aml, na.rm = TRUE)
l.mice50 = mean(l.mice, na.rm = TRUE)


LAML50 = cbind( LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol,  LOSS.AML7$lcol, LOSS.AML9$lcol, LOSS.AML11$lcol, LOSS.AML14$lcol,  LOSS.AML18$lcol, LOSS.AML20$lcol)

LMICE50 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LOSSAML50 = apply(LAML50, 1, mean, na.rm = TRUE)
LOSSMICE50 = apply(LMICE50, 1, mean, na.rm = TRUE)

l.tr50 = l.tr$loss
l.rnk50 = l.rnk$loss
LOSSTRACE50 = l.tr$lcol
LOSSRANK50 = l.rnk$lcol


save(l.tr50, l.rnk50, l.aml50, l.mice50, LOSSAML50, LOSSMICE50, LOSSTRACE50,  LOSSRANK50 , file = "all_GSS50.RData")

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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/TRACE")

trace = read.csv("Trace_GSS60.csv")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/RANK")


rank = read.csv("Rank_GSS60.csv")

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

# matrix factorization 

l.tr= loss(GFRM,df.base,s,cat1,ord1, real1 )
l.rnk= loss(GLRM,df.base,s,cat1,ord1, real1 )

# saving

l.aml60 = mean(l.aml, na.rm = TRUE)
l.mice60 = mean(l.mice, na.rm = TRUE)


LAML60 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol,  LOSS.AML4$lcol, LOSS.AML5$lcol,  LOSS.AML7$lcol,  LOSS.AML9$lcol, LOSS.AML10$lcol,  LOSS.AML12$lcol, LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML15$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol, LOSS.AML20$lcol)

LMICE60 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LOSSAML60 = apply(LAML60, 1, mean, na.rm = TRUE)
LOSSMICE60 = apply(LMICE60, 1, mean, na.rm = TRUE)

l.tr60 = l.tr$loss
l.rnk60 = l.rnk$loss
LOSSTRACE60 = l.tr$lcol
LOSSRANK60 = l.rnk$lcol


save(l.tr60, l.rnk60, l.aml60, l.mice60, LOSSAML60, LOSSMICE60, LOSSTRACE60,  LOSSRANK60 , file = "all_GSS60.RData")

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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/TRACE")

trace = read.csv("Trace_GSS70.csv")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/RANK")


rank = read.csv("Rank_GSS70.csv")

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

# matrix factorization 

l.tr= loss(GFRM,df.base,s,cat1,ord1, real1 )
l.rnk= loss(GLRM,df.base,s,cat1,ord1, real1 )

# saving

l.aml70 = mean(l.aml, na.rm = TRUE)
l.mice70 = mean(l.mice, na.rm = TRUE)


LAML70 = cbind(LOSS.AML1$lcol, LOSS.AML4$lcol,  LOSS.AML6$lcol, LOSS.AML7$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol,  LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML15$lcol, LOSS.AML16$lcol)

LMICE70 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LOSSAML70 = apply(LAML70, 1, mean, na.rm = TRUE)
LOSSMICE70 = apply(LMICE70, 1, mean, na.rm = TRUE)

l.tr70 = l.tr$loss
l.rnk70 = l.rnk$loss
LOSSTRACE70 = l.tr$lcol
LOSSRANK70 = l.rnk$lcol


save(l.tr70, l.rnk70, l.aml70, l.mice70, LOSSAML70, LOSSMICE70, LOSSTRACE70,  LOSSRANK70 , file = "all_GSS70.RData")

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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/TRACE")

trace = read.csv("Trace_GSS80.csv")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/RANK")


rank = read.csv("Rank_GSS80.csv")

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

# matrix factorization 

l.tr= loss(GFRM,df.base,s,cat1,ord1, real1 )
l.rnk= loss(GLRM,df.base,s,cat1,ord1, real1 )

# saving

l.aml80 = mean(l.aml, na.rm = TRUE)
l.mice80 = mean(l.mice, na.rm = TRUE)


LAML80 = cbind(LOSS.AML1$lcol, LOSS.AML10$lcol, LOSS.AML12$lcol, LOSS.AML18$lcol, LOSS.AML19$lcol, LOSS.AML20$lcol)

LOSSAML80 = apply(LAML80, 1, mean, na.rm = TRUE)

l.tr80 = l.tr$loss
l.rnk80 = l.rnk$loss
LOSSTRACE80 = l.tr$lcol
LOSSRANK80 = l.rnk$lcol


save(l.tr80, l.rnk80, l.aml80, l.mice80, LOSSAML80, LOSSTRACE80,  LOSSRANK80 , file = "all_GSS80.RData")

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

# matrix factorization 

l.tr= loss(GFRM,df.base,s,cat1,ord1, real1 )
l.rnk= loss(GLRM,df.base,s,cat1,ord1, real1 )

# saving

l.aml90 = mean(l.aml, na.rm = TRUE)
l.mice90 = mean(l.mice, na.rm = TRUE)

LAML90 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol, LOSS.AML5$lcol, LOSS.AML6$lcol,LOSS.AML7$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol, LOSS.AML10$lcol, LOSS.AML11$lcol, LOSS.AML12$lcol, LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML15$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol, LOSS.AML20$lcol)


LOSSAML90 = apply(LAML90, 1, mean, na.rm = TRUE)

l.tr90 = l.tr$loss
l.rnk90 = l.rnk$loss
LOSSTRACE90 = l.tr$lcol
LOSSRANK90 = l.rnk$lcol


save(l.tr90, l.rnk90, l.aml90, l.mice90, LOSSAML90, LOSSTRACE90,  LOSSRANK90 , file = "all_GSS90.RData")

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

TR = rbind(l.tr10/s10, l.tr20/s20, l.tr30/s30, l.tr40/s40, l.tr50/s50, l.tr60/s60, l.tr70/s70, l.tr80/s80, l.tr90/s90)
RK = rbind(l.rnk10/s10, l.rnk20/s20, l.rnk30/s30, l.rnk40/s40, l.rnk50/s50, l.rnk60/s60, l.rnk70/s70, l.rnk80/s80, l.rnk90/s90)
MC = rbind(l.mice10/s10, l.mice20/s20, l.mice30/s30, l.mice40/s40, l.mice50/s50, l.mice60/s60, l.mice70/s70, l.mice80/s80, l.mice90/s90)
AM = rbind(l.aml10/s10, l.aml20/s20, l.aml30/s30, l.aml40/s40, l.aml50/s50, l.aml60/s60, l.aml70/s70, l.aml80/s80, l.aml90/s90)

mat = cbind(AM, MC, RK, TR)

summ_mat = round(cbind(AM , MC, RK ,  TR, 100*(AM-RK)/AM,  100*(MC-RK)/MC,100*(AM-TR)/AM,  100*(MC-TR)/MC), 2)

colnames(summ_mat) = c("Amelia", "MICE", "GLRM", "Trace", "GLRM AM", "GLRM MICE","Trace AM", "Trace MICE"  )
rownames(summ_mat) = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")
library("xtable")
summTab = xtable(summ_mat, align = "|c|c|c|c|c|c|c|c|c|")
#print(summTab, type = "latex", file = "GSS_summary.tex")


missing_prop = seq(10, 90, by = 10)
ymax = 1.05*max(max(mat, na.rm = TRUE))
ymin = 0.95*min(min(mat, na.rm = TRUE)) 

quartz()
#pdf("sparsity_gss_single.pdf")
plot(missing_prop, AM, type = "b", col = rgb(1,0, 0), main = "Prediction Error: GSS", xlab ="Percentage of entries missing", ylab = "Total Error Metric", xlim = c(10, 90), ylim = c(ymin, ymax ), pch = 19, yaxt = "n")
lines(missing_prop, MC,  type = "b", col = "darkgreen", pch = 19)
lines(missing_prop, TR, type = "b", col = rgb(0,0,1), pch = 19)
lines(missing_prop, RK, type = "b", col = "orange", pch = 19)

axis(1, at = seq(10, 90, by = 10),  font=2 )
axis(2, at = seq(0.20, 0.50, by = 0.05),  font=2)

legend("topleft", c("Amelia", "MICE", "Rank", "Trace"),col =  c("red", "darkgreen", "orange", "blue"), lty = 1, pch = 19 ,pt.bg = 'white')
#dev.off()



##################### columnwise losses ###########################
#setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

#df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed

col.labs = names(df.base)

real_list = real1
ord_list = ord1
cat_list = cat1
all_col = c(length(real_list), length(ord_list), length(cat_list) )

meth.select = numeric()
col.meth.select = numeric()
lcol.all = cbind(LOSSAML10, LOSSMICE10, LOSSTRACE10, LOSSRANK10)

# columnwise gains over Amelia

tr.aml.lcol10  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]
rk.aml.lcol10  = (lcol.all[,1] -lcol.all[,4])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol10  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]
rk.mice.lcol10  = (lcol.all[,2] -lcol.all[,4])/lcol.all[,2]



# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account10= col.account[-1, ]

rk_list = which(meth.select == 4)
tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)



rk_col10 = c(sum(is.element(rk_list, real_list) == TRUE), sum(is.element(rk_list, ord_list)== TRUE), sum(is.element(rk_list, cat_list)== TRUE))
tr_col10 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col10 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col10 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test10 = rbind(aml_col10, mice_col10, rk_col10, tr_col10 )


rm(lcol.all, tr_list, aml_list, mice_list)

########## 20 

meth.select = numeric()
col.meth.select = numeric()
lcol.all = cbind(LOSSAML20, LOSSMICE20, LOSSTRACE20, LOSSRANK20)

# columnwise gains over Amelia

tr.aml.lcol20  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]
rk.aml.lcol20  = (lcol.all[,1] -lcol.all[,4])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol20  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]
rk.mice.lcol20  = (lcol.all[,2] -lcol.all[,4])/lcol.all[,2]



# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account20= col.account[-1, ]

rk_list = which(meth.select == 4)
tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)



rk_col20 = c(sum(is.element(rk_list, real_list) == TRUE), sum(is.element(rk_list, ord_list)== TRUE), sum(is.element(rk_list, cat_list)== TRUE))
tr_col20 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col20 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col20 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test20 = rbind(aml_col20, mice_col20, rk_col20, tr_col20 )


rm(lcol.all, tr_list, aml_list, mice_list)


########## 30 

meth.select = numeric()
col.meth.select = numeric()
lcol.all = cbind(LOSSAML30, LOSSMICE30, LOSSTRACE30, LOSSRANK30)

# columnwise gains over Amelia

tr.aml.lcol30  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]
rk.aml.lcol30  = (lcol.all[,1] -lcol.all[,4])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol30  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]
rk.mice.lcol30  = (lcol.all[,2] -lcol.all[,4])/lcol.all[,2]



# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account30= col.account[-1, ]

rk_list = which(meth.select == 4)
tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)



rk_col30 = c(sum(is.element(rk_list, real_list) == TRUE), sum(is.element(rk_list, ord_list)== TRUE), sum(is.element(rk_list, cat_list)== TRUE))
tr_col30 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col30 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col30 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test30 = rbind(aml_col30, mice_col30, rk_col30, tr_col30 )


rm(lcol.all, tr_list, aml_list, mice_list)

########## 40 

meth.select = numeric()
col.meth.select = numeric()
lcol.all = cbind(LOSSAML40, LOSSMICE40, LOSSTRACE40, LOSSRANK40)

# columnwise gains over Amelia

tr.aml.lcol40  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]
rk.aml.lcol40  = (lcol.all[,1] -lcol.all[,4])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol40  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]
rk.mice.lcol40  = (lcol.all[,2] -lcol.all[,4])/lcol.all[,2]



# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account40= col.account[-1, ]

rk_list = which(meth.select == 4)
tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)



rk_col40 = c(sum(is.element(rk_list, real_list) == TRUE), sum(is.element(rk_list, ord_list)== TRUE), sum(is.element(rk_list, cat_list)== TRUE))
tr_col40 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col40 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col40 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test40 = rbind(aml_col40, mice_col40, rk_col40, tr_col40 )


rm(lcol.all, tr_list, aml_list, mice_list)

########## 50 

meth.select = numeric()
col.meth.select = numeric()
lcol.all = cbind(LOSSAML50, LOSSMICE50, LOSSTRACE50, LOSSRANK50)

# columnwise gains over Amelia

tr.aml.lcol50  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]
rk.aml.lcol50  = (lcol.all[,1] -lcol.all[,4])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol50  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]
rk.mice.lcol50  = (lcol.all[,2] -lcol.all[,4])/lcol.all[,2]



# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account50= col.account[-1, ]

rk_list = which(meth.select == 4)
tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)



rk_col50 = c(sum(is.element(rk_list, real_list) == TRUE), sum(is.element(rk_list, ord_list)== TRUE), sum(is.element(rk_list, cat_list)== TRUE))
tr_col50 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col50 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col50 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test50 = rbind(aml_col50, mice_col50, rk_col50, tr_col50 )


rm(lcol.all, tr_list, aml_list, mice_list)

########## 60 

meth.select = numeric()
col.meth.select = numeric()
lcol.all = cbind(LOSSAML60, LOSSMICE60, LOSSTRACE60, LOSSRANK60)

# columnwise gains over Amelia

tr.aml.lcol60  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]
rk.aml.lcol60  = (lcol.all[,1] -lcol.all[,4])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol60  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]
rk.mice.lcol60  = (lcol.all[,2] -lcol.all[,4])/lcol.all[,2]



# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account60= col.account[-1, ]

rk_list = which(meth.select == 4)
tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)



rk_col60 = c(sum(is.element(rk_list, real_list) == TRUE), sum(is.element(rk_list, ord_list)== TRUE), sum(is.element(rk_list, cat_list)== TRUE))
tr_col60 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col60 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col60 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test60 = rbind(aml_col60, mice_col60, rk_col60, tr_col60 )


rm(lcol.all, tr_list, aml_list, mice_list)

########## 70 

meth.select = numeric()
col.meth.select = numeric()
lcol.all = cbind(LOSSAML70, LOSSMICE70, LOSSTRACE70, LOSSRANK70)

# columnwise gains over Amelia

tr.aml.lcol70  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]
rk.aml.lcol70  = (lcol.all[,1] -lcol.all[,4])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol70  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]
rk.mice.lcol70  = (lcol.all[,2] -lcol.all[,4])/lcol.all[,2]



# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account70= col.account[-1, ]

rk_list = which(meth.select == 4)
tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)



rk_col70 = c(sum(is.element(rk_list, real_list) == TRUE), sum(is.element(rk_list, ord_list)== TRUE), sum(is.element(rk_list, cat_list)== TRUE))
tr_col70 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col70 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col70 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test70 = rbind(aml_col70, mice_col70, rk_col70, tr_col70 )


rm(lcol.all, tr_list, aml_list, mice_list)

########## 80 

meth.select = numeric()
col.meth.select = numeric()
lcol.all = cbind(LOSSAML80, NA, LOSSTRACE80, LOSSRANK80)

# columnwise gains over Amelia

tr.aml.lcol80  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]
rk.aml.lcol80  = (lcol.all[,1] -lcol.all[,4])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol80  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]
rk.mice.lcol80  = (lcol.all[,2] -lcol.all[,4])/lcol.all[,2]



# columwise lowest loss

for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,], na.rm = TRUE)))	}
	
	
rk_list = which(meth.select == 4)
tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)



rk_col80 = c(sum(is.element(rk_list, real_list) == TRUE), sum(is.element(rk_list, ord_list)== TRUE), sum(is.element(rk_list, cat_list)== TRUE))
tr_col80 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col80 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col80 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test80 = rbind(aml_col80, mice_col80, rk_col80, tr_col80 )


rm(lcol.all, tr_list, aml_list, mice_list)



########## 90 

meth.select = numeric()
col.meth.select = numeric()
lcol.all = cbind(LOSSAML90, NA, LOSSTRACE90, LOSSRANK90)

# columnwise gains over Amelia

tr.aml.lcol90  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]
rk.aml.lcol90  = (lcol.all[,1] -lcol.all[,4])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol90  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]
rk.mice.lcol90  = (lcol.all[,2] -lcol.all[,4])/lcol.all[,2]



# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,], na.rm = TRUE)))	}

rk_list = which(meth.select == 4)
tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)



rk_col90 = c(sum(is.element(rk_list, real_list) == TRUE), sum(is.element(rk_list, ord_list)== TRUE), sum(is.element(rk_list, cat_list)== TRUE))
tr_col90 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col90 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col90 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test90 = rbind(aml_col90, mice_col90, rk_col90, tr_col90 )


rm(lcol.all, tr_list, aml_list, mice_list)


#pdf("columnwise_density_gss.pdf")
par(mfrow = c(3,3))
plot(density(tr.aml.lcol10*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 10%")
axis(2, at = seq(0, 0.04, 0.01) )#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol10*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol20*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 20%")
axis(2, at = seq(0, 0.04, 0.01) )#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol20*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol30*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 30%")
axis(2, at = seq(0, 0.04, 0.01) )#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol30*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)


plot(density(tr.aml.lcol40*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 40%")
axis(2, at = seq(0, 0.04, 0.01) )#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol40*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)


plot(density(tr.aml.lcol50*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 50%")
axis(2, at = seq(0, 0.04, 0.01) )#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol50*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol60*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.025), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 60%")
axis(2, at = seq(0, 0.04, 0.01) )#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol60*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol70*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 70%")
axis(2, at = seq(0, 0.04, 0.01) )#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol70*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol80*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 80%")
axis(2, at = seq(0, 0.04, 0.01) )#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol80*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol90*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 90% ")
axis(2, at = seq(0, 0.04, 0.01) )#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol90*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)
#dev.off()



quartz()
pdf("columnwise_density_gss_rk.pdf")
par(mfrow = c(3,3))
plot(density(rk.aml.lcol10*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 10%")
axis(2, at = seq(0, 0.04, 0.01))#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(rk.mice.lcol10*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(rk.aml.lcol20*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 20%")
axis(2, at = seq(0, 0.04, 0.01))#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(rk.mice.lcol20*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(rk.aml.lcol30*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 30%")
axis(2, at = seq(0, 0.04, 0.01))#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(rk.mice.lcol30*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)


plot(density(rk.aml.lcol40*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 40%")
axis(2, at = seq(0, 0.04, 0.01))#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(rk.mice.lcol40*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)


plot(density(rk.aml.lcol50*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 50%")
axis(2, at = seq(0, 0.04, 0.01))#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(rk.mice.lcol50*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(rk.aml.lcol60*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.025), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 60%")
axis(2, at = seq(0, 0.04, 0.01))#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(rk.mice.lcol60*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(rk.aml.lcol70*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 70%")
axis(2, at = seq(0, 0.04, 0.01))#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(rk.mice.lcol70*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(rk.aml.lcol80*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 80%")
axis(2, at = seq(0, 0.04, 0.01))#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(rk.mice.lcol80*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(rk.aml.lcol90*100), col = "red", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 90% ")
axis(2, at = seq(0, 0.04, 0.01))#, labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(rk.mice.lcol90*100), col = "darkgreen", lwd = 5)
legend("topleft", c("Amelia", "Mice"), lty = c(1,1), cex = 0.90, lwd = c(5,5),  col = c("red", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)
dev.off()

quartz()
pdf("columnwise_technique_gss.pdf")
par(mfrow = c(3,3))
barplot(t(test10), horiz = TRUE,  beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE", "LOWRANK", "TRACE"), main = "GSS: 10% ")
barplot(t(test20), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE", "LOWRANK", "TRACE"), main = "GSS: 20% ")
barplot(t(test30), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE", "LOWRANK", "TRACE"), main = "GSS: 30% ")
barplot(t(test40), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE", "LOWRANK", "TRACE"), main = "GSS: 40% ")
barplot(t(test50), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE", "LOWRANK", "TRACE"), main = "GSS: 50% ")
barplot(t(test60), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE", "LOWRANK", "TRACE"), main = "GSS: 60% ")
barplot(t(test70), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE", "LOWRANK", "TRACE"), main = "GSS: 70% ")
barplot(t(test80), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE", "LOWRANK", "TRACE"), main = "GSS: 80% ")
barplot(t(test90), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE", "LOWRANK", "TRACE"), main = "GSS: 90% ")
dev.off()


quartz()

pdf("columnwise_datatype_gss.pdf")
par(mfrow = c(3,3))
barplot(test10, horiz = TRUE, beside = TRUE, legend=  c("Amelia", "Mice", "Rank", "Trace"), col = c("red", "darkgreen", "orange", "blue"), args.legend=list( x ="bottomright", bty = "n", cex = 0.95) , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 10% ")
barplot(test20, horiz = TRUE, col = c("red", "darkgreen", "orange", "blue"), beside = TRUE, legend=  c("Amelia", "Mice", "Rank", "Trace"), args.legend=list( x ="bottomright", bty = "n", cex = 0.95) , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 20% ")
barplot(test30, horiz = TRUE, col = c("red", "darkgreen", "orange", "blue"),  beside = TRUE, legend=  c("Amelia", "Mice", "Rank", "Trace"), args.legend=list( x ="bottomright", bty = "n", cex = 0.95) , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 30% ")
barplot(test40, horiz = TRUE, beside = TRUE, col = c("red", "darkgreen", "orange", "blue"),  legend=  c("Amelia", "Mice", "Rank", "Trace"), args.legend=list( x ="bottomright", bty = "n", cex = 0.95) , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 40% ")
barplot(test50, horiz = TRUE, beside = TRUE, col = c("red", "darkgreen", "orange", "blue"), legend=  c("Amelia", "Mice", "Rank", "Trace"), args.legend=list( x ="bottomright", bty = "n", cex = 0.95) , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 50% ")
barplot(test60, horiz = TRUE, beside = TRUE, col = c("red", "darkgreen", "orange", "blue"), legend=  c("Amelia", "Mice", "Rank", "Trace"), args.legend=list( x ="bottomright", bty = "n", cex = 0.95) , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 60% ")
barplot(test70, horiz = TRUE, beside = TRUE, col = c("red", "darkgreen", "orange", "blue"), legend=  c("Amelia", "Mice", "Rank", "Trace"), args.legend=list( x ="bottomright", bty = "n", cex = 0.95) , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 70% ")
barplot(test80, horiz = TRUE, beside = TRUE, col = c("red", "darkgreen", "orange", "blue"), legend=  c("Amelia", "Mice", "Rank", "Trace"), args.legend=list( x ="bottomright", bty = "n", cex = 0.95) , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 80% ")
barplot(test90, horiz = TRUE, beside = TRUE, col = c("red", "darkgreen", "orange", "blue"), legend=  c("Amelia", "Mice", "Rank", "Trace"), args.legend=list( x ="bottomright", bty = "n", cex = 0.95) , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 90% ")
dev.off()



