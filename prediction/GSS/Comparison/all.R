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

l.tr= loss(GLRM,df.base,s,cat1,ord1, real1 )
l.rnk= loss(GFRM,df.base,s,cat1,ord1, real1 )

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


##################################################################################
####### 20 
###################################################################################


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

trace = read.csv("Trace_GSS20fin.csv")
trace = trace[,-1]

trace1 = read.csv("Trace_GSS20fin_copy.csv")
trace1 = trace1[,-1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparisons")

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

#TRACE
glrm1 = trace[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm2 = trace[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm3 = trace[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm4 = trace[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm5 = trace[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm6 = trace[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm7 = trace[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm8 = trace[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm9 = trace[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm10 = trace[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]
glrm11 = trace1[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm12 = trace1[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm13 = trace1[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm14 = trace1[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm15 = trace1[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm16 = trace1[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm17 = trace1[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm18 = trace1[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm19 = trace1[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm20 = trace1[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]



GLRM_TR1 = factorLevels(glrm1, df.base, cat1)
GLRM_TR2 = factorLevels(glrm2, df.base, cat1)
GLRM_TR3 = factorLevels(glrm3, df.base, cat1)
GLRM_TR4 = factorLevels(glrm4, df.base, cat1)
GLRM_TR5 = factorLevels(glrm5, df.base, cat1)
GLRM_TR6 = factorLevels(glrm6, df.base, cat1)
GLRM_TR7 = factorLevels(glrm7, df.base, cat1)
GLRM_TR8 = factorLevels(glrm8, df.base, cat1)
GLRM_TR9 = factorLevels(glrm9, df.base, cat1)
GLRM_TR10 = factorLevels(glrm10, df.base, cat1)
GLRM_TR11 = factorLevels(glrm11, df.base, cat1)
GLRM_TR12 = factorLevels(glrm12, df.base, cat1)
GLRM_TR13 = factorLevels(glrm13, df.base, cat1)
GLRM_TR14 = factorLevels(glrm14, df.base, cat1)
GLRM_TR15 = factorLevels(glrm15, df.base, cat1)
GLRM_TR16 = factorLevels(glrm16, df.base, cat1)
GLRM_TR17 = factorLevels(glrm17, df.base, cat1)
GLRM_TR18 = factorLevels(glrm18, df.base, cat1)
GLRM_TR19 = factorLevels(glrm19, df.base, cat1)
GLRM_TR20 = factorLevels(glrm20, df.base, cat1)


# LOSS CALCULATIONS
l.tr = rep(NA, 20)
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR1 = loss(GLRM_TR1,df.base,s,cat1,ord1, real1 )

l.tr[1]   = sum(LOSS.GLRM_TR1$lcol)
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR2 = loss(GLRM_TR2,df.base,s,cat1,ord1, real1 )

l.tr[2] = sum(LOSS.GLRM_TR2$lcol)
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR3 = loss(GLRM_TR3,df.base,s,cat1,ord1, real1 )

l.tr[3] = sum(LOSS.GLRM_TR3$lcol)
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR4 = loss(GLRM_TR4,df.base,s,cat1,ord1, real1 )

l.tr[4] = sum(LOSS.GLRM_TR4$lcol)
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR5 = loss(GLRM_TR5,df.base,s,cat1,ord1, real1 )

l.tr[5] = sum(LOSS.GLRM_TR5$lcol)
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR6 = loss(GLRM_TR6,df.base,s,cat1,ord1, real1 )

l.tr[6]   = sum(LOSS.GLRM_TR6$lcol)
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR7 = loss(GLRM_TR7,df.base,s,cat1,ord1, real1 )

l.tr[7] = sum(LOSS.GLRM_TR7$lcol)
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR8 = loss(GLRM_TR8,df.base,s,cat1,ord1, real1 )

l.tr[8] = sum(LOSS.GLRM_TR8$lcol)
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR9 = loss(GLRM_TR9,df.base,s,cat1,ord1, real1 )

l.tr[9] = sum(LOSS.GLRM_TR9$lcol)
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR10 = loss(GLRM_TR10,df.base,s,cat1,ord1, real1 )

l.tr[10] = sum(LOSS.GLRM_TR10$lcol)
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR11 = loss(GLRM_TR11,df.base,s,cat1,ord1, real1 )

l.tr[11]   = sum(LOSS.GLRM_TR11$lcol)
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR12 = loss(GLRM_TR12,df.base,s,cat1,ord1, real1 )

l.tr[12] = sum(LOSS.GLRM_TR12$lcol)
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR13 = loss(GLRM_TR13,df.base,s,cat1,ord1, real1 )

l.tr[13] = sum(LOSS.GLRM_TR13$lcol)
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR14 = loss(GLRM_TR14,df.base,s,cat1,ord1, real1 )

l.tr[14] = sum(LOSS.GLRM_TR14$lcol)
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR15 = loss(GLRM_TR15,df.base,s,cat1,ord1, real1 )

l.tr[15] = sum(LOSS.GLRM_TR15$lcol)
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR16 = loss(GLRM_TR16,df.base,s,cat1,ord1, real1 )

l.tr[16]   = sum(LOSS.GLRM_TR16$lcol)
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR17 = loss(GLRM_TR17,df.base,s,cat1,ord1, real1 )

l.tr[17] = sum(LOSS.GLRM_TR17$lcol)
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR18 = loss(GLRM_TR18,df.base,s,cat1,ord1, real1 )

l.tr[18] = sum(LOSS.GLRM_TR18$lcol)
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR19 = loss(GLRM_TR19,df.base,s,cat1,ord1, real1 )

l.tr[19] = sum(LOSS.GLRM_TR19$lcol)
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR20 = loss(GLRM_TR20,df.base,s,cat1,ord1, real1 )

l.tr[20] = sum(LOSS.GLRM_TR20$lcol)
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  



l.tr20 = l.tr
l.aml20 = l.aml
l.mice20 = l.mice

LAML20 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol, LOSS.AML5$lcol, LOSS.AML6$lcol, LOSS.AML7$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol, LOSS.AML11$lcol, LOSS.AML12$lcol, LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML15$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol, LOSS.AML19$lcol, LOSS.AML20$lcol)

LMICE20 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LTRACE20 = cbind(LOSS.GLRM_TR1$lcol, LOSS.GLRM_TR2$lcol, LOSS.GLRM_TR3$lcol, LOSS.GLRM_TR4$lcol, LOSS.GLRM_TR5$lcol, LOSS.GLRM_TR6$lcol, LOSS.GLRM_TR7$lcol, LOSS.GLRM_TR8$lcol, LOSS.GLRM_TR9$lcol, LOSS.GLRM_TR10$lcol, LOSS.GLRM_TR11$lcol, LOSS.GLRM_TR12$lcol, LOSS.GLRM_TR13$lcol, LOSS.GLRM_TR14$lcol, LOSS.GLRM_TR15$lcol, LOSS.GLRM_TR16$lcol, LOSS.GLRM_TR17$lcol, LOSS.GLRM_TR18$lcol, LOSS.GLRM_TR19$lcol, LOSS.GLRM_TR20$lcol)


LOSSAML20 = apply(LAML20, 1, mean, na.rm = TRUE)
LOSSMICE20 = apply(LMICE20, 1, mean, na.rm = TRUE)
LOSSTRACE20 = apply(LTRACE20, 1, mean, na.rm = TRUE)

save(l.tr20, l.aml20, l.mice20, LAML20, LMICE20, LTRACE20, LOSSAML20, LOSSMICE20, LOSSTRACE20, file = "all_GSS20.RData")

rm(list = ls())


##################################################################################
####### 30 
###################################################################################


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

trace = read.csv("Trace_GSS30fin.csv")
trace = trace[,-1]

trace1 = read.csv("Trace_GSS30fin_copy.csv")
trace1 = trace1[,-1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparisons")

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

#TRACE
glrm1 = trace[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm2 = trace[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm3 = trace[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm4 = trace[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm5 = trace[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm6 = trace[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm7 = trace[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm8 = trace[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm9 = trace[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm10 = trace[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]
glrm11 = trace1[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm12 = trace1[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm13 = trace1[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm14 = trace1[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm15 = trace1[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm16 = trace1[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm17 = trace1[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm18 = trace1[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm19 = trace1[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm20 = trace1[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]



GLRM_TR1 = factorLevels(glrm1, df.base, cat1)
GLRM_TR2 = factorLevels(glrm2, df.base, cat1)
GLRM_TR3 = factorLevels(glrm3, df.base, cat1)
GLRM_TR4 = factorLevels(glrm4, df.base, cat1)
GLRM_TR5 = factorLevels(glrm5, df.base, cat1)
GLRM_TR6 = factorLevels(glrm6, df.base, cat1)
GLRM_TR7 = factorLevels(glrm7, df.base, cat1)
GLRM_TR8 = factorLevels(glrm8, df.base, cat1)
GLRM_TR9 = factorLevels(glrm9, df.base, cat1)
GLRM_TR10 = factorLevels(glrm10, df.base, cat1)
GLRM_TR11 = factorLevels(glrm11, df.base, cat1)
GLRM_TR12 = factorLevels(glrm12, df.base, cat1)
GLRM_TR13 = factorLevels(glrm13, df.base, cat1)
GLRM_TR14 = factorLevels(glrm14, df.base, cat1)
GLRM_TR15 = factorLevels(glrm15, df.base, cat1)
GLRM_TR16 = factorLevels(glrm16, df.base, cat1)
GLRM_TR17 = factorLevels(glrm17, df.base, cat1)
GLRM_TR18 = factorLevels(glrm18, df.base, cat1)
GLRM_TR19 = factorLevels(glrm19, df.base, cat1)
GLRM_TR20 = factorLevels(glrm20, df.base, cat1)


# LOSS CALCULATIONS
l.tr = rep(NA, 20)
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR1 = loss(GLRM_TR1,df.base,s,cat1,ord1, real1 )

l.tr[1]   = sum(LOSS.GLRM_TR1$lcol)
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR2 = loss(GLRM_TR2,df.base,s,cat1,ord1, real1 )

l.tr[2] = sum(LOSS.GLRM_TR2$lcol)
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR3 = loss(GLRM_TR3,df.base,s,cat1,ord1, real1 )

l.tr[3] = sum(LOSS.GLRM_TR3$lcol)
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR4 = loss(GLRM_TR4,df.base,s,cat1,ord1, real1 )

l.tr[4] = sum(LOSS.GLRM_TR4$lcol)
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR5 = loss(GLRM_TR5,df.base,s,cat1,ord1, real1 )

l.tr[5] = sum(LOSS.GLRM_TR5$lcol)
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR6 = loss(GLRM_TR6,df.base,s,cat1,ord1, real1 )

l.tr[6]   = sum(LOSS.GLRM_TR6$lcol)
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR7 = loss(GLRM_TR7,df.base,s,cat1,ord1, real1 )

l.tr[7] = sum(LOSS.GLRM_TR7$lcol)
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR8 = loss(GLRM_TR8,df.base,s,cat1,ord1, real1 )

l.tr[8] = sum(LOSS.GLRM_TR8$lcol)
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR9 = loss(GLRM_TR9,df.base,s,cat1,ord1, real1 )

l.tr[9] = sum(LOSS.GLRM_TR9$lcol)
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR10 = loss(GLRM_TR10,df.base,s,cat1,ord1, real1 )

l.tr[10] = sum(LOSS.GLRM_TR10$lcol)
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR11 = loss(GLRM_TR11,df.base,s,cat1,ord1, real1 )

l.tr[11]   = sum(LOSS.GLRM_TR11$lcol)
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR12 = loss(GLRM_TR12,df.base,s,cat1,ord1, real1 )

l.tr[12] = sum(LOSS.GLRM_TR12$lcol)
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR13 = loss(GLRM_TR13,df.base,s,cat1,ord1, real1 )

l.tr[13] = sum(LOSS.GLRM_TR13$lcol)
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR14 = loss(GLRM_TR14,df.base,s,cat1,ord1, real1 )

l.tr[14] = sum(LOSS.GLRM_TR14$lcol)
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR15 = loss(GLRM_TR15,df.base,s,cat1,ord1, real1 )

l.tr[15] = sum(LOSS.GLRM_TR15$lcol)
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR16 = loss(GLRM_TR16,df.base,s,cat1,ord1, real1 )

l.tr[16]   = sum(LOSS.GLRM_TR16$lcol)
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR17 = loss(GLRM_TR17,df.base,s,cat1,ord1, real1 )

l.tr[17] = sum(LOSS.GLRM_TR17$lcol)
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR18 = loss(GLRM_TR18,df.base,s,cat1,ord1, real1 )

l.tr[18] = sum(LOSS.GLRM_TR18$lcol)
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR19 = loss(GLRM_TR19,df.base,s,cat1,ord1, real1 )

l.tr[19] = sum(LOSS.GLRM_TR19$lcol)
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR20 = loss(GLRM_TR20,df.base,s,cat1,ord1, real1 )

l.tr[20] = sum(LOSS.GLRM_TR20$lcol)
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  



l.tr30 = l.tr
l.aml30 = l.aml
l.mice30 = l.mice

LAML30 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol, LOSS.AML6$lcol, LOSS.AML7$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol, LOSS.AML10$lcol, LOSS.AML12$lcol, LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol, LOSS.AML19$lcol, LOSS.AML20$lcol)

LMICE30 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LTRACE30 = cbind(LOSS.GLRM_TR1$lcol, LOSS.GLRM_TR2$lcol, LOSS.GLRM_TR3$lcol, LOSS.GLRM_TR4$lcol, LOSS.GLRM_TR5$lcol, LOSS.GLRM_TR6$lcol, LOSS.GLRM_TR7$lcol, LOSS.GLRM_TR8$lcol, LOSS.GLRM_TR9$lcol, LOSS.GLRM_TR10$lcol, LOSS.GLRM_TR11$lcol, LOSS.GLRM_TR12$lcol, LOSS.GLRM_TR13$lcol, LOSS.GLRM_TR14$lcol, LOSS.GLRM_TR15$lcol, LOSS.GLRM_TR16$lcol, LOSS.GLRM_TR17$lcol, LOSS.GLRM_TR18$lcol, LOSS.GLRM_TR19$lcol, LOSS.GLRM_TR20$lcol)


LOSSAML30 = apply(LAML30, 1, mean, na.rm = TRUE)
LOSSMICE30 = apply(LMICE30, 1, mean, na.rm = TRUE)
LOSSTRACE30 = apply(LTRACE30, 1, mean, na.rm = TRUE)

save(l.tr30, l.aml30, l.mice30, LAML30, LMICE30, LTRACE30, LOSSAML30, LOSSMICE30, LOSSTRACE30, file = "all_GSS30.RData")

rm(list = ls())


##################################################################################
####### 40 
###################################################################################


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

trace = read.csv("Trace_GSS40fin.csv")
trace = trace[,-1]

trace1 = read.csv("Trace_GSS40fin_copy.csv")
trace1 = trace1[,-1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparisons")

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

#TRACE
glrm1 = trace[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm2 = trace[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm3 = trace[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm4 = trace[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm5 = trace[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm6 = trace[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm7 = trace[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm8 = trace[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm9 = trace[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm10 = trace[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]
glrm11 = trace1[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm12 = trace1[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm13 = trace1[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm14 = trace1[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm15 = trace1[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm16 = trace1[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm17 = trace1[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm18 = trace1[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm19 = trace1[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm20 = trace1[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]



GLRM_TR1 = factorLevels(glrm1, df.base, cat1)
GLRM_TR2 = factorLevels(glrm2, df.base, cat1)
GLRM_TR3 = factorLevels(glrm3, df.base, cat1)
GLRM_TR4 = factorLevels(glrm4, df.base, cat1)
GLRM_TR5 = factorLevels(glrm5, df.base, cat1)
GLRM_TR6 = factorLevels(glrm6, df.base, cat1)
GLRM_TR7 = factorLevels(glrm7, df.base, cat1)
GLRM_TR8 = factorLevels(glrm8, df.base, cat1)
GLRM_TR9 = factorLevels(glrm9, df.base, cat1)
GLRM_TR10 = factorLevels(glrm10, df.base, cat1)
GLRM_TR11 = factorLevels(glrm11, df.base, cat1)
GLRM_TR12 = factorLevels(glrm12, df.base, cat1)
GLRM_TR13 = factorLevels(glrm13, df.base, cat1)
GLRM_TR14 = factorLevels(glrm14, df.base, cat1)
GLRM_TR15 = factorLevels(glrm15, df.base, cat1)
GLRM_TR16 = factorLevels(glrm16, df.base, cat1)
GLRM_TR17 = factorLevels(glrm17, df.base, cat1)
GLRM_TR18 = factorLevels(glrm18, df.base, cat1)
GLRM_TR19 = factorLevels(glrm19, df.base, cat1)
GLRM_TR20 = factorLevels(glrm20, df.base, cat1)


# LOSS CALCULATIONS
l.tr = rep(NA, 20)
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR1 = loss(GLRM_TR1,df.base,s,cat1,ord1, real1 )

l.tr[1]   = sum(LOSS.GLRM_TR1$lcol)
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR2 = loss(GLRM_TR2,df.base,s,cat1,ord1, real1 )

l.tr[2] = sum(LOSS.GLRM_TR2$lcol)
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR3 = loss(GLRM_TR3,df.base,s,cat1,ord1, real1 )

l.tr[3] = sum(LOSS.GLRM_TR3$lcol)
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR4 = loss(GLRM_TR4,df.base,s,cat1,ord1, real1 )

l.tr[4] = sum(LOSS.GLRM_TR4$lcol)
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR5 = loss(GLRM_TR5,df.base,s,cat1,ord1, real1 )

l.tr[5] = sum(LOSS.GLRM_TR5$lcol)
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR6 = loss(GLRM_TR6,df.base,s,cat1,ord1, real1 )

l.tr[6]   = sum(LOSS.GLRM_TR6$lcol)
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR7 = loss(GLRM_TR7,df.base,s,cat1,ord1, real1 )

l.tr[7] = sum(LOSS.GLRM_TR7$lcol)
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR8 = loss(GLRM_TR8,df.base,s,cat1,ord1, real1 )

l.tr[8] = sum(LOSS.GLRM_TR8$lcol)
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR9 = loss(GLRM_TR9,df.base,s,cat1,ord1, real1 )

l.tr[9] = sum(LOSS.GLRM_TR9$lcol)
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR10 = loss(GLRM_TR10,df.base,s,cat1,ord1, real1 )

l.tr[10] = sum(LOSS.GLRM_TR10$lcol)
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR11 = loss(GLRM_TR11,df.base,s,cat1,ord1, real1 )

l.tr[11]   = sum(LOSS.GLRM_TR11$lcol)
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR12 = loss(GLRM_TR12,df.base,s,cat1,ord1, real1 )

l.tr[12] = sum(LOSS.GLRM_TR12$lcol)
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR13 = loss(GLRM_TR13,df.base,s,cat1,ord1, real1 )

l.tr[13] = sum(LOSS.GLRM_TR13$lcol)
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR14 = loss(GLRM_TR14,df.base,s,cat1,ord1, real1 )

l.tr[14] = sum(LOSS.GLRM_TR14$lcol)
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR15 = loss(GLRM_TR15,df.base,s,cat1,ord1, real1 )

l.tr[15] = sum(LOSS.GLRM_TR15$lcol)
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR16 = loss(GLRM_TR16,df.base,s,cat1,ord1, real1 )

l.tr[16]   = sum(LOSS.GLRM_TR16$lcol)
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR17 = loss(GLRM_TR17,df.base,s,cat1,ord1, real1 )

l.tr[17] = sum(LOSS.GLRM_TR17$lcol)
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR18 = loss(GLRM_TR18,df.base,s,cat1,ord1, real1 )

l.tr[18] = sum(LOSS.GLRM_TR18$lcol)
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR19 = loss(GLRM_TR19,df.base,s,cat1,ord1, real1 )

l.tr[19] = sum(LOSS.GLRM_TR19$lcol)
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR20 = loss(GLRM_TR20,df.base,s,cat1,ord1, real1 )

l.tr[20] = sum(LOSS.GLRM_TR20$lcol)
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  



l.tr40 = l.tr
l.aml40 = l.aml
l.mice40 = l.mice

LAML40 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol, LOSS.AML5$lcol, LOSS.AML6$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol, LOSS.AML10$lcol, LOSS.AML11$lcol, LOSS.AML12$lcol, LOSS.AML13$lcol,  LOSS.AML15$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol,  LOSS.AML20$lcol)

LMICE40 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LTRACE40 = cbind(LOSS.GLRM_TR1$lcol, LOSS.GLRM_TR2$lcol, LOSS.GLRM_TR3$lcol, LOSS.GLRM_TR4$lcol, LOSS.GLRM_TR5$lcol, LOSS.GLRM_TR6$lcol, LOSS.GLRM_TR7$lcol, LOSS.GLRM_TR8$lcol, LOSS.GLRM_TR9$lcol, LOSS.GLRM_TR10$lcol, LOSS.GLRM_TR11$lcol, LOSS.GLRM_TR12$lcol, LOSS.GLRM_TR13$lcol, LOSS.GLRM_TR14$lcol, LOSS.GLRM_TR15$lcol, LOSS.GLRM_TR16$lcol, LOSS.GLRM_TR17$lcol, LOSS.GLRM_TR18$lcol, LOSS.GLRM_TR19$lcol, LOSS.GLRM_TR20$lcol)


LOSSAML40 = apply(LAML40, 1, mean, na.rm = TRUE)
LOSSMICE40 = apply(LMICE40, 1, mean, na.rm = TRUE)
LOSSTRACE40 = apply(LTRACE40, 1, mean, na.rm = TRUE)

save(l.tr40, l.aml40, l.mice40, LAML40, LMICE40, LTRACE40, LOSSAML40, LOSSMICE40, LOSSTRACE40, file = "all_GSS40.RData")

rm(list = ls())


##################################################################################
####### 50 
###################################################################################



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

trace = read.csv("Trace_GSS50fin.csv")
trace = trace[,-1]

trace1 = read.csv("Trace_GSS50fin_copy.csv")
trace1 = trace1[,-1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparisons")

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

#TRACE
glrm1 = trace[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm2 = trace[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm3 = trace[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm4 = trace[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm5 = trace[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm6 = trace[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm7 = trace[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm8 = trace[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm9 = trace[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm10 = trace[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]
glrm11 = trace1[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm12 = trace1[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm13 = trace1[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm14 = trace1[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm15 = trace1[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm16 = trace1[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm17 = trace1[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm18 = trace1[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm19 = trace1[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm20 = trace1[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]



GLRM_TR1 = factorLevels(glrm1, df.base, cat1)
GLRM_TR2 = factorLevels(glrm2, df.base, cat1)
GLRM_TR3 = factorLevels(glrm3, df.base, cat1)
GLRM_TR4 = factorLevels(glrm4, df.base, cat1)
GLRM_TR5 = factorLevels(glrm5, df.base, cat1)
GLRM_TR6 = factorLevels(glrm6, df.base, cat1)
GLRM_TR7 = factorLevels(glrm7, df.base, cat1)
GLRM_TR8 = factorLevels(glrm8, df.base, cat1)
GLRM_TR9 = factorLevels(glrm9, df.base, cat1)
GLRM_TR10 = factorLevels(glrm10, df.base, cat1)
GLRM_TR11 = factorLevels(glrm11, df.base, cat1)
GLRM_TR12 = factorLevels(glrm12, df.base, cat1)
GLRM_TR13 = factorLevels(glrm13, df.base, cat1)
GLRM_TR14 = factorLevels(glrm14, df.base, cat1)
GLRM_TR15 = factorLevels(glrm15, df.base, cat1)
GLRM_TR16 = factorLevels(glrm16, df.base, cat1)
GLRM_TR17 = factorLevels(glrm17, df.base, cat1)
GLRM_TR18 = factorLevels(glrm18, df.base, cat1)
GLRM_TR19 = factorLevels(glrm19, df.base, cat1)
GLRM_TR20 = factorLevels(glrm20, df.base, cat1)


# LOSS CALCULATIONS
l.tr = rep(NA, 20)
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR1 = loss(GLRM_TR1,df.base,s,cat1,ord1, real1 )

l.tr[1]   = sum(LOSS.GLRM_TR1$lcol)
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR2 = loss(GLRM_TR2,df.base,s,cat1,ord1, real1 )

l.tr[2] = sum(LOSS.GLRM_TR2$lcol)
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR3 = loss(GLRM_TR3,df.base,s,cat1,ord1, real1 )

l.tr[3] = sum(LOSS.GLRM_TR3$lcol)
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR4 = loss(GLRM_TR4,df.base,s,cat1,ord1, real1 )

l.tr[4] = sum(LOSS.GLRM_TR4$lcol)
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR5 = loss(GLRM_TR5,df.base,s,cat1,ord1, real1 )

l.tr[5] = sum(LOSS.GLRM_TR5$lcol)
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR6 = loss(GLRM_TR6,df.base,s,cat1,ord1, real1 )

l.tr[6]   = sum(LOSS.GLRM_TR6$lcol)
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR7 = loss(GLRM_TR7,df.base,s,cat1,ord1, real1 )

l.tr[7] = sum(LOSS.GLRM_TR7$lcol)
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR8 = loss(GLRM_TR8,df.base,s,cat1,ord1, real1 )

l.tr[8] = sum(LOSS.GLRM_TR8$lcol)
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR9 = loss(GLRM_TR9,df.base,s,cat1,ord1, real1 )

l.tr[9] = sum(LOSS.GLRM_TR9$lcol)
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR10 = loss(GLRM_TR10,df.base,s,cat1,ord1, real1 )

l.tr[10] = sum(LOSS.GLRM_TR10$lcol)
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR11 = loss(GLRM_TR11,df.base,s,cat1,ord1, real1 )

l.tr[11]   = sum(LOSS.GLRM_TR11$lcol)
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR12 = loss(GLRM_TR12,df.base,s,cat1,ord1, real1 )

l.tr[12] = sum(LOSS.GLRM_TR12$lcol)
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR13 = loss(GLRM_TR13,df.base,s,cat1,ord1, real1 )

l.tr[13] = sum(LOSS.GLRM_TR13$lcol)
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR14 = loss(GLRM_TR14,df.base,s,cat1,ord1, real1 )

l.tr[14] = sum(LOSS.GLRM_TR14$lcol)
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR15 = loss(GLRM_TR15,df.base,s,cat1,ord1, real1 )

l.tr[15] = sum(LOSS.GLRM_TR15$lcol)
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR16 = loss(GLRM_TR16,df.base,s,cat1,ord1, real1 )

l.tr[16]   = sum(LOSS.GLRM_TR16$lcol)
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR17 = loss(GLRM_TR17,df.base,s,cat1,ord1, real1 )

l.tr[17] = sum(LOSS.GLRM_TR17$lcol)
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR18 = loss(GLRM_TR18,df.base,s,cat1,ord1, real1 )

l.tr[18] = sum(LOSS.GLRM_TR18$lcol)
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR19 = loss(GLRM_TR19,df.base,s,cat1,ord1, real1 )

l.tr[19] = sum(LOSS.GLRM_TR19$lcol)
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR20 = loss(GLRM_TR20,df.base,s,cat1,ord1, real1 )

l.tr[20] = sum(LOSS.GLRM_TR20$lcol)
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  



l.tr50 = l.tr
l.aml50 = l.aml
l.mice50 = l.mice

LAML50 = cbind( LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol,  LOSS.AML7$lcol, LOSS.AML9$lcol, LOSS.AML11$lcol, LOSS.AML14$lcol,  LOSS.AML18$lcol, LOSS.AML20$lcol)

LMICE50 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LTRACE50 = cbind(LOSS.GLRM_TR1$lcol, LOSS.GLRM_TR2$lcol, LOSS.GLRM_TR3$lcol, LOSS.GLRM_TR4$lcol, LOSS.GLRM_TR5$lcol, LOSS.GLRM_TR6$lcol, LOSS.GLRM_TR7$lcol, LOSS.GLRM_TR8$lcol, LOSS.GLRM_TR9$lcol, LOSS.GLRM_TR10$lcol, LOSS.GLRM_TR11$lcol, LOSS.GLRM_TR12$lcol, LOSS.GLRM_TR13$lcol, LOSS.GLRM_TR14$lcol, LOSS.GLRM_TR15$lcol, LOSS.GLRM_TR16$lcol, LOSS.GLRM_TR17$lcol, LOSS.GLRM_TR18$lcol, LOSS.GLRM_TR19$lcol, LOSS.GLRM_TR20$lcol)


LOSSAML50 = apply(LAML50, 1, mean, na.rm = TRUE)
LOSSMICE50 = apply(LMICE50, 1, mean, na.rm = TRUE)
LOSSTRACE50 = apply(LTRACE50, 1, mean, na.rm = TRUE)

save(l.tr50, l.aml50, l.mice50, LAML50, LMICE50, LTRACE50, LOSSAML50, LOSSMICE50, LOSSTRACE50, file = "all_GSS50.RData")

rm(list = ls())


##################################################################################
####### 60 
###################################################################################




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

trace = read.csv("Trace_GSS60fin.csv")
trace = trace[,-1]

trace1 = read.csv("Trace_GSS60fin_copy.csv")
trace1 = trace1[,-1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparisons")

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

#TRACE
glrm1 = trace[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm2 = trace[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm3 = trace[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm4 = trace[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm5 = trace[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm6 = trace[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm7 = trace[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm8 = trace[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm9 = trace[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm10 = trace[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]
glrm11 = trace1[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm12 = trace1[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm13 = trace1[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm14 = trace1[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm15 = trace1[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm16 = trace1[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm17 = trace1[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm18 = trace1[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm19 = trace1[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm20 = trace1[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]



GLRM_TR1 = factorLevels(glrm1, df.base, cat1)
GLRM_TR2 = factorLevels(glrm2, df.base, cat1)
GLRM_TR3 = factorLevels(glrm3, df.base, cat1)
GLRM_TR4 = factorLevels(glrm4, df.base, cat1)
GLRM_TR5 = factorLevels(glrm5, df.base, cat1)
GLRM_TR6 = factorLevels(glrm6, df.base, cat1)
GLRM_TR7 = factorLevels(glrm7, df.base, cat1)
GLRM_TR8 = factorLevels(glrm8, df.base, cat1)
GLRM_TR9 = factorLevels(glrm9, df.base, cat1)
GLRM_TR10 = factorLevels(glrm10, df.base, cat1)
GLRM_TR11 = factorLevels(glrm11, df.base, cat1)
GLRM_TR12 = factorLevels(glrm12, df.base, cat1)
GLRM_TR13 = factorLevels(glrm13, df.base, cat1)
GLRM_TR14 = factorLevels(glrm14, df.base, cat1)
GLRM_TR15 = factorLevels(glrm15, df.base, cat1)
GLRM_TR16 = factorLevels(glrm16, df.base, cat1)
GLRM_TR17 = factorLevels(glrm17, df.base, cat1)
GLRM_TR18 = factorLevels(glrm18, df.base, cat1)
GLRM_TR19 = factorLevels(glrm19, df.base, cat1)
GLRM_TR20 = factorLevels(glrm20, df.base, cat1)


# LOSS CALCULATIONS
l.tr = rep(NA, 20)
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR1 = loss(GLRM_TR1,df.base,s,cat1,ord1, real1 )

l.tr[1]   = sum(LOSS.GLRM_TR1$lcol)
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR2 = loss(GLRM_TR2,df.base,s,cat1,ord1, real1 )

l.tr[2] = sum(LOSS.GLRM_TR2$lcol)
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR3 = loss(GLRM_TR3,df.base,s,cat1,ord1, real1 )

l.tr[3] = sum(LOSS.GLRM_TR3$lcol)
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR4 = loss(GLRM_TR4,df.base,s,cat1,ord1, real1 )

l.tr[4] = sum(LOSS.GLRM_TR4$lcol)
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR5 = loss(GLRM_TR5,df.base,s,cat1,ord1, real1 )

l.tr[5] = sum(LOSS.GLRM_TR5$lcol)
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR6 = loss(GLRM_TR6,df.base,s,cat1,ord1, real1 )

l.tr[6]   = sum(LOSS.GLRM_TR6$lcol)
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR7 = loss(GLRM_TR7,df.base,s,cat1,ord1, real1 )

l.tr[7] = sum(LOSS.GLRM_TR7$lcol)
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR8 = loss(GLRM_TR8,df.base,s,cat1,ord1, real1 )

l.tr[8] = sum(LOSS.GLRM_TR8$lcol)
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR9 = loss(GLRM_TR9,df.base,s,cat1,ord1, real1 )

l.tr[9] = sum(LOSS.GLRM_TR9$lcol)
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR10 = loss(GLRM_TR10,df.base,s,cat1,ord1, real1 )

l.tr[10] = sum(LOSS.GLRM_TR10$lcol)
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR11 = loss(GLRM_TR11,df.base,s,cat1,ord1, real1 )

l.tr[11]   = sum(LOSS.GLRM_TR11$lcol)
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR12 = loss(GLRM_TR12,df.base,s,cat1,ord1, real1 )

l.tr[12] = sum(LOSS.GLRM_TR12$lcol)
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR13 = loss(GLRM_TR13,df.base,s,cat1,ord1, real1 )

l.tr[13] = sum(LOSS.GLRM_TR13$lcol)
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR14 = loss(GLRM_TR14,df.base,s,cat1,ord1, real1 )

l.tr[14] = sum(LOSS.GLRM_TR14$lcol)
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR15 = loss(GLRM_TR15,df.base,s,cat1,ord1, real1 )

l.tr[15] = sum(LOSS.GLRM_TR15$lcol)
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR16 = loss(GLRM_TR16,df.base,s,cat1,ord1, real1 )

l.tr[16]   = sum(LOSS.GLRM_TR16$lcol)
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR17 = loss(GLRM_TR17,df.base,s,cat1,ord1, real1 )

l.tr[17] = sum(LOSS.GLRM_TR17$lcol)
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR18 = loss(GLRM_TR18,df.base,s,cat1,ord1, real1 )

l.tr[18] = sum(LOSS.GLRM_TR18$lcol)
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR19 = loss(GLRM_TR19,df.base,s,cat1,ord1, real1 )

l.tr[19] = sum(LOSS.GLRM_TR19$lcol)
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR20 = loss(GLRM_TR20,df.base,s,cat1,ord1, real1 )

l.tr[20] = sum(LOSS.GLRM_TR20$lcol)
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  

l.tr60 = l.tr
l.aml60 = l.aml
l.mice60 = l.mice

LAML60 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol,  LOSS.AML4$lcol, LOSS.AML5$lcol,  LOSS.AML7$lcol,  LOSS.AML9$lcol, LOSS.AML10$lcol,  LOSS.AML12$lcol, LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML15$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol, LOSS.AML20$lcol)

LMICE60 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LTRACE60 = cbind(LOSS.GLRM_TR1$lcol, LOSS.GLRM_TR2$lcol, LOSS.GLRM_TR3$lcol, LOSS.GLRM_TR4$lcol, LOSS.GLRM_TR5$lcol, LOSS.GLRM_TR6$lcol, LOSS.GLRM_TR7$lcol, LOSS.GLRM_TR8$lcol, LOSS.GLRM_TR9$lcol, LOSS.GLRM_TR10$lcol, LOSS.GLRM_TR11$lcol, LOSS.GLRM_TR12$lcol, LOSS.GLRM_TR13$lcol, LOSS.GLRM_TR14$lcol, LOSS.GLRM_TR15$lcol, LOSS.GLRM_TR16$lcol, LOSS.GLRM_TR17$lcol, LOSS.GLRM_TR18$lcol, LOSS.GLRM_TR19$lcol, LOSS.GLRM_TR20$lcol)


LOSSAML60 = apply(LAML60, 1, mean, na.rm = TRUE)
LOSSMICE60 = apply(LMICE60, 1, mean, na.rm = TRUE)
LOSSTRACE60 = apply(LTRACE60, 1, mean, na.rm = TRUE)

save(l.tr60, l.aml60, l.mice60, LAML60, LMICE60, LTRACE60, LOSSAML60, LOSSMICE60, LOSSTRACE60, file = "all_GSS60.RData")

rm(list = ls())


##################################################################################
####### 70 
##################################################################################



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

trace = read.csv("Trace_GSS70fin.csv")
trace = trace[,-1]

trace1 = read.csv("Trace_GSS70fin_copy.csv")
trace1 = trace1[,-1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparisons")

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

#TRACE
glrm1 = trace[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm2 = trace[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm3 = trace[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm4 = trace[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm5 = trace[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm6 = trace[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm7 = trace[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm8 = trace[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm9 = trace[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm10 = trace[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]
glrm11 = trace1[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm12 = trace1[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm13 = trace1[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm14 = trace1[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm15 = trace1[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm16 = trace1[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm17 = trace1[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm18 = trace1[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm19 = trace1[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm20 = trace1[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]



GLRM_TR1 = factorLevels(glrm1, df.base, cat1)
GLRM_TR2 = factorLevels(glrm2, df.base, cat1)
GLRM_TR3 = factorLevels(glrm3, df.base, cat1)
GLRM_TR4 = factorLevels(glrm4, df.base, cat1)
GLRM_TR5 = factorLevels(glrm5, df.base, cat1)
GLRM_TR6 = factorLevels(glrm6, df.base, cat1)
GLRM_TR7 = factorLevels(glrm7, df.base, cat1)
GLRM_TR8 = factorLevels(glrm8, df.base, cat1)
GLRM_TR9 = factorLevels(glrm9, df.base, cat1)
GLRM_TR10 = factorLevels(glrm10, df.base, cat1)
GLRM_TR11 = factorLevels(glrm11, df.base, cat1)
GLRM_TR12 = factorLevels(glrm12, df.base, cat1)
GLRM_TR13 = factorLevels(glrm13, df.base, cat1)
GLRM_TR14 = factorLevels(glrm14, df.base, cat1)
GLRM_TR15 = factorLevels(glrm15, df.base, cat1)
GLRM_TR16 = factorLevels(glrm16, df.base, cat1)
GLRM_TR17 = factorLevels(glrm17, df.base, cat1)
GLRM_TR18 = factorLevels(glrm18, df.base, cat1)
GLRM_TR19 = factorLevels(glrm19, df.base, cat1)
GLRM_TR20 = factorLevels(glrm20, df.base, cat1)


# LOSS CALCULATIONS
l.tr = rep(NA, 20)
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR1 = loss(GLRM_TR1,df.base,s,cat1,ord1, real1 )

l.tr[1]   = sum(LOSS.GLRM_TR1$lcol)
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR2 = loss(GLRM_TR2,df.base,s,cat1,ord1, real1 )

l.tr[2] = sum(LOSS.GLRM_TR2$lcol)
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR3 = loss(GLRM_TR3,df.base,s,cat1,ord1, real1 )

l.tr[3] = sum(LOSS.GLRM_TR3$lcol)
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR4 = loss(GLRM_TR4,df.base,s,cat1,ord1, real1 )

l.tr[4] = sum(LOSS.GLRM_TR4$lcol)
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR5 = loss(GLRM_TR5,df.base,s,cat1,ord1, real1 )

l.tr[5] = sum(LOSS.GLRM_TR5$lcol)
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR6 = loss(GLRM_TR6,df.base,s,cat1,ord1, real1 )

l.tr[6]   = sum(LOSS.GLRM_TR6$lcol)
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR7 = loss(GLRM_TR7,df.base,s,cat1,ord1, real1 )

l.tr[7] = sum(LOSS.GLRM_TR7$lcol)
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR8 = loss(GLRM_TR8,df.base,s,cat1,ord1, real1 )

l.tr[8] = sum(LOSS.GLRM_TR8$lcol)
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR9 = loss(GLRM_TR9,df.base,s,cat1,ord1, real1 )

l.tr[9] = sum(LOSS.GLRM_TR9$lcol)
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR10 = loss(GLRM_TR10,df.base,s,cat1,ord1, real1 )

l.tr[10] = sum(LOSS.GLRM_TR10$lcol)
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR11 = loss(GLRM_TR11,df.base,s,cat1,ord1, real1 )

l.tr[11]   = sum(LOSS.GLRM_TR11$lcol)
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR12 = loss(GLRM_TR12,df.base,s,cat1,ord1, real1 )

l.tr[12] = sum(LOSS.GLRM_TR12$lcol)
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR13 = loss(GLRM_TR13,df.base,s,cat1,ord1, real1 )

l.tr[13] = sum(LOSS.GLRM_TR13$lcol)
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR14 = loss(GLRM_TR14,df.base,s,cat1,ord1, real1 )

l.tr[14] = sum(LOSS.GLRM_TR14$lcol)
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR15 = loss(GLRM_TR15,df.base,s,cat1,ord1, real1 )

l.tr[15] = sum(LOSS.GLRM_TR15$lcol)
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR16 = loss(GLRM_TR16,df.base,s,cat1,ord1, real1 )

l.tr[16]   = sum(LOSS.GLRM_TR16$lcol)
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR17 = loss(GLRM_TR17,df.base,s,cat1,ord1, real1 )

l.tr[17] = sum(LOSS.GLRM_TR17$lcol)
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR18 = loss(GLRM_TR18,df.base,s,cat1,ord1, real1 )

l.tr[18] = sum(LOSS.GLRM_TR18$lcol)
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR19 = loss(GLRM_TR19,df.base,s,cat1,ord1, real1 )

l.tr[19] = sum(LOSS.GLRM_TR19$lcol)
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR20 = loss(GLRM_TR20,df.base,s,cat1,ord1, real1 )

l.tr[20] = sum(LOSS.GLRM_TR20$lcol)
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  



l.tr70 = l.tr
l.aml70 = l.aml
l.mice70 = l.mice

LAML70 = cbind(LOSS.AML1$lcol, LOSS.AML4$lcol,  LOSS.AML6$lcol, LOSS.AML7$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol,  LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML15$lcol, LOSS.AML16$lcol)

LMICE70 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LTRACE70 = cbind(LOSS.GLRM_TR1$lcol, LOSS.GLRM_TR2$lcol, LOSS.GLRM_TR3$lcol, LOSS.GLRM_TR4$lcol, LOSS.GLRM_TR5$lcol, LOSS.GLRM_TR6$lcol, LOSS.GLRM_TR7$lcol, LOSS.GLRM_TR8$lcol, LOSS.GLRM_TR9$lcol, LOSS.GLRM_TR10$lcol, LOSS.GLRM_TR11$lcol, LOSS.GLRM_TR12$lcol, LOSS.GLRM_TR13$lcol, LOSS.GLRM_TR14$lcol, LOSS.GLRM_TR15$lcol, LOSS.GLRM_TR16$lcol, LOSS.GLRM_TR17$lcol, LOSS.GLRM_TR18$lcol, LOSS.GLRM_TR19$lcol, LOSS.GLRM_TR20$lcol)


LOSSAML70 = apply(LAML70, 1, mean, na.rm = TRUE)
LOSSMICE70 = apply(LMICE70, 1, mean, na.rm = TRUE)
LOSSTRACE70 = apply(LTRACE70, 1, mean, na.rm = TRUE)

save(l.tr70, l.aml70, l.mice70, LAML70, LMICE70, LTRACE70, LOSSAML70, LOSSMICE70, LOSSTRACE70, file = "all_GSS70.RData")

rm(list = ls())


##################################################################################
####### 80 
###################################################################################


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

trace = read.csv("Trace_GSS80fin.csv")
trace = trace[,-1]

trace1 = read.csv("Trace_GSS80fin_copy.csv")
trace1 = trace1[,-1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparisons")

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

#TRACE
glrm1 = trace[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm2 = trace[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm3 = trace[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm4 = trace[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm5 = trace[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm6 = trace[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm7 = trace[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm8 = trace[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm9 = trace[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm10 = trace[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]
glrm11 = trace1[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm12 = trace1[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm13 = trace1[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm14 = trace1[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm15 = trace1[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm16 = trace1[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm17 = trace1[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm18 = trace1[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm19 = trace1[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm20 = trace1[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]



GLRM_TR1 = factorLevels(glrm1, df.base, cat1)
GLRM_TR2 = factorLevels(glrm2, df.base, cat1)
GLRM_TR3 = factorLevels(glrm3, df.base, cat1)
GLRM_TR4 = factorLevels(glrm4, df.base, cat1)
GLRM_TR5 = factorLevels(glrm5, df.base, cat1)
GLRM_TR6 = factorLevels(glrm6, df.base, cat1)
GLRM_TR7 = factorLevels(glrm7, df.base, cat1)
GLRM_TR8 = factorLevels(glrm8, df.base, cat1)
GLRM_TR9 = factorLevels(glrm9, df.base, cat1)
GLRM_TR10 = factorLevels(glrm10, df.base, cat1)
GLRM_TR11 = factorLevels(glrm11, df.base, cat1)
GLRM_TR12 = factorLevels(glrm12, df.base, cat1)
GLRM_TR13 = factorLevels(glrm13, df.base, cat1)
GLRM_TR14 = factorLevels(glrm14, df.base, cat1)
GLRM_TR15 = factorLevels(glrm15, df.base, cat1)
GLRM_TR16 = factorLevels(glrm16, df.base, cat1)
GLRM_TR17 = factorLevels(glrm17, df.base, cat1)
GLRM_TR18 = factorLevels(glrm18, df.base, cat1)
GLRM_TR19 = factorLevels(glrm19, df.base, cat1)
GLRM_TR20 = factorLevels(glrm20, df.base, cat1)


# LOSS CALCULATIONS
l.tr = rep(NA, 20)
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR1 = loss(GLRM_TR1,df.base,s,cat1,ord1, real1 )

l.tr[1]   = sum(LOSS.GLRM_TR1$lcol)
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR2 = loss(GLRM_TR2,df.base,s,cat1,ord1, real1 )

l.tr[2] = sum(LOSS.GLRM_TR2$lcol)
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR3 = loss(GLRM_TR3,df.base,s,cat1,ord1, real1 )

l.tr[3] = sum(LOSS.GLRM_TR3$lcol)
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR4 = loss(GLRM_TR4,df.base,s,cat1,ord1, real1 )

l.tr[4] = sum(LOSS.GLRM_TR4$lcol)
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR5 = loss(GLRM_TR5,df.base,s,cat1,ord1, real1 )

l.tr[5] = sum(LOSS.GLRM_TR5$lcol)
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR6 = loss(GLRM_TR6,df.base,s,cat1,ord1, real1 )

l.tr[6]   = sum(LOSS.GLRM_TR6$lcol)
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR7 = loss(GLRM_TR7,df.base,s,cat1,ord1, real1 )

l.tr[7] = sum(LOSS.GLRM_TR7$lcol)
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR8 = loss(GLRM_TR8,df.base,s,cat1,ord1, real1 )

l.tr[8] = sum(LOSS.GLRM_TR8$lcol)
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR9 = loss(GLRM_TR9,df.base,s,cat1,ord1, real1 )

l.tr[9] = sum(LOSS.GLRM_TR9$lcol)
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR10 = loss(GLRM_TR10,df.base,s,cat1,ord1, real1 )

l.tr[10] = sum(LOSS.GLRM_TR10$lcol)
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR11 = loss(GLRM_TR11,df.base,s,cat1,ord1, real1 )

l.tr[11]   = sum(LOSS.GLRM_TR11$lcol)
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR12 = loss(GLRM_TR12,df.base,s,cat1,ord1, real1 )

l.tr[12] = sum(LOSS.GLRM_TR12$lcol)
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR13 = loss(GLRM_TR13,df.base,s,cat1,ord1, real1 )

l.tr[13] = sum(LOSS.GLRM_TR13$lcol)
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR14 = loss(GLRM_TR14,df.base,s,cat1,ord1, real1 )

l.tr[14] = sum(LOSS.GLRM_TR14$lcol)
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR15 = loss(GLRM_TR15,df.base,s,cat1,ord1, real1 )

l.tr[15] = sum(LOSS.GLRM_TR15$lcol)
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR16 = loss(GLRM_TR16,df.base,s,cat1,ord1, real1 )

l.tr[16]   = sum(LOSS.GLRM_TR16$lcol)
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR17 = loss(GLRM_TR17,df.base,s,cat1,ord1, real1 )

l.tr[17] = sum(LOSS.GLRM_TR17$lcol)
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR18 = loss(GLRM_TR18,df.base,s,cat1,ord1, real1 )

l.tr[18] = sum(LOSS.GLRM_TR18$lcol)
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR19 = loss(GLRM_TR19,df.base,s,cat1,ord1, real1 )

l.tr[19] = sum(LOSS.GLRM_TR19$lcol)
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR20 = loss(GLRM_TR20,df.base,s,cat1,ord1, real1 )

l.tr[20] = sum(LOSS.GLRM_TR20$lcol)
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  



l.tr80 = l.tr
l.aml80 = l.aml
l.mice80 = l.mice

LAML80 = cbind(LOSS.AML1$lcol, LOSS.AML10$lcol, LOSS.AML12$lcol, LOSS.AML18$lcol, LOSS.AML19$lcol, LOSS.AML20$lcol)

#LMICE80 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LTRACE80 = cbind(LOSS.GLRM_TR1$lcol, LOSS.GLRM_TR2$lcol, LOSS.GLRM_TR3$lcol, LOSS.GLRM_TR4$lcol, LOSS.GLRM_TR5$lcol, LOSS.GLRM_TR6$lcol, LOSS.GLRM_TR7$lcol, LOSS.GLRM_TR8$lcol, LOSS.GLRM_TR9$lcol, LOSS.GLRM_TR10$lcol, LOSS.GLRM_TR11$lcol, LOSS.GLRM_TR12$lcol, LOSS.GLRM_TR13$lcol, LOSS.GLRM_TR14$lcol, LOSS.GLRM_TR15$lcol, LOSS.GLRM_TR16$lcol, LOSS.GLRM_TR17$lcol, LOSS.GLRM_TR18$lcol, LOSS.GLRM_TR19$lcol, LOSS.GLRM_TR20$lcol)


LOSSAML80 = apply(LAML80, 1, mean, na.rm = TRUE)
#LOSSMICE80 = apply(LMICE80, 1, mean, na.rm = TRUE)
LOSSTRACE80 = apply(LTRACE80, 1, mean, na.rm = TRUE)

save(l.tr80, l.aml80, l.mice80,  LTRACE80,  LAML80,  LOSSTRACE80, LOSSAML80, file = "all_GSS80.RData")

rm(list = ls())



##################################################################################
####### 90 
###################################################################################



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

trace = read.csv("Trace_GSS90fin.csv")
trace = trace[,-1]

trace1 = read.csv("Trace_GSS90fin_copy.csv")
trace1 = trace1[,-1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparisons")

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

#TRACE
glrm1 = trace[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm2 = trace[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm3 = trace[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm4 = trace[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm5 = trace[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm6 = trace[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm7 = trace[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm8 = trace[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm9 = trace[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm10 = trace[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]
glrm11 = trace1[, (0*dim(df.base)[2]+1):(1*dim(df.base)[2])]
glrm12 = trace1[, (1*dim(df.base)[2]+1):(2*dim(df.base)[2])]
glrm13 = trace1[, (2*dim(df.base)[2]+1):(3*dim(df.base)[2])]
glrm14 = trace1[, (3*dim(df.base)[2]+1):(4*dim(df.base)[2])]
glrm15 = trace1[, (4*dim(df.base)[2]+1):(5*dim(df.base)[2])]
glrm16 = trace1[, (5*dim(df.base)[2]+1):(6*dim(df.base)[2])]
glrm17 = trace1[, (6*dim(df.base)[2]+1):(7*dim(df.base)[2])]
glrm18 = trace1[, (7*dim(df.base)[2]+1):(8*dim(df.base)[2])]
glrm19 = trace1[, (8*dim(df.base)[2]+1):(9*dim(df.base)[2])]
glrm20 = trace1[, (9*dim(df.base)[2]+1):(10*dim(df.base)[2])]



GLRM_TR1 = factorLevels(glrm1, df.base, cat1)
GLRM_TR2 = factorLevels(glrm2, df.base, cat1)
GLRM_TR3 = factorLevels(glrm3, df.base, cat1)
GLRM_TR4 = factorLevels(glrm4, df.base, cat1)
GLRM_TR5 = factorLevels(glrm5, df.base, cat1)
GLRM_TR6 = factorLevels(glrm6, df.base, cat1)
GLRM_TR7 = factorLevels(glrm7, df.base, cat1)
GLRM_TR8 = factorLevels(glrm8, df.base, cat1)
GLRM_TR9 = factorLevels(glrm9, df.base, cat1)
GLRM_TR10 = factorLevels(glrm10, df.base, cat1)
GLRM_TR11 = factorLevels(glrm11, df.base, cat1)
GLRM_TR12 = factorLevels(glrm12, df.base, cat1)
GLRM_TR13 = factorLevels(glrm13, df.base, cat1)
GLRM_TR14 = factorLevels(glrm14, df.base, cat1)
GLRM_TR15 = factorLevels(glrm15, df.base, cat1)
GLRM_TR16 = factorLevels(glrm16, df.base, cat1)
GLRM_TR17 = factorLevels(glrm17, df.base, cat1)
GLRM_TR18 = factorLevels(glrm18, df.base, cat1)
GLRM_TR19 = factorLevels(glrm19, df.base, cat1)
GLRM_TR20 = factorLevels(glrm20, df.base, cat1)


# LOSS CALCULATIONS
l.tr = rep(NA, 20)
l.aml = rep(NA, 20)
l.mice = rep(NA, 20)

#### 1 
LOSS.MICE1 = loss(MICE1,df.base,s,cat1,ord1 , real1 )
LOSS.AML1 = loss(AML1,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR1 = loss(GLRM_TR1,df.base,s,cat1,ord1, real1 )

l.tr[1]   = sum(LOSS.GLRM_TR1$lcol)
l.aml[1]  = sum(LOSS.AML1$lcol)
l.mice[1] = sum(LOSS.MICE1$lcol)

#### 2 
LOSS.MICE2 = loss(MICE2,df.base,s,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR2 = loss(GLRM_TR2,df.base,s,cat1,ord1, real1 )

l.tr[2] = sum(LOSS.GLRM_TR2$lcol)
l.aml[2]= sum(LOSS.AML2$lcol)
l.mice[2] = sum(LOSS.MICE2$lcol)  

#### 3 
LOSS.MICE3 = loss(MICE3,df.base,s,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR3 = loss(GLRM_TR3,df.base,s,cat1,ord1, real1 )

l.tr[3] = sum(LOSS.GLRM_TR3$lcol)
l.aml[3] = sum(LOSS.AML3$lcol)
l.mice[3] = sum(LOSS.MICE3$lcol)  

#### 4 
LOSS.MICE4 = loss(MICE4,df.base,s,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR4 = loss(GLRM_TR4,df.base,s,cat1,ord1, real1 )

l.tr[4] = sum(LOSS.GLRM_TR4$lcol)
l.aml[4]= sum(LOSS.AML4$lcol)
l.mice[4] = sum(LOSS.MICE4$lcol)  

#### 5 
LOSS.MICE5 = loss(MICE5,df.base,s,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR5 = loss(GLRM_TR5,df.base,s,cat1,ord1, real1 )

l.tr[5] = sum(LOSS.GLRM_TR5$lcol)
l.aml[5] = sum(LOSS.AML5$lcol)
l.mice[5] = sum(LOSS.MICE5$lcol)  

#### 6 
LOSS.MICE6 = loss(MICE6,df.base,s,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR6 = loss(GLRM_TR6,df.base,s,cat1,ord1, real1 )

l.tr[6]   = sum(LOSS.GLRM_TR6$lcol)
l.aml[6]  = sum(LOSS.AML6$lcol)
l.mice[6] = sum(LOSS.MICE6$lcol)

#### 7 
LOSS.MICE7 = loss(MICE7,df.base,s,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR7 = loss(GLRM_TR7,df.base,s,cat1,ord1, real1 )

l.tr[7] = sum(LOSS.GLRM_TR7$lcol)
l.aml[7]= sum(LOSS.AML7$lcol)
l.mice[7] = sum(LOSS.MICE7$lcol)  

#### 8 
LOSS.MICE8 = loss(MICE8,df.base,s,cat1,ord1, real1 )
LOSS.AML8 = loss(AML8,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR8 = loss(GLRM_TR8,df.base,s,cat1,ord1, real1 )

l.tr[8] = sum(LOSS.GLRM_TR8$lcol)
l.aml[8] = sum(LOSS.AML8$lcol)
l.mice[8] = sum(LOSS.MICE8$lcol)  

#### 9
LOSS.MICE9 = loss(MICE9,df.base,s,cat1,ord1, real1 )
LOSS.AML9 = loss(AML9,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR9 = loss(GLRM_TR9,df.base,s,cat1,ord1, real1 )

l.tr[9] = sum(LOSS.GLRM_TR9$lcol)
l.aml[9]= sum(LOSS.AML9$lcol)
l.mice[9] = sum(LOSS.MICE9$lcol)  

#### 10
LOSS.MICE10 = loss(MICE10,df.base,s,cat1,ord1, real1 )
LOSS.AML10 = loss(AML10,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR10 = loss(GLRM_TR10,df.base,s,cat1,ord1, real1 )

l.tr[10] = sum(LOSS.GLRM_TR10$lcol)
l.aml[10] = sum(LOSS.AML10$lcol)
l.mice[10] = sum(LOSS.MICE10$lcol)  


#### 11 
LOSS.MICE11 = loss(MICE11,df.base,s,cat1,ord1 , real1 )
LOSS.AML11 = loss(AML11,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR11 = loss(GLRM_TR11,df.base,s,cat1,ord1, real1 )

l.tr[11]   = sum(LOSS.GLRM_TR11$lcol)
l.aml[11]  = sum(LOSS.AML11$lcol)
l.mice[11] = sum(LOSS.MICE11$lcol)

#### 12 
LOSS.MICE12 = loss(MICE12,df.base,s,cat1,ord1, real1 )
LOSS.AML12 = loss(AML12,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR12 = loss(GLRM_TR12,df.base,s,cat1,ord1, real1 )

l.tr[12] = sum(LOSS.GLRM_TR12$lcol)
l.aml[12]= sum(LOSS.AML12$lcol)
l.mice[12] = sum(LOSS.MICE12$lcol)  

#### 13 
LOSS.MICE13 = loss(MICE13,df.base,s,cat1,ord1, real1 )
LOSS.AML13 = loss(AML13,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR13 = loss(GLRM_TR13,df.base,s,cat1,ord1, real1 )

l.tr[13] = sum(LOSS.GLRM_TR13$lcol)
l.aml[13] = sum(LOSS.AML13$lcol)
l.mice[13] = sum(LOSS.MICE13$lcol)  

#### 14 
LOSS.MICE14 = loss(MICE14,df.base,s,cat1,ord1, real1 )
LOSS.AML14 = loss(AML14,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR14 = loss(GLRM_TR14,df.base,s,cat1,ord1, real1 )

l.tr[14] = sum(LOSS.GLRM_TR14$lcol)
l.aml[14]= sum(LOSS.AML14$lcol)
l.mice[14] = sum(LOSS.MICE14$lcol)  

#### 15 
LOSS.MICE15 = loss(MICE15,df.base,s,cat1,ord1, real1 )
LOSS.AML15 = loss(AML15,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR15 = loss(GLRM_TR15,df.base,s,cat1,ord1, real1 )

l.tr[15] = sum(LOSS.GLRM_TR15$lcol)
l.aml[15] = sum(LOSS.AML15$lcol)
l.mice[15] = sum(LOSS.MICE15$lcol)  

#### 16 
LOSS.MICE16 = loss(MICE16,df.base,s,cat1,ord1, real1 )
LOSS.AML16 = loss(AML16,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR16 = loss(GLRM_TR16,df.base,s,cat1,ord1, real1 )

l.tr[16]   = sum(LOSS.GLRM_TR16$lcol)
l.aml[16]  = sum(LOSS.AML16$lcol)
l.mice[16] = sum(LOSS.MICE16$lcol)

#### 17 
LOSS.MICE17 = loss(MICE17,df.base,s,cat1,ord1, real1 )
LOSS.AML17 = loss(AML17,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR17 = loss(GLRM_TR17,df.base,s,cat1,ord1, real1 )

l.tr[17] = sum(LOSS.GLRM_TR17$lcol)
l.aml[17]= sum(LOSS.AML17$lcol)
l.mice[17] = sum(LOSS.MICE17$lcol)  

#### 18 
LOSS.MICE18 = loss(MICE18,df.base,s,cat1,ord1, real1 )
LOSS.AML18 = loss(AML18,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR18 = loss(GLRM_TR18,df.base,s,cat1,ord1, real1 )

l.tr[18] = sum(LOSS.GLRM_TR18$lcol)
l.aml[18] = sum(LOSS.AML18$lcol)
l.mice[18] = sum(LOSS.MICE18$lcol)  

#### 19
LOSS.MICE19 = loss(MICE19,df.base,s,cat1,ord1, real1 )
LOSS.AML19 = loss(AML19,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR19 = loss(GLRM_TR19,df.base,s,cat1,ord1, real1 )

l.tr[19] = sum(LOSS.GLRM_TR19$lcol)
l.aml[19]= sum(LOSS.AML19$lcol)
l.mice[19] = sum(LOSS.MICE19$lcol)  

#### 20
LOSS.MICE20 = loss(MICE20,df.base,s,cat1,ord1, real1 )
LOSS.AML20 = loss(AML20,df.base,s,cat1,ord1, real1 )
LOSS.GLRM_TR20 = loss(GLRM_TR20,df.base,s,cat1,ord1, real1 )

l.tr[20] = sum(LOSS.GLRM_TR20$lcol)
l.aml[20] = sum(LOSS.AML20$lcol)
l.mice[20] = sum(LOSS.MICE20$lcol)  



l.tr90 = l.tr
l.aml90 = l.aml
l.mice90 = l.mice

LAML90 = cbind(LOSS.AML1$lcol, LOSS.AML2$lcol, LOSS.AML3$lcol, LOSS.AML4$lcol, LOSS.AML5$lcol, LOSS.AML6$lcol,LOSS.AML7$lcol, LOSS.AML8$lcol, LOSS.AML9$lcol, LOSS.AML10$lcol, LOSS.AML11$lcol, LOSS.AML12$lcol, LOSS.AML13$lcol, LOSS.AML14$lcol, LOSS.AML15$lcol, LOSS.AML16$lcol, LOSS.AML17$lcol, LOSS.AML18$lcol, LOSS.AML20$lcol)

#LMICE90 = cbind(LOSS.MICE1$lcol, LOSS.MICE2$lcol, LOSS.MICE3$lcol, LOSS.MICE4$lcol, LOSS.MICE5$lcol, LOSS.MICE6$lcol, LOSS.MICE7$lcol, LOSS.MICE8$lcol, LOSS.MICE9$lcol, LOSS.MICE10$lcol, LOSS.MICE11$lcol, LOSS.MICE12$lcol, LOSS.MICE13$lcol, LOSS.MICE14$lcol, LOSS.MICE15$lcol, LOSS.MICE16$lcol, LOSS.MICE17$lcol, LOSS.MICE18$lcol, LOSS.MICE19$lcol, LOSS.MICE20$lcol)

LTRACE90 = cbind(LOSS.GLRM_TR1$lcol, LOSS.GLRM_TR2$lcol, LOSS.GLRM_TR3$lcol, LOSS.GLRM_TR4$lcol, LOSS.GLRM_TR5$lcol, LOSS.GLRM_TR6$lcol, LOSS.GLRM_TR7$lcol, LOSS.GLRM_TR8$lcol, LOSS.GLRM_TR9$lcol, LOSS.GLRM_TR10$lcol, LOSS.GLRM_TR11$lcol, LOSS.GLRM_TR12$lcol, LOSS.GLRM_TR13$lcol, LOSS.GLRM_TR14$lcol, LOSS.GLRM_TR15$lcol, LOSS.GLRM_TR16$lcol, LOSS.GLRM_TR17$lcol, LOSS.GLRM_TR18$lcol, LOSS.GLRM_TR19$lcol, LOSS.GLRM_TR20$lcol)


LOSSAML90 = apply(LAML90, 1, mean, na.rm = TRUE)
#LOSSMICE90 = apply(LMICE90, 1, mean, na.rm = TRUE)
LOSSTRACE90 = apply(LTRACE90, 1, mean, na.rm = TRUE)

save(l.tr90, l.aml90, l.mice90, LAML90, LTRACE90, LOSSAML90,  LOSSTRACE90, file = "all_GSS90.RData")


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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparisons")

load('all_GSS10.RData')
load('all_GSS20.RData')
load('all_GSS30.RData')
load('all_GSS40.RData')
load('all_GSS50.RData')
load('all_GSS60.RData')
load('all_GSS70.RData')
load('all_GSS80.RData')
load('all_GSS90.RData')

TR = cbind(l.tr10/s10, l.tr20/s20, l.tr30/s30, l.tr40/s40, l.tr50/s50, l.tr60/s60, l.tr70/s70, l.tr80/s80, l.tr90/s90)
MC = cbind(l.mice10/s10, l.mice20/s20, l.mice30/s30, l.mice40/s40, l.mice50/s50, l.mice60/s60, l.mice70/s70, l.mice80/s80, l.mice90/s90)
AM = cbind(l.aml10/s10, l.aml20/s20, l.aml30/s30, l.aml40/s40, l.aml50/s50, l.aml60/s60, l.aml70/s70, l.aml80/s80, l.aml90/s90)

mat = rbind(TR, MC, AM)
rownames(mat) = c(rep("trace", 20), rep("mice", 20), rep("amelia", 20))
colnames(mat) = c("10%", "20%","30%","40%","50%","60%","70%","80%","90%")


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Comparisons")


#write.csv(mat, "GSSsparsitycomp.csv")

#mat = read.csv("GSSsparsitycomp.csv")
#mat = mat[,-1]

avg.tr = apply(mat[1:20 ,], 2 , mean, na.rm = TRUE)
avg.mice = apply(mat[21:40 , ], 2 , mean, na.rm = TRUE)
avg.aml = apply(mat[41:60 ,], 2 , mean, na.rm = TRUE)
imp.mice = 100*((avg.mice- avg.tr)/avg.mice)
imp.aml = 100*(avg.aml- avg.tr)/avg.aml

prop.na = function(vec){
	prop.na = 100*(sum(is.na(vec))/length(vec))
}

prop.na.aml = apply(AM, 2, prop.na)
prop.na.mice = apply(MC, 2, prop.na)
prop.na.tr = apply(TR, 2, prop.na)


summ_mat = round(rbind(avg.aml, prop.na.aml , avg.mice, prop.na.mice , avg.tr, 
 prop.na.tr, imp.aml, imp.mice), 2)


library("xtable")
summTab = xtable(summ_mat, align = "|l|c|c|c|c|c|c|c|c|c|")
print(summTab, type = "latex", file = "GSS_summary.tex")


missing_prop = seq(10, 90, by = 10)
ymax = 1.05*max(max(mat, na.rm = TRUE))
ymin = 0.95*min(min(mat, na.rm = TRUE)) 


check = c(rep(10, 20), rep(20, 20), rep(30, 20), rep(40, 20), rep(50, 20), rep(60, 20) , rep(70, 20), rep(80, 20), rep(90, 20)  )

pdf("sparsity_boxplots_gss.pdf")
boxplot(as.vector(AM) ~ check, ylim = c(ymin, ymax ), xlim = c(0.5, 9.5), main = "Sparsity Analysis: GSS Subset", xlab ="Percentage of entries missing", ylab = "Scaled Loss", boxlwd = 1, boxcol = "red", medlwd = 2, medcol = "black", whiskcol = "red", outcol = "red", staplecol = "red", col ="red")
boxplot(as.vector(MC) ~check,  col = "darkgreen", add= TRUE,  boxlwd = 1, boxcol = "darkgreen", medlwd = 2, medcol = "black", whiskcol = "darkgreen", outcol = "darkgreen", staplecol = "darkgreen")
boxplot(as.vector(TR)~ check, col = "blue", add = TRUE,  boxlwd = 1, boxcol = "blue", medlwd = 2, medcol = "black", whiskcol = "blue", outcol = "blue", staplecol = "blue")
lines(seq(1,9), avg.aml,  type = "b", col = rgb(1,0,0), pch = 19, lwd = 1.5 )
lines(seq(1,9), avg.mice,  type = "b", col = "darkgreen", pch = 19, lwd = 1.5)
lines(seq(1,9), avg.tr, type = "b", col = rgb(0,0,1), pch = 19, lwd = 1.5)
legend("topleft", c("Amelia", "MICE", "Trace"),col =  c("red", "darkgreen", "blue"), lty = 1, pch = 19 ,pt.bg = 'white')
dev.off()



#pdf("sparsity_gss.pdf")
plot(missing_prop, avg.aml, type = "b", col = rgb(1,0, 0), main = "Sparsity Analysis: GSS Subset", xlab ="Percentage of entries missing", ylab = "Scaled Loss", xlim = c(10, 90), ylim = c(ymin, ymax ), pch = 19, yaxt = "n")
lines(missing_prop, avg.mice,  type = "b", col = rgb(0,1,0), pch = 19)
lines(missing_prop, avg.tr, type = "b", col = rgb(0,0,1), pch = 19)
axis(1, at = seq(10, 90, by = 10))
axis(2, at = seq(10000, 200000, by = 50000))

legend("topleft", c("Amelia", "MICE", "Trace"),col =  c("red", "green", "blue"), lty = 1, pch = 19 ,pt.bg = 'white')
#dev.off()


std.tr = apply(mat[1:20 , ], 2 , sd, na.rm = TRUE)
std.mice = apply(mat[21:40 , ], 2 , sd, na.rm = TRUE)
std.aml = apply(mat[41:60 ,], 2 , sd, na.rm = TRUE)

CI.up.aml = avg.aml + 2*std.aml
CI.dn.aml = avg.aml - 2*std.aml
CI.up.mice = avg.mice + 2*std.mice
CI.dn.mice = avg.mice - 2*std.mice
CI.up.tr = avg.tr + 2*std.tr
CI.dn.tr = avg.tr - 2*std.tr

x = missing_prop 
pdf("sparsity_gss_ci.pdf")
plot(missing_prop, avg.aml, type = "b", col = rgb(1,0, 0), main = "Sparsity Analysis: GSS Subset", xlab ="Percentage of entries missing", ylab = "Scaled Loss", xlim = c(10, 90), ylim = c(ymin, ymax ), pch = 19, yaxt = "n")
arrows(x,CI.dn.aml,x,CI.up.aml,code=3,length=0.2,angle=90,col='red')
lines(missing_prop, avg.mice,  type = "b", col = rgb(0,1,0), pch = 19)
arrows(x,CI.dn.mice,x,CI.up.mice,code=3,length=0.2,angle=90,col='green')
lines(missing_prop, avg.tr, type = "b", col = rgb(0,0,1), pch = 19)
arrows(x,CI.dn.tr,x,CI.up.tr,code=3,length=0.2,angle=90,col='blue')
axis(1, at = seq(10, 90, by = 10))
axis(2, at = seq(10000, 200000, by = 50000))
legend("topleft", c("Amelia", "MICE", "Trace"),col =  c("red", "green", "blue"), lty = 1, pch = 19 ,pt.bg = 'white')
dev.off()



pdf("sparsity_gss_ci_points.pdf")

plot(missing_prop, avg.aml, type = "b", col = rgb(1,0, 0), main = "Sparsity Analysis: GSS Subset", xlab ="Percentage of entries missing", ylab = "Scaled Loss", xlim = c(10, 90), ylim = c(ymin, ymax ), pch = 19, yaxt = "n")
#arrows(x,CI.dn.aml,x,CI.up.aml,code=3,length=0.2,angle=90,col='red')
lines(missing_prop, avg.mice,  type = "b", col = rgb(0,1,0), pch = 19)
#arrows(x,CI.dn.mice,x,CI.up.mice,code=3,length=0.2,angle=90,col='green')
lines(missing_prop, avg.tr, type = "b", col = rgb(0,0,1), pch = 19)
#arrows(x,CI.dn.tr,x,CI.up.tr,code=3,length=0.2,angle=90,col='blue')
axis(1, at = seq(10, 90, by = 10))
axis(2, at = seq(10000, 200000, by = 50000))
legend("topleft", c("Amelia", "MICE", "Trace"),col =  c("red", "green", "blue"), lty = 1, pch = 19 ,pt.bg = 'white')
lines(missing_prop, AM[1,], type = "p", col = "red")
lines(missing_prop, AM[2,], type = "p", col = "red")
lines(missing_prop, AM[3,], type = "p", col = "red")
lines(missing_prop, AM[4,], type = "p", col = "red")
lines(missing_prop, AM[5,], type = "p", col = "red")
lines(missing_prop, AM[6,], type = "p", col = "red")
lines(missing_prop, AM[7,], type = "p", col = "red")
lines(missing_prop, AM[8,], type = "p", col = "red")
lines(missing_prop, AM[9,], type = "p", col = "red")
lines(missing_prop, AM[10,], type = "p", col = "red")
lines(missing_prop, AM[11,], type = "p", col = "red")
lines(missing_prop, AM[12,], type = "p", col = "red")
lines(missing_prop, AM[13,], type = "p", col = "red")
lines(missing_prop, AM[14,], type = "p", col = "red")
lines(missing_prop, AM[15,], type = "p", col = "red")
lines(missing_prop, AM[16,], type = "p", col = "red")
lines(missing_prop, AM[17,], type = "p", col = "red")
lines(missing_prop, AM[18,], type = "p", col = "red")
lines(missing_prop, AM[19,], type = "p", col = "red")
lines(missing_prop, AM[20,], type = "p", col = "red")

lines(missing_prop, MC[1,], type = "p", col = "green")
lines(missing_prop, MC[2,], type = "p", col = "green")
lines(missing_prop, MC[3,], type = "p", col = "green")
lines(missing_prop, MC[4,], type = "p", col = "green")
lines(missing_prop, MC[5,], type = "p", col = "green")
lines(missing_prop, MC[6,], type = "p", col = "green")
lines(missing_prop, MC[7,], type = "p", col = "green")
lines(missing_prop, MC[8,], type = "p", col = "green")
lines(missing_prop, MC[9,], type = "p", col = "green")
lines(missing_prop, MC[10,], type = "p", col = "green")
lines(missing_prop, MC[11,], type = "p", col = "green")
lines(missing_prop, MC[12,], type = "p", col = "green")
lines(missing_prop, MC[13,], type = "p", col = "green")
lines(missing_prop, MC[14,], type = "p", col = "green")
lines(missing_prop, MC[15,], type = "p", col = "green")
lines(missing_prop, MC[16,], type = "p", col = "green")
lines(missing_prop, MC[17,], type = "p", col = "green")
lines(missing_prop, MC[18,], type = "p", col = "green")
lines(missing_prop, MC[19,], type = "p", col = "green")
lines(missing_prop, MC[20,], type = "p", col = "green")

lines(missing_prop, TR[1,], type = "p", col = "blue")
lines(missing_prop, TR[2,], type = "p", col = "blue")
lines(missing_prop, TR[3,], type = "p", col = "blue")
lines(missing_prop, TR[4,], type = "p", col = "blue")
lines(missing_prop, TR[5,], type = "p", col = "blue")
lines(missing_prop, TR[6,], type = "p", col = "blue")
lines(missing_prop, TR[7,], type = "p", col = "blue")
lines(missing_prop, TR[8,], type = "p", col = "blue")
lines(missing_prop, TR[9,], type = "p", col = "blue")
lines(missing_prop, TR[10,], type = "p", col = "blue")
lines(missing_prop, TR[11,], type = "p", col = "blue")
lines(missing_prop, TR[12,], type = "p", col = "blue")
lines(missing_prop, TR[13,], type = "p", col = "blue")
lines(missing_prop, TR[14,], type = "p", col = "blue")
lines(missing_prop, TR[15,], type = "p", col = "blue")
lines(missing_prop, TR[16,], type = "p", col = "blue")
lines(missing_prop, TR[17,], type = "p", col = "blue")
lines(missing_prop, TR[18,], type = "p", col = "blue")
lines(missing_prop, TR[19,], type = "p", col = "blue")
lines(missing_prop, TR[20,], type = "p", col = "blue")

dev.off()




##################### columnwise losses ###########################
#setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/prediction/GSS/Data")

#df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed

col.labs = names(df.base)

real_list = real1
ord_list = ord1
cat_list = cat1
all_col = c(length(real_list), length(ord_list), length(cat_list) )

# Make Table here 

col1 = c(mean(l.aml10, na.rm = TRUE), mean(l.mice10, na.rm = TRUE), mean(l.tr10, na.rm = TRUE))
col2 = c(((mean(l.aml10, na.rm = TRUE)-mean(l.aml10, na.rm = TRUE))/mean(l.aml10, na.rm = TRUE)), ((mean(l.aml10, na.rm = TRUE)-mean(l.mice10, na.rm = TRUE))/mean(l.aml10, na.rm = TRUE)), ((mean(l.aml10, na.rm = TRUE)-mean(l.tr10, na.rm = TRUE))/mean(l.aml10, na.rm = TRUE)))
col3 = c(((mean(l.mice10, na.rm = TRUE)-mean(l.aml10, na.rm = TRUE))/mean(l.mice10, na.rm = TRUE)), ((mean(l.mice10, na.rm = TRUE)-mean(l.mice10, na.rm = TRUE))/mean(l.mice10, na.rm = TRUE)),((mean(l.mice10, na.rm = TRUE)-mean(l.tr10, na.rm = TRUE))/mean(l.mice10, na.rm = TRUE)))

meth.select = numeric()
col.meth.select = numeric()

lcol.all = cbind(LOSSAML10, LOSSMICE10, LOSSTRACE10)

# columnwise gains over Amelia

tr.aml.lcol10  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol10  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]


# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account10= col.account[-1, ]

tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)

tr_col10 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col10 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col10 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test10 = rbind(aml_col10, mice_col10, tr_col10 )


rm(lcol.all, tr_list, aml_list, mice_list)

########## 20 

# Make Table here 

col1 = c(col1, c(mean(l.aml20, na.rm = TRUE), mean(l.mice20, na.rm = TRUE), mean(l.tr20, na.rm = TRUE)))
col2 = c( col2, c(((mean(l.aml20, na.rm = TRUE)-mean(l.aml20, na.rm = TRUE))/mean(l.aml20, na.rm = TRUE)), ((mean(l.aml20, na.rm = TRUE)-mean(l.mice20, na.rm = TRUE))/mean(l.aml20, na.rm = TRUE)), ((mean(l.aml20, na.rm = TRUE)-mean(l.tr20, na.rm = TRUE))/mean(l.aml20, na.rm = TRUE))))
col3 = c(col3, c(((mean(l.mice20, na.rm = TRUE)-mean(l.aml20, na.rm = TRUE))/mean(l.mice20, na.rm = TRUE)), ((mean(l.mice20, na.rm = TRUE)-mean(l.mice20, na.rm = TRUE))/mean(l.mice20, na.rm = TRUE)),((mean(l.mice20, na.rm = TRUE)-mean(l.tr20, na.rm = TRUE))/mean(l.mice20, na.rm = TRUE))))

meth.select = numeric()
col.meth.select = numeric()

lcol.all = cbind(LOSSAML20, LOSSMICE20, LOSSTRACE20)

# columnwise gains over Amelia

tr.aml.lcol20  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol20  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]


# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account20= col.account[-1, ]

tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)

tr_col20 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col20 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col20 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test20 = rbind(aml_col20, mice_col20, tr_col20 )

rm(lcol.all, tr_list, aml_list, mice_list)


########## 30 

# Make Table here 

col1 = c(col1, c(mean(l.aml30, na.rm = TRUE), mean(l.mice30, na.rm = TRUE), mean(l.tr30, na.rm = TRUE)))
col2 = c( col2, c(((mean(l.aml30, na.rm = TRUE)-mean(l.aml30, na.rm = TRUE))/mean(l.aml30, na.rm = TRUE)), ((mean(l.aml30, na.rm = TRUE)-mean(l.mice30, na.rm = TRUE))/mean(l.aml30, na.rm = TRUE)), ((mean(l.aml30, na.rm = TRUE)-mean(l.tr30, na.rm = TRUE))/mean(l.aml30, na.rm = TRUE))))
col3 = c(col3, c(((mean(l.mice30, na.rm = TRUE)-mean(l.aml30, na.rm = TRUE))/mean(l.mice30, na.rm = TRUE)), ((mean(l.mice30, na.rm = TRUE)-mean(l.mice30, na.rm = TRUE))/mean(l.mice30, na.rm = TRUE)),((mean(l.mice30, na.rm = TRUE)-mean(l.tr30, na.rm = TRUE))/mean(l.mice30, na.rm = TRUE))))

meth.select = numeric()
col.meth.select = numeric()

lcol.all = cbind(LOSSAML30, LOSSMICE30, LOSSTRACE30)

# columnwise gains over Amelia

tr.aml.lcol30  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol30  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]


# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account30= col.account[-1, ]

tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)

tr_col30 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col30 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col30 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test30 = rbind(aml_col30, mice_col30, tr_col30 )


rm(lcol.all, tr_list, aml_list, mice_list)

########## 40 

# Make Table here 

col1 = c(col1, c(mean(l.aml40, na.rm = TRUE), mean(l.mice40, na.rm = TRUE), mean(l.tr40, na.rm = TRUE)))
col2 = c( col2, c(((mean(l.aml40, na.rm = TRUE)-mean(l.aml40, na.rm = TRUE))/mean(l.aml40, na.rm = TRUE)), ((mean(l.aml40, na.rm = TRUE)-mean(l.mice40, na.rm = TRUE))/mean(l.aml40, na.rm = TRUE)), ((mean(l.aml40, na.rm = TRUE)-mean(l.tr40, na.rm = TRUE))/mean(l.aml40, na.rm = TRUE))))
col3 = c(col3, c(((mean(l.mice40, na.rm = TRUE)-mean(l.aml40, na.rm = TRUE))/mean(l.mice40, na.rm = TRUE)), ((mean(l.mice40, na.rm = TRUE)-mean(l.mice40, na.rm = TRUE))/mean(l.mice40, na.rm = TRUE)),((mean(l.mice40, na.rm = TRUE)-mean(l.tr40, na.rm = TRUE))/mean(l.mice40, na.rm = TRUE))))

meth.select = numeric()
col.meth.select = numeric()

lcol.all = cbind(LOSSAML40, LOSSMICE40, LOSSTRACE40)

# columnwise gains over Amelia

tr.aml.lcol40  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol40  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]


# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account40= col.account[-1, ]

tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)

tr_col40 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col40 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col40 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test40 = rbind(aml_col40, mice_col40, tr_col40 )


rm(lcol.all, tr_list, aml_list, mice_list)

########## 50 

# Make Table here 

col1 = c(col1, c(mean(l.aml50, na.rm = TRUE), mean(l.mice50, na.rm = TRUE), mean(l.tr50, na.rm = TRUE)))
col2 = c( col2, c(((mean(l.aml50, na.rm = TRUE)-mean(l.aml50, na.rm = TRUE))/mean(l.aml50, na.rm = TRUE)), ((mean(l.aml50, na.rm = TRUE)-mean(l.mice50, na.rm = TRUE))/mean(l.aml50, na.rm = TRUE)), ((mean(l.aml50, na.rm = TRUE)-mean(l.tr50, na.rm = TRUE))/mean(l.aml50, na.rm = TRUE))))
col3 = c(col3, c(((mean(l.mice50, na.rm = TRUE)-mean(l.aml50, na.rm = TRUE))/mean(l.mice50, na.rm = TRUE)), ((mean(l.mice50, na.rm = TRUE)-mean(l.mice50, na.rm = TRUE))/mean(l.mice50, na.rm = TRUE)),((mean(l.mice50, na.rm = TRUE)-mean(l.tr50, na.rm = TRUE))/mean(l.mice50, na.rm = TRUE))))

meth.select = numeric()
col.meth.select = numeric()

lcol.all = cbind(LOSSAML50, LOSSMICE50, LOSSTRACE50)

# columnwise gains over Amelia

tr.aml.lcol50  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol50  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]


# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account50= col.account[-1, ]

tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)

tr_col50 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col50 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col50 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test50 = rbind(aml_col50, mice_col50, tr_col50 )


rm(lcol.all, tr_list, aml_list, mice_list)


########## 60 

# Make Table here 

col1 = c(col1, c(mean(l.aml60, na.rm = TRUE), mean(l.mice60, na.rm = TRUE), mean(l.tr60, na.rm = TRUE)))
col2 = c( col2, c(((mean(l.aml60, na.rm = TRUE)-mean(l.aml60, na.rm = TRUE))/mean(l.aml60, na.rm = TRUE)), ((mean(l.aml60, na.rm = TRUE)-mean(l.mice60, na.rm = TRUE))/mean(l.aml60, na.rm = TRUE)), ((mean(l.aml60, na.rm = TRUE)-mean(l.tr60, na.rm = TRUE))/mean(l.aml60, na.rm = TRUE))))
col3 = c(col3, c(((mean(l.mice60, na.rm = TRUE)-mean(l.aml60, na.rm = TRUE))/mean(l.mice60, na.rm = TRUE)), ((mean(l.mice60, na.rm = TRUE)-mean(l.mice60, na.rm = TRUE))/mean(l.mice60, na.rm = TRUE)),((mean(l.mice60, na.rm = TRUE)-mean(l.tr60, na.rm = TRUE))/mean(l.mice60, na.rm = TRUE))))

meth.select = numeric()
col.meth.select = numeric()

lcol.all = cbind(LOSSAML60, LOSSMICE60, LOSSTRACE60)

# columnwise gains over Amelia

tr.aml.lcol60  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol60  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]


# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account60= col.account[-1, ]

tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)

tr_col60 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col60 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col60 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test60 = rbind(aml_col60, mice_col60, tr_col60 )

rm(lcol.all, tr_list, aml_list, mice_list)


########## 70 

# Make Table here 

col1 = c(col1, c(mean(l.aml70, na.rm = TRUE), mean(l.mice70, na.rm = TRUE), mean(l.tr70, na.rm = TRUE)))
col2 = c( col2, c(((mean(l.aml70, na.rm = TRUE)-mean(l.aml70, na.rm = TRUE))/mean(l.aml70, na.rm = TRUE)), ((mean(l.aml70, na.rm = TRUE)-mean(l.mice70, na.rm = TRUE))/mean(l.aml70, na.rm = TRUE)), ((mean(l.aml70, na.rm = TRUE)-mean(l.tr70, na.rm = TRUE))/mean(l.aml70, na.rm = TRUE))))
col3 = c(col3, c(((mean(l.mice70, na.rm = TRUE)-mean(l.aml70, na.rm = TRUE))/mean(l.mice70, na.rm = TRUE)), ((mean(l.mice70, na.rm = TRUE)-mean(l.mice70, na.rm = TRUE))/mean(l.mice70, na.rm = TRUE)),((mean(l.mice70, na.rm = TRUE)-mean(l.tr70, na.rm = TRUE))/mean(l.mice70, na.rm = TRUE))))

meth.select = numeric()
col.meth.select = numeric()

lcol.all = cbind(LOSSAML70, LOSSMICE70, LOSSTRACE70)

# columnwise gains over Amelia

tr.aml.lcol70  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol70  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]


# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,])))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,])) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,])))))	}

col.account70= col.account[-1, ]

tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)

tr_col70 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col70 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col70 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test70 = rbind(aml_col70, mice_col70, tr_col70 )

rm(lcol.all, tr_list, aml_list, mice_list)

########## 80 

# Make Table here 

col1 = c(col1, c(mean(l.aml80, na.rm = TRUE), mean(l.mice80, na.rm = TRUE), mean(l.tr80, na.rm = TRUE)))
col2 = c( col2, c(((mean(l.aml80, na.rm = TRUE)-mean(l.aml80, na.rm = TRUE))/mean(l.aml80, na.rm = TRUE)), ((mean(l.aml80, na.rm = TRUE)-mean(l.mice80, na.rm = TRUE))/mean(l.aml80, na.rm = TRUE)), ((mean(l.aml80, na.rm = TRUE)-mean(l.tr80, na.rm = TRUE))/mean(l.aml80, na.rm = TRUE))))
col3 = c(col3, c(((mean(l.mice80, na.rm = TRUE)-mean(l.aml80, na.rm = TRUE))/mean(l.mice80, na.rm = TRUE)), ((mean(l.mice80, na.rm = TRUE)-mean(l.mice80, na.rm = TRUE))/mean(l.mice80, na.rm = TRUE)),((mean(l.mice80, na.rm = TRUE)-mean(l.tr80, na.rm = TRUE))/mean(l.mice80, na.rm = TRUE))))

meth.select = numeric()
col.meth.select = numeric()

lcol.all = cbind(LOSSAML80, NA, LOSSTRACE80)

# columnwise gains over Amelia

tr.aml.lcol80  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]

# columnwise gains over MICE

tr.mice.lcol80  = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]


# columwise lowest loss

col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,], na.rm = TRUE)))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,], na.rm = TRUE) ) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,], na.rm = TRUE)))))	}

col.account80= col.account[-1, ]

tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)

tr_col80 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col80 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col80 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test80 = rbind(aml_col80, mice_col80, tr_col80 )

rm(lcol.all, tr_list, aml_list, mice_list)


########## 90 

# Make Table here 

col1 = c(col1, c(mean(l.aml90, na.rm = TRUE), mean(l.mice90, na.rm = TRUE), mean(l.tr90, na.rm = TRUE)))
col2 = c( col2, c(((mean(l.aml90, na.rm = TRUE)-mean(l.aml90, na.rm = TRUE))/mean(l.aml90, na.rm = TRUE)), ((mean(l.aml90, na.rm = TRUE)-mean(l.mice90, na.rm = TRUE))/mean(l.aml90, na.rm = TRUE)), ((mean(l.aml90, na.rm = TRUE)-mean(l.tr90, na.rm = TRUE))/mean(l.aml90, na.rm = TRUE))))
col3 = c(col3, c(((mean(l.mice90, na.rm = TRUE)-mean(l.aml90, na.rm = TRUE))/mean(l.mice90, na.rm = TRUE)), ((mean(l.mice90, na.rm = TRUE)-mean(l.mice90, na.rm = TRUE))/mean(l.mice90, na.rm = TRUE)),((mean(l.mice90, na.rm = TRUE)-mean(l.tr90, na.rm = TRUE))/mean(l.mice90, na.rm = TRUE))))

meth.select = numeric()
col.meth.select = numeric()

lcol.all = cbind(LOSSAML90, NA, LOSSTRACE90)

# columnwise gains over Amelia

tr.aml.lcol90  = (lcol.all[,1] -lcol.all[,3])/lcol.all[,1]

# columnwise gains over MICE


tr.mice.lcol90   = (lcol.all[,2] -lcol.all[,3])/lcol.all[,2]


col.account = rep(0,2)
for (v in 1: dim(lcol.all)[1]){
	meth.select = c(meth.select, which(lcol.all[v,] == min(lcol.all[v,], na.rm = TRUE)))
	col.meth.select = c(col.meth.select, col.labs[v], which(lcol.all[v,] == min(lcol.all[v,], na.rm = TRUE) ) )
	col.account = rbind(col.account, c(v, length(which(lcol.all[v,] == min(lcol.all[v,], na.rm = TRUE)))))	}

col.account80= col.account[-1, ]

tr_list = which(meth.select == 3)
mice_list = which(meth.select == 2)
aml_list = which(meth.select == 1)

tr_col90 = c(sum(is.element(tr_list, real_list) == TRUE), sum(is.element(tr_list, ord_list)== TRUE), sum(is.element(tr_list, cat_list)== TRUE))
mice_col90 = c(sum(is.element(mice_list, real_list) == TRUE), sum(is.element(mice_list, ord_list)== TRUE), sum(is.element(mice_list, cat_list)== TRUE))
aml_col90 = c(sum(is.element(aml_list, real_list) == TRUE), sum(is.element(aml_list, ord_list)== TRUE), sum(is.element(aml_list, cat_list)== TRUE))
test90 = rbind(aml_col90, mice_col90, tr_col90 )




rm(lcol.all, tr_list, aml_list, mice_list)


pdf("columnwise_density_gss.pdf")
par(mfrow = c(3,3))
plot(density(tr.aml.lcol10*100), col = "blue", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 10%")
#axis(2, at = seq(0, 0.04, 0.01), labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol10*100), col = "darkgreen", lwd = 5)
legend("topleft", c("AML", "MICE"), lty = c(1,1), lwd = c(5,5),  col = c("blue", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol20*100), col = "blue", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 20%")
#axis(2, at = seq(0, 0.04, 0.01), labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol20*100), col = "darkgreen", lwd = 5)
legend("topleft", c("AML", "MICE"), lty = c(1,1), lwd = c(5,5),  col = c("blue", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol30*100), col = "blue", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 30%")
#axis(2, at = seq(0, 0.04, 0.01), labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol30*100), col = "darkgreen", lwd = 5)
legend("topleft", c("AML", "MICE"), lty = c(1,1), lwd = c(5,5),  col = c("blue", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)


plot(density(tr.aml.lcol40*100), col = "blue", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 40%")
#axis(2, at = seq(0, 0.04, 0.01), labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol40*100), col = "darkgreen", lwd = 5)
legend("topleft", c("AML", "MICE"), lty = c(1,1), lwd = c(5,5),  col = c("blue", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)


plot(density(tr.aml.lcol50*100), col = "blue", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 50%")
#axis(2, at = seq(0, 0.04, 0.01), labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol50*100), col = "darkgreen", lwd = 5)
legend("topleft", c("AML", "MICE"), lty = c(1,1), lwd = c(5,5),  col = c("blue", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol60*100), col = "blue", xlim = c(-200, 100), ylim = c(0, 0.025), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 60%")
#axis(2, at = seq(0, 0.04, 0.01), labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol60*100), col = "darkgreen", lwd = 5)
legend("topleft", c("AML", "MICE"), lty = c(1,1), lwd = c(5,5),  col = c("blue", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol70*100), col = "blue", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 70%")
#axis(2, at = seq(0, 0.04, 0.01), labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol70*100), col = "darkgreen", lwd = 5)
legend("topleft", c("AML", "MICE"), lty = c(1,1), lwd = c(5,5),  col = c("blue", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol80*100), col = "blue", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 80%")
#axis(2, at = seq(0, 0.04, 0.01), labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol80*100), col = "darkgreen", lwd = 5)
legend("topleft", c("AML", "MICE"), lty = c(1,1), lwd = c(5,5),  col = c("blue", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)

plot(density(tr.aml.lcol90*100), col = "blue", xlim = c(-200, 100), ylim = c(0, 0.026), lwd = 5, xlab = "Percentage Gain",  yaxt = "n", ylab = "", main = "GSS: 90% ")
#axis(2, at = seq(0, 0.04, 0.01), labels = paste(seq(0, 40, 10) , "%", sep = ""))
lines(density(tr.mice.lcol90*100), col = "darkgreen", lwd = 5)
legend("topleft", c("AML", "MICE"), lty = c(1,1), lwd = c(5,5),  col = c("blue", "darkgreen"))
lines(rep(0, 40) , seq(-0.001,0.03, length.out = 40 ), lty = 2)
dev.off()


par(mfrow = c(3,3))
barplot(t(test10), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE",  "TRACE"), main = "GSS: 10% ")
barplot(t(test20), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE",  "TRACE"), main = "GSS: 20% ")
barplot(t(test30), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE",  "TRACE"), main = "GSS: 30% ")
barplot(t(test40), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE",  "TRACE"), main = "GSS: 40% ")
barplot(t(test50), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE",  "TRACE"), main = "GSS: 50% ")
barplot(t(test60), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE",  "TRACE"), main = "GSS: 60% ")
barplot(t(test70), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE",  "TRACE"), main = "GSS: 70% ")
barplot(t(test80), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE",  "TRACE"), main = "GSS: 80% ")
barplot(t(test90), horiz = TRUE, beside = TRUE, legend=  c("real", "ordinal", "categorical"), args.legend=list( x ="bottomright") , las = 2, names.arg= c("AMELIA", "MICE",  "TRACE"), main = "GSS: 90% ")

quartz()


pdf("columnwise_sep_gss.pdf")
par(mfrow = c(3,3))
barplot(test10, horiz = TRUE, beside = TRUE, legend=  c("amelia", "mice", "trace"), args.legend=list( x ="bottomright", bty = "n") , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 10% ")
barplot(test20, horiz = TRUE, beside = TRUE, legend=  c("amelia", "mice", "trace"), args.legend=list( x ="bottomright", bty = "n") , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 20% ")
barplot(test30, horiz = TRUE, beside = TRUE, legend=  c("amelia", "mice", "trace"), args.legend=list( x ="bottomright", bty = "n") , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 30% ")
barplot(test40, horiz = TRUE, beside = TRUE, legend=  c("amelia", "mice", "trace"), args.legend=list( x ="bottomright", bty = "n") , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 40% ")
barplot(test50, horiz = TRUE, beside = TRUE, legend=  c("amelia", "mice", "trace"), args.legend=list( x ="bottomright", bty = "n") , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 50% ")
barplot(test60, horiz = TRUE, beside = TRUE, legend=  c("amelia", "mice", "trace"), args.legend=list( x ="bottomright", bty = "n") , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 60% ")
barplot(test70, horiz = TRUE, beside = TRUE, legend=  c("amelia", "mice", "trace"), args.legend=list( x ="bottomright", bty = "n") , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 70% ")
barplot(test80, horiz = TRUE, beside = TRUE, legend=  c("amelia", "mice", "trace"), args.legend=list( x ="bottomright", bty = "n") , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 80% ")
barplot(test90, horiz = TRUE, beside = TRUE, legend=  c("amelia", "mice", "trace"), args.legend=list( x ="bottomright", bty = "n") , las = 2, names.arg= c("REAL", "ORD",  "CAT"), main = "GSS: 90% ")
dev.off()



