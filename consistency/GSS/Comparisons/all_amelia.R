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
		if(!is.na(df.base[r,c])){
		l = l+ (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lind = (1 - 1*(df.base[r,c]== IMP[r,c] ))
		lcol[c] = lcol[c]+ lind} }
	 

# for remaining columns loss is squared difference
		
    if (!is.element(c,cat1) ){
    	if(!is.na(df.base[r,c])){
    	l = l + (df.base[r,c] - IMP[r,c])^2
    	lind = (df.base[r,c] - IMP[r,c])^2	
	lcol[c] = lcol[c] + lind}}
	
	#	print(c(j, c, r, lind, lcol[c], l))
		}	

	rm(lind)
	
# get back total loss, columnwise loss and checks for factor levels
	return(list(loss = l/length(s), lcol = lcol))}


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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/GSS/Data")

df.base = read.csv("GSScleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("GSScategoricals.csv")[, -1]
ord1 = read.csv("GSSordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))
l.aml10 = numeric()
l.aml20 = numeric()
l.aml30 = numeric()
l.aml40 = numeric()
l.aml50 = numeric()
l.aml60 = numeric()
l.aml70 = numeric()
l.aml80 = numeric()
l.aml90 = numeric()

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/GSS/AMELIA/")


##### 10

load('amelia_GSS10 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, AML1,  sdf1, LOSS.AML1)

load('amelia_GSS10 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_GSS10 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (12).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (13).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (14).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (15).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (16).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (17).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (18).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS10 (19).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


##### 20

load('amelia_GSS20 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_GSS20 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (12).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (13).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (14).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (15).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (16).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (17).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (18).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS20 (19).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)



##### 30

load('amelia_GSS30 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_GSS30 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (12).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (13).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (14).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (15).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (16).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (17).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (18).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS30 (19).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)



##### 40

load('amelia_GSS40 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_GSS40 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (12).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (13).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (14).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (15).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (16).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (17).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (18).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS40 (19).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)



##### 50

load('amelia_GSS50 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_GSS50 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (12).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (13).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (14).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (15).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (16).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (17).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (18).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS50 (19).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)



##### 60

load('amelia_GSS60 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_GSS60 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (12).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (13).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (14).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (15).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (16).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (17).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (18).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS60 (19).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)



##### 70

load('amelia_GSS70 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_GSS70 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (12).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (13).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (14).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (15).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (16).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (17).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (18).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS70 (19).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)



##### 80

load('amelia_GSS80 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_GSS80 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (12).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (13).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (14).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (15).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (16).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (17).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (18).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS80 (19).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)



##### 90

load('amelia_GSS90 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_GSS90 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (12).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (13).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (14).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (15).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (16).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (17).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (18).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_GSS90 (19).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


save(l.aml10, l.aml20, l.aml30, l.aml40, l.aml50, l.aml60, l.aml70, l.aml80, l.aml90, file = "amelia_consistency.RData")




