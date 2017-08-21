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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/Data")

df.base = read.csv("NLSYcleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("NLSYcategoricals.csv")[, -1]
ord1 = read.csv("NLSYordinals.csv")[, -1]
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

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/AMELIA/")



##### 10


load('amelia_NLSY10_1.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)

LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,sdf3,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,sdf4,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,sdf5,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,sdf6,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,sdf7,cat1,ord1, real1 )


l.aml10 = c(l.aml10, LOSS.AML1$loss, LOSS.AML2$loss, LOSS.AML3$loss, LOSS.AML4$loss, LOSS.AML5$loss, LOSS.AML6$loss, LOSS.AML7$loss)

rm(am1, am2, am3, am4, am5, am6, am7, 
AML1, AML2, AML3, AML4, AML5, AML6, AML7,  
sdf1, sdf2, sdf3, sdf4, sdf5, sdf6, sdf7
, LOSS.AML1, LOSS.AML2, LOSS.AML3, LOSS.AML4, LOSS.AML5, LOSS.AML6, LOSS.AML7)


load('amelia_NLSY10_2.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)


LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )


l.aml10 = c(l.aml10, LOSS.AML1$loss, LOSS.AML2$loss)


rm(am1, am2, AML1, AML2, sdf1, sdf2, LOSS.AML1, LOSS.AML2)




load('amelia_NLSY10.RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, AML1,  sdf1, LOSS.AML1)

load('amelia_NLSY10 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, AML1,  sdf1, LOSS.AML1)

load('amelia_NLSY10 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY10 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY10 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY10 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY10 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY10 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY10 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY10 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY10 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_NLSY10 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml10 = c(l.aml10, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


##### 20


load('amelia_NLSY20_1.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)

LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,sdf3,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,sdf4,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,sdf5,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,sdf6,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,sdf7,cat1,ord1, real1 )


l.aml20 = c(l.aml20, LOSS.AML1$loss, LOSS.AML2$loss, LOSS.AML3$loss, LOSS.AML4$loss, LOSS.AML5$loss, LOSS.AML6$loss, LOSS.AML7$loss)

rm(am1, am2, am3, am4, am5, am6, am7, 
AML1, AML2, AML3, AML4, AML5, AML6, AML7,  
sdf1, sdf2, sdf3, sdf4, sdf5, sdf6, sdf7
, LOSS.AML1, LOSS.AML2, LOSS.AML3, LOSS.AML4, LOSS.AML5, LOSS.AML6, LOSS.AML7)

load('amelia_NLSY20_2.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)


LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )


l.aml20 = c(l.aml20, LOSS.AML1$loss, LOSS.AML2$loss)


rm(am1, am2, AML1, AML2, sdf1, sdf2, LOSS.AML1, LOSS.AML2)


load('amelia_NLSY20 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY20 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY20 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY20 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY20 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY20 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY20 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY20 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY20 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY20 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_NLSY20 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml20 = c(l.aml20, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


##### 30


load('amelia_NLSY30_1.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)

LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,sdf3,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,sdf4,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,sdf5,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,sdf6,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,sdf7,cat1,ord1, real1 )


l.aml30 = c(l.aml30, LOSS.AML1$loss, LOSS.AML2$loss, LOSS.AML3$loss, LOSS.AML4$loss, LOSS.AML5$loss, LOSS.AML6$loss, LOSS.AML7$loss)

rm(am1, am2, am3, am4, am5, am6, am7, 
AML1, AML2, AML3, AML4, AML5, AML6, AML7,  
sdf1, sdf2, sdf3, sdf4, sdf5, sdf6, sdf7
, LOSS.AML1, LOSS.AML2, LOSS.AML3, LOSS.AML4, LOSS.AML5, LOSS.AML6, LOSS.AML7)

load('amelia_NLSY30_2.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)


LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )


l.aml30 = c(l.aml30, LOSS.AML1$loss, LOSS.AML2$loss)


rm(am1, am2, AML1, AML2, sdf1, sdf2, LOSS.AML1, LOSS.AML2)



load('amelia_NLSY30 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY30 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY30 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY30 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY30 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY30 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY30 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY30 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY30 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY30 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_NLSY30 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml30 = c(l.aml30, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)




##### 40


load('amelia_NLSY40_1.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)

LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,sdf3,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,sdf4,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,sdf5,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,sdf6,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,sdf7,cat1,ord1, real1 )


l.aml40 = c(l.aml40, LOSS.AML1$loss, LOSS.AML2$loss, LOSS.AML3$loss, LOSS.AML4$loss, LOSS.AML5$loss, LOSS.AML6$loss, LOSS.AML7$loss)

rm(am1, am2, am3, am4, am5, am6, am7, 
AML1, AML2, AML3, AML4, AML5, AML6, AML7,  
sdf1, sdf2, sdf3, sdf4, sdf5, sdf6, sdf7
, LOSS.AML1, LOSS.AML2, LOSS.AML3, LOSS.AML4, LOSS.AML5, LOSS.AML6, LOSS.AML7)


load('amelia_NLSY40_2.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)


LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )


l.aml40 = c(l.aml40, LOSS.AML1$loss, LOSS.AML2$loss)


rm(am1, am2, AML1, AML2, sdf1, sdf2, LOSS.AML1, LOSS.AML2)




load('amelia_NLSY40 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY40 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY40 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY40 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY40 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY40 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY40 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY40 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY40 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY40 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_NLSY40 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml40 = c(l.aml40, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)



##### 50


load('amelia_NLSY50_1.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)

LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,sdf3,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,sdf4,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,sdf5,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,sdf6,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,sdf7,cat1,ord1, real1 )


l.aml50 = c(l.aml50, LOSS.AML1$loss, LOSS.AML2$loss, LOSS.AML3$loss, LOSS.AML4$loss, LOSS.AML5$loss, LOSS.AML6$loss, LOSS.AML7$loss)

rm(am1, am2, am3, am4, am5, am6, am7, 
AML1, AML2, AML3, AML4, AML5, AML6, AML7,  
sdf1, sdf2, sdf3, sdf4, sdf5, sdf6, sdf7
, LOSS.AML1, LOSS.AML2, LOSS.AML3, LOSS.AML4, LOSS.AML5, LOSS.AML6, LOSS.AML7)

load('amelia_NLSY50_2.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)


LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )


l.aml50 = c(l.aml50, LOSS.AML1$loss, LOSS.AML2$loss)


rm(am1, am2, AML1, AML2, sdf1, sdf2, LOSS.AML1, LOSS.AML2)


load('amelia_NLSY50 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY50 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY50 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY50 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY50 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY50 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY50 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY50 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY50 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY50 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_NLSY50 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml50 = c(l.aml50, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)




##### 60


load('amelia_NLSY60_1.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)

LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,sdf3,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,sdf4,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,sdf5,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,sdf6,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,sdf7,cat1,ord1, real1 )


l.aml60 = c(l.aml60, LOSS.AML1$loss, LOSS.AML2$loss, LOSS.AML3$loss, LOSS.AML4$loss, LOSS.AML5$loss, LOSS.AML6$loss, LOSS.AML7$loss)

rm(am1, am2, am3, am4, am5, am6, am7, 
AML1, AML2, AML3, AML4, AML5, AML6, AML7,  
sdf1, sdf2, sdf3, sdf4, sdf5, sdf6, sdf7
, LOSS.AML1, LOSS.AML2, LOSS.AML3, LOSS.AML4, LOSS.AML5, LOSS.AML6, LOSS.AML7)

load('amelia_NLSY60_2.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)


LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )


l.aml60 = c(l.aml60, LOSS.AML1$loss, LOSS.AML2$loss)


rm(am1, am2, AML1, AML2, sdf1, sdf2, LOSS.AML1, LOSS.AML2)


load('amelia_NLSY60 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY60 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY60 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY60 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY60 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY60 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY60 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY60 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY60 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY60 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_NLSY60 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml60 = c(l.aml60, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)



##### 70


load('amelia_NLSY70_1.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)

LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,sdf3,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,sdf4,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,sdf5,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,sdf6,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,sdf7,cat1,ord1, real1 )


l.aml70 = c(l.aml70, LOSS.AML1$loss, LOSS.AML2$loss, LOSS.AML3$loss, LOSS.AML4$loss, LOSS.AML5$loss, LOSS.AML6$loss, LOSS.AML7$loss)

rm(am1, am2, am3, am4, am5, am6, am7, 
AML1, AML2, AML3, AML4, AML5, AML6, AML7,  
sdf1, sdf2, sdf3, sdf4, sdf5, sdf6, sdf7
, LOSS.AML1, LOSS.AML2, LOSS.AML3, LOSS.AML4, LOSS.AML5, LOSS.AML6, LOSS.AML7)

load('amelia_NLSY70_2.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)


LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )


l.aml70 = c(l.aml70, LOSS.AML1$loss, LOSS.AML2$loss)


rm(am1, am2, AML1, AML2, sdf1, sdf2, LOSS.AML1, LOSS.AML2)


load('amelia_NLSY70 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY70 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY70 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY70 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY70 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY70 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY70 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY70 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY70 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY70 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_NLSY70 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml70 = c(l.aml70, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

##### 80


load('amelia_NLSY80_1.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)

LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,sdf3,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,sdf4,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,sdf5,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,sdf6,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,sdf7,cat1,ord1, real1 )


l.aml80 = c(l.aml80, LOSS.AML1$loss, LOSS.AML2$loss, LOSS.AML3$loss, LOSS.AML4$loss, LOSS.AML5$loss, LOSS.AML6$loss, LOSS.AML7$loss)

rm(am1, am2, am3, am4, am5, am6, am7, 
AML1, AML2, AML3, AML4, AML5, AML6, AML7,  
sdf1, sdf2, sdf3, sdf4, sdf5, sdf6, sdf7
, LOSS.AML1, LOSS.AML2, LOSS.AML3, LOSS.AML4, LOSS.AML5, LOSS.AML6, LOSS.AML7)

load('amelia_NLSY80_2.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)


LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )


l.aml80 = c(l.aml80, LOSS.AML1$loss, LOSS.AML2$loss)


rm(am1, am2, AML1, AML2, sdf1, sdf2, LOSS.AML1, LOSS.AML2)



load('amelia_NLSY80 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY80 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY80 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY80 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY80 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY80 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY80 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY80 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY80 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY80 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_NLSY80 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml80 = c(l.aml80, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)



##### 90


load('amelia_NLSY90_1.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)
AML3= factorLevels(am3, df.base, cat1)
AML4= factorLevels(am4, df.base, cat1)
AML5= factorLevels(am5, df.base, cat1)
AML6= factorLevels(am6, df.base, cat1)
AML7= factorLevels(am7, df.base, cat1)

LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )
LOSS.AML3 = loss(AML3,df.base,sdf3,cat1,ord1, real1 )
LOSS.AML4 = loss(AML4,df.base,sdf4,cat1,ord1, real1 )
LOSS.AML5 = loss(AML5,df.base,sdf5,cat1,ord1, real1 )
LOSS.AML6 = loss(AML6,df.base,sdf6,cat1,ord1, real1 )
LOSS.AML7 = loss(AML7,df.base,sdf7,cat1,ord1, real1 )


l.aml90 = c(l.aml90, LOSS.AML1$loss, LOSS.AML2$loss, LOSS.AML3$loss, LOSS.AML4$loss, LOSS.AML5$loss, LOSS.AML6$loss, LOSS.AML7$loss)

rm(am1, am2, am3, am4, am5, am6, am7, 
AML1, AML2, AML3, AML4, AML5, AML6, AML7,  
sdf1, sdf2, sdf3, sdf4, sdf5, sdf6, sdf7
, LOSS.AML1, LOSS.AML2, LOSS.AML3, LOSS.AML4, LOSS.AML5, LOSS.AML6, LOSS.AML7)


load('amelia_NLSY90_2.RData')
AML1= factorLevels(am1, df.base, cat1)
AML2= factorLevels(am2, df.base, cat1)


LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
LOSS.AML2 = loss(AML2,df.base,sdf2,cat1,ord1, real1 )


l.aml90 = c(l.aml90, LOSS.AML1$loss, LOSS.AML2$loss)


rm(am1, am2, AML1, AML2, sdf1, sdf2, LOSS.AML1, LOSS.AML2)



load('amelia_NLSY90 (1).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY90 (2).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY90 (3).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY90 (4).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY90 (5).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY90 (6).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY90 (7).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY90 (8).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY90 (9).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

load('amelia_NLSY90 (10).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)


load('amelia_NLSY90 (11).RData')
AML1= factorLevels(am1, df.base, cat1)
LOSS.AML1 = loss(AML1,df.base,sdf1,cat1,ord1, real1 )
l.aml90 = c(l.aml90, LOSS.AML1$loss)
rm(am1, sdf1, LOSS.AML1, AML1)

save(l.aml10, l.aml20, l.aml30, l.aml40, l.aml50, l.aml60, l.aml70, l.aml80, l.aml90, file = "amelia_consistency.RData")




