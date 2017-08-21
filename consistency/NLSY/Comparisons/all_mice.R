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

df.cv = read.csv("NLSY10.csv")[,-1] #df with CV indices removed

df.base = read.csv("NLSYcleaned.csv")[,-1] #df without CV indices removed
cat1 = read.csv("NLSYcategoricals.csv")[, -1]
ord1 = read.csv("NLSYordinals.csv")[, -1]
real1 = which(!is.element(seq(1: dim(df.base)[2]),c(ord1, cat1)))
l.mice10 = numeric()
l.mice20 = numeric()
l.mice30 = numeric()
l.mice40 = numeric()
l.mice50 = numeric()
l.mice60 = numeric()
l.mice70 = numeric()
l.mice80 = numeric()
l.mice90 = numeric()


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/NLSY/MICE")


###### 10 

load('MICE_NLSY10_1.RData')
load('MICE_NLSY10_2.RData')
load('MICE_NLSY10_3.RData')
load('MICE_NLSY10_4.RData')
load('MICE_NLSY10_5.RData')
load('MICE_NLSY10_6.RData')
load('MICE_NLSY10_7.RData')
load('MICE_NLSY10_8.RData')
load('MICE_NLSY10_9.RData')
load('MICE_NLSY10_10.RData')


colna = function(df){
	natrack = numeric()
	for (i in 1: dim(df)[2]){
	natrack = c(natrack,sum(is.na(df[,i])/ length(df[,i])))
	}
	return(natrack)}
	
	
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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )




l.mice10 = c(l.mice10, LOSS.MICE1$loss)
l.mice10 = c(l.mice10, LOSS.MICE2$loss)
l.mice10 = c(l.mice10, LOSS.MICE3$loss)
l.mice10 = c(l.mice10, LOSS.MICE4$loss)
l.mice10 = c(l.mice10, LOSS.MICE5$loss)
l.mice10 = c(l.mice10, LOSS.MICE6$loss)
l.mice10 = c(l.mice10, LOSS.MICE7$loss)
l.mice10 = c(l.mice10, LOSS.MICE8$loss)
l.mice10 = c(l.mice10, LOSS.MICE9$loss)
l.mice10 = c(l.mice10, LOSS.MICE10$loss)


rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


load('MICE_NLSY10_11.RData')
load('MICE_NLSY10_12.RData')
load('MICE_NLSY10_13.RData')
load('MICE_NLSY10_14.RData')
load('MICE_NLSY10_15.RData')
load('MICE_NLSY10_16.RData')
load('MICE_NLSY10_17.RData')
load('MICE_NLSY10_18.RData')
load('MICE_NLSY10_19.RData')
load('MICE_NLSY10_20.RData')



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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice10 = c(l.mice10, LOSS.MICE1$loss)
l.mice10 = c(l.mice10, LOSS.MICE2$loss)
l.mice10 = c(l.mice10, LOSS.MICE3$loss)
l.mice10 = c(l.mice10, LOSS.MICE4$loss)
l.mice10 = c(l.mice10, LOSS.MICE5$loss)
l.mice10 = c(l.mice10, LOSS.MICE6$loss)
l.mice10 = c(l.mice10, LOSS.MICE7$loss)
l.mice10 = c(l.mice10, LOSS.MICE8$loss)
l.mice10 = c(l.mice10, LOSS.MICE9$loss)
l.mice10 = c(l.mice10, LOSS.MICE10$loss)

rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


###### 20 

load('MICE_NLSY20_1.RData')
load('MICE_NLSY20_2.RData')
load('MICE_NLSY20_3.RData')
load('MICE_NLSY20_4.RData')
load('MICE_NLSY20_5.RData')
load('MICE_NLSY20_6.RData')
load('MICE_NLSY20_7.RData')
load('MICE_NLSY20_8.RData')
load('MICE_NLSY20_9.RData')
load('MICE_NLSY20_10.RData')


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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice20 = c(l.mice20, LOSS.MICE1$loss)
l.mice20 = c(l.mice20, LOSS.MICE2$loss)
l.mice20 = c(l.mice20, LOSS.MICE3$loss)
l.mice20 = c(l.mice20, LOSS.MICE4$loss)
l.mice20 = c(l.mice20, LOSS.MICE5$loss)
l.mice20 = c(l.mice20, LOSS.MICE6$loss)
l.mice20 = c(l.mice20, LOSS.MICE7$loss)
l.mice20 = c(l.mice20, LOSS.MICE8$loss)
l.mice20 = c(l.mice20, LOSS.MICE9$loss)
l.mice20 = c(l.mice20, LOSS.MICE10$loss)


rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


load('MICE_NLSY20_11.RData')
load('MICE_NLSY20_12.RData')
load('MICE_NLSY20_13.RData')
load('MICE_NLSY20_14.RData')
load('MICE_NLSY20_15.RData')
load('MICE_NLSY20_16.RData')
load('MICE_NLSY20_17.RData')
load('MICE_NLSY20_18.RData')
load('MICE_NLSY20_19.RData')
load('MICE_NLSY20_20.RData')



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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice20 = c(l.mice20, LOSS.MICE1$loss)
l.mice20 = c(l.mice20, LOSS.MICE2$loss)
l.mice20 = c(l.mice20, LOSS.MICE3$loss)
l.mice20 = c(l.mice20, LOSS.MICE4$loss)
l.mice20 = c(l.mice20, LOSS.MICE5$loss)
l.mice20 = c(l.mice20, LOSS.MICE6$loss)
l.mice20 = c(l.mice20, LOSS.MICE7$loss)
l.mice20 = c(l.mice20, LOSS.MICE8$loss)
l.mice20 = c(l.mice20, LOSS.MICE9$loss)
l.mice20 = c(l.mice20, LOSS.MICE10$loss)

rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


###### 30 

load('MICE_NLSY30_1.RData')
load('MICE_NLSY30_2.RData')
load('MICE_NLSY30_3.RData')
load('MICE_NLSY30_4.RData')
load('MICE_NLSY30_5.RData')
load('MICE_NLSY30_6.RData')
load('MICE_NLSY30_7.RData')
load('MICE_NLSY30_8.RData')
load('MICE_NLSY30_9.RData')
load('MICE_NLSY30_10.RData')


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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice30 = c(l.mice30, LOSS.MICE1$loss)
l.mice30 = c(l.mice30, LOSS.MICE2$loss)
l.mice30 = c(l.mice30, LOSS.MICE3$loss)
l.mice30 = c(l.mice30, LOSS.MICE4$loss)
l.mice30 = c(l.mice30, LOSS.MICE5$loss)
l.mice30 = c(l.mice30, LOSS.MICE6$loss)
l.mice30 = c(l.mice30, LOSS.MICE7$loss)
l.mice30 = c(l.mice30, LOSS.MICE8$loss)
l.mice30 = c(l.mice30, LOSS.MICE9$loss)
l.mice30 = c(l.mice30, LOSS.MICE10$loss)


rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


load('MICE_NLSY30_11.RData')
load('MICE_NLSY30_12.RData')
load('MICE_NLSY30_13.RData')
load('MICE_NLSY30_14.RData')
load('MICE_NLSY30_15.RData')
load('MICE_NLSY30_16.RData')
load('MICE_NLSY30_17.RData')
load('MICE_NLSY30_18.RData')
load('MICE_NLSY30_19.RData')
load('MICE_NLSY30_20.RData')



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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice30 = c(l.mice30, LOSS.MICE1$loss)
l.mice30 = c(l.mice30, LOSS.MICE2$loss)
l.mice30 = c(l.mice30, LOSS.MICE3$loss)
l.mice30 = c(l.mice30, LOSS.MICE4$loss)
l.mice30 = c(l.mice30, LOSS.MICE5$loss)
l.mice30 = c(l.mice30, LOSS.MICE6$loss)
l.mice30 = c(l.mice30, LOSS.MICE7$loss)
l.mice30 = c(l.mice30, LOSS.MICE8$loss)
l.mice30 = c(l.mice30, LOSS.MICE9$loss)
l.mice30 = c(l.mice30, LOSS.MICE10$loss)

rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )



###### 40 

load('MICE_NLSY40_1.RData')
load('MICE_NLSY40_2.RData')
load('MICE_NLSY40_3.RData')
load('MICE_NLSY40_4.RData')
load('MICE_NLSY40_5.RData')
load('MICE_NLSY40_6.RData')
load('MICE_NLSY40_7.RData')
load('MICE_NLSY40_8.RData')
load('MICE_NLSY40_9.RData')
load('MICE_NLSY40_10.RData')


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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice40 = c(l.mice40, LOSS.MICE1$loss)
l.mice40 = c(l.mice40, LOSS.MICE2$loss)
l.mice40 = c(l.mice40, LOSS.MICE3$loss)
l.mice40 = c(l.mice40, LOSS.MICE4$loss)
l.mice40 = c(l.mice40, LOSS.MICE5$loss)
l.mice40 = c(l.mice40, LOSS.MICE6$loss)
l.mice40 = c(l.mice40, LOSS.MICE7$loss)
l.mice40 = c(l.mice40, LOSS.MICE8$loss)
l.mice40 = c(l.mice40, LOSS.MICE9$loss)
l.mice40 = c(l.mice40, LOSS.MICE10$loss)


rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


load('MICE_NLSY40_11.RData')
load('MICE_NLSY40_12.RData')
load('MICE_NLSY40_13.RData')
load('MICE_NLSY40_14.RData')
load('MICE_NLSY40_15.RData')
load('MICE_NLSY40_16.RData')
load('MICE_NLSY40_17.RData')
load('MICE_NLSY40_18.RData')
load('MICE_NLSY40_19.RData')
load('MICE_NLSY40_20.RData')



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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice40 = c(l.mice40, LOSS.MICE1$loss)
l.mice40 = c(l.mice40, LOSS.MICE2$loss)
l.mice40 = c(l.mice40, LOSS.MICE3$loss)
l.mice40 = c(l.mice40, LOSS.MICE4$loss)
l.mice40 = c(l.mice40, LOSS.MICE5$loss)
l.mice40 = c(l.mice40, LOSS.MICE6$loss)
l.mice40 = c(l.mice40, LOSS.MICE7$loss)
l.mice40 = c(l.mice40, LOSS.MICE8$loss)
l.mice40 = c(l.mice40, LOSS.MICE9$loss)
l.mice40 = c(l.mice40, LOSS.MICE10$loss)

rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )




###### 50 

load('MICE_NLSY50_1.RData')
load('MICE_NLSY50_2.RData')
load('MICE_NLSY50_3.RData')
load('MICE_NLSY50_4.RData')
load('MICE_NLSY50_5.RData')
load('MICE_NLSY50_6.RData')
load('MICE_NLSY50_7.RData')
load('MICE_NLSY50_8.RData')
load('MICE_NLSY50_9.RData')
load('MICE_NLSY50_10.RData')


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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice50 = c(l.mice50, LOSS.MICE1$loss)
l.mice50 = c(l.mice50, LOSS.MICE2$loss)
l.mice50 = c(l.mice50, LOSS.MICE3$loss)
l.mice50 = c(l.mice50, LOSS.MICE4$loss)
l.mice50 = c(l.mice50, LOSS.MICE5$loss)
l.mice50 = c(l.mice50, LOSS.MICE6$loss)
l.mice50 = c(l.mice50, LOSS.MICE7$loss)
l.mice50 = c(l.mice50, LOSS.MICE8$loss)
l.mice50 = c(l.mice50, LOSS.MICE9$loss)
l.mice50 = c(l.mice50, LOSS.MICE10$loss)


rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


load('MICE_NLSY50_11.RData')
load('MICE_NLSY50_12.RData')
load('MICE_NLSY50_13.RData')
load('MICE_NLSY50_14.RData')
load('MICE_NLSY50_15.RData')
load('MICE_NLSY50_16.RData')
load('MICE_NLSY50_17.RData')
load('MICE_NLSY50_18.RData')
load('MICE_NLSY50_19.RData')
load('MICE_NLSY50_20.RData')



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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice50 = c(l.mice50, LOSS.MICE1$loss)
l.mice50 = c(l.mice50, LOSS.MICE2$loss)
l.mice50 = c(l.mice50, LOSS.MICE3$loss)
l.mice50 = c(l.mice50, LOSS.MICE4$loss)
l.mice50 = c(l.mice50, LOSS.MICE5$loss)
l.mice50 = c(l.mice50, LOSS.MICE6$loss)
l.mice50 = c(l.mice50, LOSS.MICE7$loss)
l.mice50 = c(l.mice50, LOSS.MICE8$loss)
l.mice50 = c(l.mice50, LOSS.MICE9$loss)
l.mice50 = c(l.mice50, LOSS.MICE10$loss)

rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )



###### 60 

load('MICE_NLSY60_1.RData')
load('MICE_NLSY60_2.RData')
load('MICE_NLSY60_3.RData')
load('MICE_NLSY60_4.RData')
load('MICE_NLSY60_5.RData')
load('MICE_NLSY60_6.RData')
load('MICE_NLSY60_7.RData')
load('MICE_NLSY60_8.RData')
load('MICE_NLSY60_9.RData')
load('MICE_NLSY60_10.RData')


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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice60 = c(l.mice60, LOSS.MICE1$loss)
l.mice60 = c(l.mice60, LOSS.MICE2$loss)
l.mice60 = c(l.mice60, LOSS.MICE3$loss)
l.mice60 = c(l.mice60, LOSS.MICE4$loss)
l.mice60 = c(l.mice60, LOSS.MICE5$loss)
l.mice60 = c(l.mice60, LOSS.MICE6$loss)
l.mice60 = c(l.mice60, LOSS.MICE7$loss)
l.mice60 = c(l.mice60, LOSS.MICE8$loss)
l.mice60 = c(l.mice60, LOSS.MICE9$loss)
l.mice60 = c(l.mice60, LOSS.MICE10$loss)


rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


load('MICE_NLSY60_11.RData')
load('MICE_NLSY60_12.RData')
load('MICE_NLSY60_13.RData')
load('MICE_NLSY60_14.RData')
load('MICE_NLSY60_15.RData')
load('MICE_NLSY60_16.RData')
load('MICE_NLSY60_17.RData')
load('MICE_NLSY60_18.RData')
load('MICE_NLSY60_19.RData')
load('MICE_NLSY60_20.RData')



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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice60 = c(l.mice60, LOSS.MICE1$loss)
l.mice60 = c(l.mice60, LOSS.MICE2$loss)
l.mice60 = c(l.mice60, LOSS.MICE3$loss)
l.mice60 = c(l.mice60, LOSS.MICE4$loss)
l.mice60 = c(l.mice60, LOSS.MICE5$loss)
l.mice60 = c(l.mice60, LOSS.MICE6$loss)
l.mice60 = c(l.mice60, LOSS.MICE7$loss)
l.mice60 = c(l.mice60, LOSS.MICE8$loss)
l.mice60 = c(l.mice60, LOSS.MICE9$loss)
l.mice60 = c(l.mice60, LOSS.MICE10$loss)

rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )



###### 70 

load('MICE_NLSY70_1.RData')
load('MICE_NLSY70_2.RData')
load('MICE_NLSY70_3.RData')
load('MICE_NLSY70_4.RData')
load('MICE_NLSY70_5.RData')
load('MICE_NLSY70_6.RData')
load('MICE_NLSY70_7.RData')
load('MICE_NLSY70_8.RData')
load('MICE_NLSY70_9.RData')
load('MICE_NLSY70_10.RData')


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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice70 = c(l.mice70, LOSS.MICE1$loss)
l.mice70 = c(l.mice70, LOSS.MICE2$loss)
l.mice70 = c(l.mice70, LOSS.MICE3$loss)
l.mice70 = c(l.mice70, LOSS.MICE4$loss)
l.mice70 = c(l.mice70, LOSS.MICE5$loss)
l.mice70 = c(l.mice70, LOSS.MICE6$loss)
l.mice70 = c(l.mice70, LOSS.MICE7$loss)
l.mice70 = c(l.mice70, LOSS.MICE8$loss)
l.mice70 = c(l.mice70, LOSS.MICE9$loss)
l.mice70 = c(l.mice70, LOSS.MICE10$loss)


rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


load('MICE_NLSY70_11.RData')
load('MICE_NLSY70_12.RData')
load('MICE_NLSY70_13.RData')
load('MICE_NLSY70_14.RData')
load('MICE_NLSY70_15.RData')
load('MICE_NLSY70_16.RData')
load('MICE_NLSY70_17.RData')
load('MICE_NLSY70_18.RData')
load('MICE_NLSY70_19.RData')
load('MICE_NLSY70_20.RData')



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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice70 = c(l.mice70, LOSS.MICE1$loss)
l.mice70 = c(l.mice70, LOSS.MICE2$loss)
l.mice70 = c(l.mice70, LOSS.MICE3$loss)
l.mice70 = c(l.mice70, LOSS.MICE4$loss)
l.mice70 = c(l.mice70, LOSS.MICE5$loss)
l.mice70 = c(l.mice70, LOSS.MICE6$loss)
l.mice70 = c(l.mice70, LOSS.MICE7$loss)
l.mice70 = c(l.mice70, LOSS.MICE8$loss)
l.mice70 = c(l.mice70, LOSS.MICE9$loss)
l.mice70 = c(l.mice70, LOSS.MICE10$loss)

rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )



###### 80 

load('MICE_NLSY80_1.RData')
load('MICE_NLSY80_2.RData')
load('MICE_NLSY80_3.RData')
load('MICE_NLSY80_4.RData')
load('MICE_NLSY80_5.RData')
load('MICE_NLSY80_6.RData')
load('MICE_NLSY80_7.RData')
load('MICE_NLSY80_8.RData')
load('MICE_NLSY80_9.RData')
load('MICE_NLSY80_10.RData')


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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice80 = c(l.mice80, LOSS.MICE1$loss)
l.mice80 = c(l.mice80, LOSS.MICE2$loss)
l.mice80 = c(l.mice80, LOSS.MICE3$loss)
l.mice80 = c(l.mice80, LOSS.MICE4$loss)
l.mice80 = c(l.mice80, LOSS.MICE5$loss)
l.mice80 = c(l.mice80, LOSS.MICE6$loss)
l.mice80 = c(l.mice80, LOSS.MICE7$loss)
l.mice80 = c(l.mice80, LOSS.MICE8$loss)
l.mice80 = c(l.mice80, LOSS.MICE9$loss)
l.mice80 = c(l.mice80, LOSS.MICE10$loss)


rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


load('MICE_NLSY80_11.RData')
load('MICE_NLSY80_12.RData')
load('MICE_NLSY80_13.RData')
load('MICE_NLSY80_14.RData')
load('MICE_NLSY80_15.RData')
load('MICE_NLSY80_16.RData')
load('MICE_NLSY80_17.RData')
load('MICE_NLSY80_18.RData')
load('MICE_NLSY80_19.RData')
load('MICE_NLSY80_20.RData')



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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice80 = c(l.mice80, LOSS.MICE1$loss)
l.mice80 = c(l.mice80, LOSS.MICE2$loss)
l.mice80 = c(l.mice80, LOSS.MICE3$loss)
l.mice80 = c(l.mice80, LOSS.MICE4$loss)
l.mice80 = c(l.mice80, LOSS.MICE5$loss)
l.mice80 = c(l.mice80, LOSS.MICE6$loss)
l.mice80 = c(l.mice80, LOSS.MICE7$loss)
l.mice80 = c(l.mice80, LOSS.MICE8$loss)
l.mice80 = c(l.mice80, LOSS.MICE9$loss)
l.mice80 = c(l.mice80, LOSS.MICE10$loss)

rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )




###### 90 

load('MICE_NLSY90_1.RData')
load('MICE_NLSY90_2.RData')
load('MICE_NLSY90_3.RData')
load('MICE_NLSY90_4.RData')
load('MICE_NLSY90_5.RData')
load('MICE_NLSY90_6.RData')
load('MICE_NLSY90_7.RData')
load('MICE_NLSY90_8.RData')
load('MICE_NLSY90_9.RData')
load('MICE_NLSY90_10.RData')


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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice90 = c(l.mice90, LOSS.MICE1$loss)
l.mice90 = c(l.mice90, LOSS.MICE2$loss)
l.mice90 = c(l.mice90, LOSS.MICE3$loss)
l.mice90 = c(l.mice90, LOSS.MICE4$loss)
l.mice90 = c(l.mice90, LOSS.MICE5$loss)
l.mice90 = c(l.mice90, LOSS.MICE6$loss)
l.mice90 = c(l.mice90, LOSS.MICE7$loss)
l.mice90 = c(l.mice90, LOSS.MICE8$loss)
l.mice90 = c(l.mice90, LOSS.MICE9$loss)
l.mice90 = c(l.mice90, LOSS.MICE10$loss)


rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )


load('MICE_NLSY90_11.RData')
load('MICE_NLSY90_12.RData')
load('MICE_NLSY90_13.RData')
load('MICE_NLSY90_14.RData')
load('MICE_NLSY90_15.RData')
load('MICE_NLSY90_16.RData')
load('MICE_NLSY90_17.RData')
load('MICE_NLSY90_18.RData')
load('MICE_NLSY90_19.RData')
load('MICE_NLSY90_20.RData')



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


LOSS.MICE1 = loss(MICE1,df.base,sdf1 ,cat1,ord1 , real1 )
LOSS.MICE2 = loss(MICE2,df.base,sdf2 ,cat1,ord1 , real1 )
LOSS.MICE3 = loss(MICE3,df.base,sdf3 ,cat1,ord1 , real1 )
LOSS.MICE4 = loss(MICE4,df.base,sdf4 ,cat1,ord1 , real1 )
LOSS.MICE5 = loss(MICE5,df.base,sdf5 ,cat1,ord1 , real1 )
LOSS.MICE6 = loss(MICE6,df.base,sdf6 ,cat1,ord1 , real1 )
LOSS.MICE7 = loss(MICE7,df.base,sdf7 ,cat1,ord1 , real1 )
LOSS.MICE8 = loss(MICE8,df.base,sdf8 ,cat1,ord1 , real1 )
LOSS.MICE9 = loss(MICE9,df.base,sdf9 ,cat1,ord1 , real1 )
LOSS.MICE10 = loss(MICE10,df.base,sdf10 ,cat1,ord1 , real1 )

l.mice90 = c(l.mice90, LOSS.MICE1$loss)
l.mice90 = c(l.mice90, LOSS.MICE2$loss)
l.mice90 = c(l.mice90, LOSS.MICE3$loss)
l.mice90 = c(l.mice90, LOSS.MICE4$loss)
l.mice90 = c(l.mice90, LOSS.MICE5$loss)
l.mice90 = c(l.mice90, LOSS.MICE6$loss)
l.mice90 = c(l.mice90, LOSS.MICE7$loss)
l.mice90 = c(l.mice90, LOSS.MICE8$loss)
l.mice90 = c(l.mice90, LOSS.MICE9$loss)
l.mice90 = c(l.mice90, LOSS.MICE10$loss)

rm(MICE1, MICE2, MICE3, MICE4, MICE5, MICE6, MICE7, MICE8, MICE9, MICE10, LOSS.MICE1, LOSS.MICE2, LOSS.MICE3, LOSS.MICE4, LOSS.MICE5, LOSS.MICE6, LOSS.MICE7, LOSS.MICE8, LOSS.MICE9, LOSS.MICE10 )






save(l.mice10, l.mice20, l.mice30, l.mice40, l.mice50, l.mice60, l.mice70, l.mice80, l.mice90,  file = "mice_consistency.RData")





























