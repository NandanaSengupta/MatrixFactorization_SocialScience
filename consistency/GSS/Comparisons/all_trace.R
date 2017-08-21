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
l.tr10 = numeric()
l.tr20 = numeric()
l.tr30 = numeric()
l.tr40 = numeric()
l.tr50 = numeric()
l.tr60 = numeric()
l.tr70 = numeric()
l.tr80 = numeric()
l.tr90 = numeric()

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/GSS/TRACE")


######## 10

glrm1 =  read.csv("Trace_GSS10_MI.csv")
glrm2 =  read.csv("Trace_GSS10_MI (1).csv")
glrm3 =  read.csv("Trace_GSS10_MI (2).csv")
glrm4 =  read.csv("Trace_GSS10_MI (3).csv")
glrm5 =  read.csv("Trace_GSS10_MI (4).csv")
glrm6 =  read.csv("Trace_GSS10_MI (5).csv")
glrm7 =  read.csv("Trace_GSS10_MI (6).csv")
glrm8 =  read.csv("Trace_GSS10_MI (7).csv")
glrm9 =  read.csv("Trace_GSS10_MI (8).csv")
glrm10 = read.csv("Trace_GSS10_MI (9).csv")
glrm11 = read.csv("Trace_GSS10_MI (10).csv")
glrm12 = read.csv("Trace_GSS10_MI (11).csv")
glrm13 = read.csv("Trace_GSS10_MI (12).csv")
glrm14 = read.csv("Trace_GSS10_MI (13).csv")
glrm15 = read.csv("Trace_GSS10_MI (14).csv")
glrm16 = read.csv("Trace_GSS10_MI (15).csv")
glrm17 = read.csv("Trace_GSS10_MI (16).csv")
glrm18 = read.csv("Trace_GSS10_MI (17).csv")
glrm19 = read.csv("Trace_GSS10_MI (18).csv")
glrm20 = read.csv("Trace_GSS10_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Trace_s10.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Trace_s10 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Trace_s10 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Trace_s10 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Trace_s10 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Trace_s10 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Trace_s10 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Trace_s10 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Trace_s10 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Trace_s10 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Trace_s10 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Trace_s10 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Trace_s10 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Trace_s10 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Trace_s10 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Trace_s10 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Trace_s10 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Trace_s10 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Trace_s10 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Trace_s10 (19).csv", header = FALSE)))


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

LOSS.TR1  = loss(GLRM_TR1,df.base,sdf1,cat1,ord1, real1 )
LOSS.TR2  = loss(GLRM_TR2,df.base,sdf2,cat1,ord1, real1 )
LOSS.TR3  = loss(GLRM_TR3,df.base,sdf3,cat1,ord1, real1 )
LOSS.TR4  = loss(GLRM_TR4,df.base,sdf4,cat1,ord1, real1 )
LOSS.TR5  = loss(GLRM_TR5,df.base,sdf5,cat1,ord1, real1 )
LOSS.TR6  = loss(GLRM_TR6,df.base,sdf6,cat1,ord1, real1 )
LOSS.TR7  = loss(GLRM_TR7,df.base,sdf7,cat1,ord1, real1 )
LOSS.TR8  = loss(GLRM_TR8,df.base,sdf8,cat1,ord1, real1 )
LOSS.TR9  = loss(GLRM_TR9,df.base,sdf9,cat1,ord1, real1 )
LOSS.TR10  = loss(GLRM_TR10,df.base,sdf10,cat1,ord1, real1 )
LOSS.TR11  = loss(GLRM_TR11,df.base,sdf11,cat1,ord1, real1 )
LOSS.TR12  = loss(GLRM_TR12,df.base,sdf12,cat1,ord1, real1 )
LOSS.TR13  = loss(GLRM_TR13,df.base,sdf13,cat1,ord1, real1 )
LOSS.TR14  = loss(GLRM_TR14,df.base,sdf14,cat1,ord1, real1 )
LOSS.TR15  = loss(GLRM_TR15,df.base,sdf15,cat1,ord1, real1 )
LOSS.TR16  = loss(GLRM_TR16,df.base,sdf16,cat1,ord1, real1 )
LOSS.TR17  = loss(GLRM_TR17,df.base,sdf17,cat1,ord1, real1 )
LOSS.TR18  = loss(GLRM_TR18,df.base,sdf18,cat1,ord1, real1 )
LOSS.TR19  = loss(GLRM_TR19,df.base,sdf19,cat1,ord1, real1 )
LOSS.TR20  = loss(GLRM_TR20,df.base,sdf20,cat1,ord1, real1 )


l.tr10 = c(l.tr10, LOSS.TR1$loss)
l.tr10 = c(l.tr10, LOSS.TR2$loss)
l.tr10 = c(l.tr10, LOSS.TR3$loss)
l.tr10 = c(l.tr10, LOSS.TR4$loss)
l.tr10 = c(l.tr10, LOSS.TR5$loss)
l.tr10 = c(l.tr10, LOSS.TR6$loss)
l.tr10 = c(l.tr10, LOSS.TR7$loss)
l.tr10 = c(l.tr10, LOSS.TR8$loss)
l.tr10 = c(l.tr10, LOSS.TR9$loss)
l.tr10 = c(l.tr10, LOSS.TR10$loss)
l.tr10 = c(l.tr10, LOSS.TR11$loss)
l.tr10 = c(l.tr10, LOSS.TR12$loss)
l.tr10 = c(l.tr10, LOSS.TR13$loss)
l.tr10 = c(l.tr10, LOSS.TR14$loss)
l.tr10 = c(l.tr10, LOSS.TR15$loss)
l.tr10 = c(l.tr10, LOSS.TR16$loss)
l.tr10 = c(l.tr10, LOSS.TR17$loss)
l.tr10 = c(l.tr10, LOSS.TR18$loss)
l.tr10 = c(l.tr10, LOSS.TR19$loss)
l.tr10 = c(l.tr10, LOSS.TR20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_TR1, GLRM_TR2, GLRM_TR3, GLRM_TR4, GLRM_TR5,  GLRM_TR6, GLRM_TR7, GLRM_TR8, GLRM_TR9, GLRM_TR10, 
GLRM_TR11, GLRM_TR12, GLRM_TR13, GLRM_TR14, GLRM_TR15,  GLRM_TR16, GLRM_TR17, GLRM_TR18, GLRM_TR19, GLRM_TR20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.TR1, LOSS.TR2, LOSS.TR3, LOSS.TR4, LOSS.TR5,  LOSS.TR6, LOSS.TR7, LOSS.TR8, LOSS.TR9, LOSS.TR10, 
LOSS.TR11, LOSS.TR12, LOSS.TR13, LOSS.TR14, LOSS.TR15,  LOSS.TR16, LOSS.TR17, LOSS.TR18, LOSS.TR19, LOSS.TR20 )


######## 20

glrm1 =  read.csv("Trace_GSS20_MI.csv")
glrm2 =  read.csv("Trace_GSS20_MI (1).csv")
glrm3 =  read.csv("Trace_GSS20_MI (2).csv")
glrm4 =  read.csv("Trace_GSS20_MI (3).csv")
glrm5 =  read.csv("Trace_GSS20_MI (4).csv")
glrm6 =  read.csv("Trace_GSS20_MI (5).csv")
glrm7 =  read.csv("Trace_GSS20_MI (6).csv")
glrm8 =  read.csv("Trace_GSS20_MI (7).csv")
glrm9 =  read.csv("Trace_GSS20_MI (8).csv")
glrm10 = read.csv("Trace_GSS20_MI (9).csv")
glrm11 = read.csv("Trace_GSS20_MI (10).csv")
glrm12 = read.csv("Trace_GSS20_MI (11).csv")
glrm13 = read.csv("Trace_GSS20_MI (12).csv")
glrm14 = read.csv("Trace_GSS20_MI (13).csv")
glrm15 = read.csv("Trace_GSS20_MI (14).csv")
glrm16 = read.csv("Trace_GSS20_MI (15).csv")
glrm17 = read.csv("Trace_GSS20_MI (16).csv")
glrm18 = read.csv("Trace_GSS20_MI (17).csv")
glrm19 = read.csv("Trace_GSS20_MI (18).csv")
glrm20 = read.csv("Trace_GSS20_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Trace_s20.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Trace_s20 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Trace_s20 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Trace_s20 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Trace_s20 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Trace_s20 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Trace_s20 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Trace_s20 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Trace_s20 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Trace_s20 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Trace_s20 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Trace_s20 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Trace_s20 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Trace_s20 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Trace_s20 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Trace_s20 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Trace_s20 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Trace_s20 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Trace_s20 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Trace_s20 (19).csv", header = FALSE)))


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

LOSS.TR1  = loss(GLRM_TR1,df.base,sdf1,cat1,ord1, real1 )
LOSS.TR2  = loss(GLRM_TR2,df.base,sdf2,cat1,ord1, real1 )
LOSS.TR3  = loss(GLRM_TR3,df.base,sdf3,cat1,ord1, real1 )
LOSS.TR4  = loss(GLRM_TR4,df.base,sdf4,cat1,ord1, real1 )
LOSS.TR5  = loss(GLRM_TR5,df.base,sdf5,cat1,ord1, real1 )
LOSS.TR6  = loss(GLRM_TR6,df.base,sdf6,cat1,ord1, real1 )
LOSS.TR7  = loss(GLRM_TR7,df.base,sdf7,cat1,ord1, real1 )
LOSS.TR8  = loss(GLRM_TR8,df.base,sdf8,cat1,ord1, real1 )
LOSS.TR9  = loss(GLRM_TR9,df.base,sdf9,cat1,ord1, real1 )
LOSS.TR10  = loss(GLRM_TR10,df.base,sdf10,cat1,ord1, real1 )
LOSS.TR11  = loss(GLRM_TR11,df.base,sdf11,cat1,ord1, real1 )
LOSS.TR12  = loss(GLRM_TR12,df.base,sdf12,cat1,ord1, real1 )
LOSS.TR13  = loss(GLRM_TR13,df.base,sdf13,cat1,ord1, real1 )
LOSS.TR14  = loss(GLRM_TR14,df.base,sdf14,cat1,ord1, real1 )
LOSS.TR15  = loss(GLRM_TR15,df.base,sdf15,cat1,ord1, real1 )
LOSS.TR16  = loss(GLRM_TR16,df.base,sdf16,cat1,ord1, real1 )
LOSS.TR17  = loss(GLRM_TR17,df.base,sdf17,cat1,ord1, real1 )
LOSS.TR18  = loss(GLRM_TR18,df.base,sdf18,cat1,ord1, real1 )
LOSS.TR19  = loss(GLRM_TR19,df.base,sdf19,cat1,ord1, real1 )
LOSS.TR20  = loss(GLRM_TR20,df.base,sdf20,cat1,ord1, real1 )


l.tr20 = c(l.tr20, LOSS.TR1$loss)
l.tr20 = c(l.tr20, LOSS.TR2$loss)
l.tr20 = c(l.tr20, LOSS.TR3$loss)
l.tr20 = c(l.tr20, LOSS.TR4$loss)
l.tr20 = c(l.tr20, LOSS.TR5$loss)
l.tr20 = c(l.tr20, LOSS.TR6$loss)
l.tr20 = c(l.tr20, LOSS.TR7$loss)
l.tr20 = c(l.tr20, LOSS.TR8$loss)
l.tr20 = c(l.tr20, LOSS.TR9$loss)
l.tr20 = c(l.tr20, LOSS.TR10$loss)
l.tr20 = c(l.tr20, LOSS.TR11$loss)
l.tr20 = c(l.tr20, LOSS.TR12$loss)
l.tr20 = c(l.tr20, LOSS.TR13$loss)
l.tr20 = c(l.tr20, LOSS.TR14$loss)
l.tr20 = c(l.tr20, LOSS.TR15$loss)
l.tr20 = c(l.tr20, LOSS.TR16$loss)
l.tr20 = c(l.tr20, LOSS.TR17$loss)
l.tr20 = c(l.tr20, LOSS.TR18$loss)
l.tr20 = c(l.tr20, LOSS.TR19$loss)
l.tr20 = c(l.tr20, LOSS.TR20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_TR1, GLRM_TR2, GLRM_TR3, GLRM_TR4, GLRM_TR5,  GLRM_TR6, GLRM_TR7, GLRM_TR8, GLRM_TR9, GLRM_TR10, 
GLRM_TR11, GLRM_TR12, GLRM_TR13, GLRM_TR14, GLRM_TR15,  GLRM_TR16, GLRM_TR17, GLRM_TR18, GLRM_TR19, GLRM_TR20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.TR1, LOSS.TR2, LOSS.TR3, LOSS.TR4, LOSS.TR5,  LOSS.TR6, LOSS.TR7, LOSS.TR8, LOSS.TR9, LOSS.TR10, 
LOSS.TR11, LOSS.TR12, LOSS.TR13, LOSS.TR14, LOSS.TR15,  LOSS.TR16, LOSS.TR17, LOSS.TR18, LOSS.TR19, LOSS.TR20 )


######## 30

glrm1 =  read.csv("Trace_GSS30_MI.csv")
glrm2 =  read.csv("Trace_GSS30_MI (1).csv")
glrm3 =  read.csv("Trace_GSS30_MI (2).csv")
glrm4 =  read.csv("Trace_GSS30_MI (3).csv")
glrm5 =  read.csv("Trace_GSS30_MI (4).csv")
glrm6 =  read.csv("Trace_GSS30_MI (5).csv")
glrm7 =  read.csv("Trace_GSS30_MI (6).csv")
glrm8 =  read.csv("Trace_GSS30_MI (7).csv")
glrm9 =  read.csv("Trace_GSS30_MI (8).csv")
glrm10 = read.csv("Trace_GSS30_MI (9).csv")
glrm11 = read.csv("Trace_GSS30_MI (10).csv")
glrm12 = read.csv("Trace_GSS30_MI (11).csv")
glrm13 = read.csv("Trace_GSS30_MI (12).csv")
glrm14 = read.csv("Trace_GSS30_MI (13).csv")
glrm15 = read.csv("Trace_GSS30_MI (14).csv")
glrm16 = read.csv("Trace_GSS30_MI (15).csv")
glrm17 = read.csv("Trace_GSS30_MI (16).csv")
glrm18 = read.csv("Trace_GSS30_MI (17).csv")
glrm19 = read.csv("Trace_GSS30_MI (18).csv")
glrm20 = read.csv("Trace_GSS30_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Trace_s30.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Trace_s30 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Trace_s30 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Trace_s30 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Trace_s30 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Trace_s30 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Trace_s30 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Trace_s30 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Trace_s30 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Trace_s30 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Trace_s30 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Trace_s30 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Trace_s30 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Trace_s30 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Trace_s30 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Trace_s30 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Trace_s30 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Trace_s30 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Trace_s30 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Trace_s30 (19).csv", header = FALSE)))


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

LOSS.TR1  = loss(GLRM_TR1,df.base,sdf1,cat1,ord1, real1 )
LOSS.TR2  = loss(GLRM_TR2,df.base,sdf2,cat1,ord1, real1 )
LOSS.TR3  = loss(GLRM_TR3,df.base,sdf3,cat1,ord1, real1 )
LOSS.TR4  = loss(GLRM_TR4,df.base,sdf4,cat1,ord1, real1 )
LOSS.TR5  = loss(GLRM_TR5,df.base,sdf5,cat1,ord1, real1 )
LOSS.TR6  = loss(GLRM_TR6,df.base,sdf6,cat1,ord1, real1 )
LOSS.TR7  = loss(GLRM_TR7,df.base,sdf7,cat1,ord1, real1 )
LOSS.TR8  = loss(GLRM_TR8,df.base,sdf8,cat1,ord1, real1 )
LOSS.TR9  = loss(GLRM_TR9,df.base,sdf9,cat1,ord1, real1 )
LOSS.TR10  = loss(GLRM_TR10,df.base,sdf10,cat1,ord1, real1 )
LOSS.TR11  = loss(GLRM_TR11,df.base,sdf11,cat1,ord1, real1 )
LOSS.TR12  = loss(GLRM_TR12,df.base,sdf12,cat1,ord1, real1 )
LOSS.TR13  = loss(GLRM_TR13,df.base,sdf13,cat1,ord1, real1 )
LOSS.TR14  = loss(GLRM_TR14,df.base,sdf14,cat1,ord1, real1 )
LOSS.TR15  = loss(GLRM_TR15,df.base,sdf15,cat1,ord1, real1 )
LOSS.TR16  = loss(GLRM_TR16,df.base,sdf16,cat1,ord1, real1 )
LOSS.TR17  = loss(GLRM_TR17,df.base,sdf17,cat1,ord1, real1 )
LOSS.TR18  = loss(GLRM_TR18,df.base,sdf18,cat1,ord1, real1 )
LOSS.TR19  = loss(GLRM_TR19,df.base,sdf19,cat1,ord1, real1 )
LOSS.TR20  = loss(GLRM_TR20,df.base,sdf20,cat1,ord1, real1 )


l.tr30 = c(l.tr30, LOSS.TR1$loss)
l.tr30 = c(l.tr30, LOSS.TR2$loss)
l.tr30 = c(l.tr30, LOSS.TR3$loss)
l.tr30 = c(l.tr30, LOSS.TR4$loss)
l.tr30 = c(l.tr30, LOSS.TR5$loss)
l.tr30 = c(l.tr30, LOSS.TR6$loss)
l.tr30 = c(l.tr30, LOSS.TR7$loss)
l.tr30 = c(l.tr30, LOSS.TR8$loss)
l.tr30 = c(l.tr30, LOSS.TR9$loss)
l.tr30 = c(l.tr30, LOSS.TR10$loss)
l.tr30 = c(l.tr30, LOSS.TR11$loss)
l.tr30 = c(l.tr30, LOSS.TR12$loss)
l.tr30 = c(l.tr30, LOSS.TR13$loss)
l.tr30 = c(l.tr30, LOSS.TR14$loss)
l.tr30 = c(l.tr30, LOSS.TR15$loss)
l.tr30 = c(l.tr30, LOSS.TR16$loss)
l.tr30 = c(l.tr30, LOSS.TR17$loss)
l.tr30 = c(l.tr30, LOSS.TR18$loss)
l.tr30 = c(l.tr30, LOSS.TR19$loss)
l.tr30 = c(l.tr30, LOSS.TR20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_TR1, GLRM_TR2, GLRM_TR3, GLRM_TR4, GLRM_TR5,  GLRM_TR6, GLRM_TR7, GLRM_TR8, GLRM_TR9, GLRM_TR10, 
GLRM_TR11, GLRM_TR12, GLRM_TR13, GLRM_TR14, GLRM_TR15,  GLRM_TR16, GLRM_TR17, GLRM_TR18, GLRM_TR19, GLRM_TR20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.TR1, LOSS.TR2, LOSS.TR3, LOSS.TR4, LOSS.TR5,  LOSS.TR6, LOSS.TR7, LOSS.TR8, LOSS.TR9, LOSS.TR10, 
LOSS.TR11, LOSS.TR12, LOSS.TR13, LOSS.TR14, LOSS.TR15,  LOSS.TR16, LOSS.TR17, LOSS.TR18, LOSS.TR19, LOSS.TR20 )



######## 40

glrm1 =  read.csv("Trace_GSS40_MI.csv")
glrm2 =  read.csv("Trace_GSS40_MI (1).csv")
glrm3 =  read.csv("Trace_GSS40_MI (2).csv")
glrm4 =  read.csv("Trace_GSS40_MI (3).csv")
glrm5 =  read.csv("Trace_GSS40_MI (4).csv")
glrm6 =  read.csv("Trace_GSS40_MI (5).csv")
glrm7 =  read.csv("Trace_GSS40_MI (6).csv")
glrm8 =  read.csv("Trace_GSS40_MI (7).csv")
glrm9 =  read.csv("Trace_GSS40_MI (8).csv")
glrm10 = read.csv("Trace_GSS40_MI (9).csv")
glrm11 = read.csv("Trace_GSS40_MI (10).csv")
glrm12 = read.csv("Trace_GSS40_MI (11).csv")
glrm13 = read.csv("Trace_GSS40_MI (12).csv")
glrm14 = read.csv("Trace_GSS40_MI (13).csv")
glrm15 = read.csv("Trace_GSS40_MI (14).csv")
glrm16 = read.csv("Trace_GSS40_MI (15).csv")
glrm17 = read.csv("Trace_GSS40_MI (16).csv")
glrm18 = read.csv("Trace_GSS40_MI (17).csv")
glrm19 = read.csv("Trace_GSS40_MI (18).csv")
glrm20 = read.csv("Trace_GSS40_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Trace_s40.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Trace_s40 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Trace_s40 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Trace_s40 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Trace_s40 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Trace_s40 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Trace_s40 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Trace_s40 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Trace_s40 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Trace_s40 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Trace_s40 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Trace_s40 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Trace_s40 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Trace_s40 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Trace_s40 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Trace_s40 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Trace_s40 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Trace_s40 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Trace_s40 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Trace_s40 (19).csv", header = FALSE)))


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

LOSS.TR1  = loss(GLRM_TR1,df.base,sdf1,cat1,ord1, real1 )
LOSS.TR2  = loss(GLRM_TR2,df.base,sdf2,cat1,ord1, real1 )
LOSS.TR3  = loss(GLRM_TR3,df.base,sdf3,cat1,ord1, real1 )
LOSS.TR4  = loss(GLRM_TR4,df.base,sdf4,cat1,ord1, real1 )
LOSS.TR5  = loss(GLRM_TR5,df.base,sdf5,cat1,ord1, real1 )
LOSS.TR6  = loss(GLRM_TR6,df.base,sdf6,cat1,ord1, real1 )
LOSS.TR7  = loss(GLRM_TR7,df.base,sdf7,cat1,ord1, real1 )
LOSS.TR8  = loss(GLRM_TR8,df.base,sdf8,cat1,ord1, real1 )
LOSS.TR9  = loss(GLRM_TR9,df.base,sdf9,cat1,ord1, real1 )
LOSS.TR10  = loss(GLRM_TR10,df.base,sdf10,cat1,ord1, real1 )
LOSS.TR11  = loss(GLRM_TR11,df.base,sdf11,cat1,ord1, real1 )
LOSS.TR12  = loss(GLRM_TR12,df.base,sdf12,cat1,ord1, real1 )
LOSS.TR13  = loss(GLRM_TR13,df.base,sdf13,cat1,ord1, real1 )
LOSS.TR14  = loss(GLRM_TR14,df.base,sdf14,cat1,ord1, real1 )
LOSS.TR15  = loss(GLRM_TR15,df.base,sdf15,cat1,ord1, real1 )
LOSS.TR16  = loss(GLRM_TR16,df.base,sdf16,cat1,ord1, real1 )
LOSS.TR17  = loss(GLRM_TR17,df.base,sdf17,cat1,ord1, real1 )
LOSS.TR18  = loss(GLRM_TR18,df.base,sdf18,cat1,ord1, real1 )
LOSS.TR19  = loss(GLRM_TR19,df.base,sdf19,cat1,ord1, real1 )
LOSS.TR20  = loss(GLRM_TR20,df.base,sdf20,cat1,ord1, real1 )


l.tr40 = c(l.tr40, LOSS.TR1$loss)
l.tr40 = c(l.tr40, LOSS.TR2$loss)
l.tr40 = c(l.tr40, LOSS.TR3$loss)
l.tr40 = c(l.tr40, LOSS.TR4$loss)
l.tr40 = c(l.tr40, LOSS.TR5$loss)
l.tr40 = c(l.tr40, LOSS.TR6$loss)
l.tr40 = c(l.tr40, LOSS.TR7$loss)
l.tr40 = c(l.tr40, LOSS.TR8$loss)
l.tr40 = c(l.tr40, LOSS.TR9$loss)
l.tr40 = c(l.tr40, LOSS.TR10$loss)
l.tr40 = c(l.tr40, LOSS.TR11$loss)
l.tr40 = c(l.tr40, LOSS.TR12$loss)
l.tr40 = c(l.tr40, LOSS.TR13$loss)
l.tr40 = c(l.tr40, LOSS.TR14$loss)
l.tr40 = c(l.tr40, LOSS.TR15$loss)
l.tr40 = c(l.tr40, LOSS.TR16$loss)
l.tr40 = c(l.tr40, LOSS.TR17$loss)
l.tr40 = c(l.tr40, LOSS.TR18$loss)
l.tr40 = c(l.tr40, LOSS.TR19$loss)
l.tr40 = c(l.tr40, LOSS.TR20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_TR1, GLRM_TR2, GLRM_TR3, GLRM_TR4, GLRM_TR5,  GLRM_TR6, GLRM_TR7, GLRM_TR8, GLRM_TR9, GLRM_TR10, 
GLRM_TR11, GLRM_TR12, GLRM_TR13, GLRM_TR14, GLRM_TR15,  GLRM_TR16, GLRM_TR17, GLRM_TR18, GLRM_TR19, GLRM_TR20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.TR1, LOSS.TR2, LOSS.TR3, LOSS.TR4, LOSS.TR5,  LOSS.TR6, LOSS.TR7, LOSS.TR8, LOSS.TR9, LOSS.TR10, 
LOSS.TR11, LOSS.TR12, LOSS.TR13, LOSS.TR14, LOSS.TR15,  LOSS.TR16, LOSS.TR17, LOSS.TR18, LOSS.TR19, LOSS.TR20 )


######## 50

glrm1 =  read.csv("Trace_GSS50_MI.csv")
glrm2 =  read.csv("Trace_GSS50_MI (1).csv")
glrm3 =  read.csv("Trace_GSS50_MI (2).csv")
glrm4 =  read.csv("Trace_GSS50_MI (3).csv")
glrm5 =  read.csv("Trace_GSS50_MI (4).csv")
glrm6 =  read.csv("Trace_GSS50_MI (5).csv")
glrm7 =  read.csv("Trace_GSS50_MI (6).csv")
glrm8 =  read.csv("Trace_GSS50_MI (7).csv")
glrm9 =  read.csv("Trace_GSS50_MI (8).csv")
glrm10 = read.csv("Trace_GSS50_MI (9).csv")
glrm11 = read.csv("Trace_GSS50_MI (10).csv")
glrm12 = read.csv("Trace_GSS50_MI (11).csv")
glrm13 = read.csv("Trace_GSS50_MI (12).csv")
glrm14 = read.csv("Trace_GSS50_MI (13).csv")
glrm15 = read.csv("Trace_GSS50_MI (14).csv")
glrm16 = read.csv("Trace_GSS50_MI (15).csv")
glrm17 = read.csv("Trace_GSS50_MI (16).csv")
glrm18 = read.csv("Trace_GSS50_MI (17).csv")
glrm19 = read.csv("Trace_GSS50_MI (18).csv")
glrm20 = read.csv("Trace_GSS50_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Trace_s50.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Trace_s50 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Trace_s50 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Trace_s50 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Trace_s50 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Trace_s50 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Trace_s50 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Trace_s50 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Trace_s50 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Trace_s50 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Trace_s50 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Trace_s50 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Trace_s50 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Trace_s50 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Trace_s50 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Trace_s50 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Trace_s50 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Trace_s50 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Trace_s50 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Trace_s50 (19).csv", header = FALSE)))


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

LOSS.TR1  = loss(GLRM_TR1,df.base,sdf1,cat1,ord1, real1 )
LOSS.TR2  = loss(GLRM_TR2,df.base,sdf2,cat1,ord1, real1 )
LOSS.TR3  = loss(GLRM_TR3,df.base,sdf3,cat1,ord1, real1 )
LOSS.TR4  = loss(GLRM_TR4,df.base,sdf4,cat1,ord1, real1 )
LOSS.TR5  = loss(GLRM_TR5,df.base,sdf5,cat1,ord1, real1 )
LOSS.TR6  = loss(GLRM_TR6,df.base,sdf6,cat1,ord1, real1 )
LOSS.TR7  = loss(GLRM_TR7,df.base,sdf7,cat1,ord1, real1 )
LOSS.TR8  = loss(GLRM_TR8,df.base,sdf8,cat1,ord1, real1 )
LOSS.TR9  = loss(GLRM_TR9,df.base,sdf9,cat1,ord1, real1 )
LOSS.TR10  = loss(GLRM_TR10,df.base,sdf10,cat1,ord1, real1 )
LOSS.TR11  = loss(GLRM_TR11,df.base,sdf11,cat1,ord1, real1 )
LOSS.TR12  = loss(GLRM_TR12,df.base,sdf12,cat1,ord1, real1 )
LOSS.TR13  = loss(GLRM_TR13,df.base,sdf13,cat1,ord1, real1 )
LOSS.TR14  = loss(GLRM_TR14,df.base,sdf14,cat1,ord1, real1 )
LOSS.TR15  = loss(GLRM_TR15,df.base,sdf15,cat1,ord1, real1 )
LOSS.TR16  = loss(GLRM_TR16,df.base,sdf16,cat1,ord1, real1 )
LOSS.TR17  = loss(GLRM_TR17,df.base,sdf17,cat1,ord1, real1 )
LOSS.TR18  = loss(GLRM_TR18,df.base,sdf18,cat1,ord1, real1 )
LOSS.TR19  = loss(GLRM_TR19,df.base,sdf19,cat1,ord1, real1 )
LOSS.TR20  = loss(GLRM_TR20,df.base,sdf20,cat1,ord1, real1 )


l.tr50 = c(l.tr50, LOSS.TR1$loss)
l.tr50 = c(l.tr50, LOSS.TR2$loss)
l.tr50 = c(l.tr50, LOSS.TR3$loss)
l.tr50 = c(l.tr50, LOSS.TR4$loss)
l.tr50 = c(l.tr50, LOSS.TR5$loss)
l.tr50 = c(l.tr50, LOSS.TR6$loss)
l.tr50 = c(l.tr50, LOSS.TR7$loss)
l.tr50 = c(l.tr50, LOSS.TR8$loss)
l.tr50 = c(l.tr50, LOSS.TR9$loss)
l.tr50 = c(l.tr50, LOSS.TR10$loss)
l.tr50 = c(l.tr50, LOSS.TR11$loss)
l.tr50 = c(l.tr50, LOSS.TR12$loss)
l.tr50 = c(l.tr50, LOSS.TR13$loss)
l.tr50 = c(l.tr50, LOSS.TR14$loss)
l.tr50 = c(l.tr50, LOSS.TR15$loss)
l.tr50 = c(l.tr50, LOSS.TR16$loss)
l.tr50 = c(l.tr50, LOSS.TR17$loss)
l.tr50 = c(l.tr50, LOSS.TR18$loss)
l.tr50 = c(l.tr50, LOSS.TR19$loss)
l.tr50 = c(l.tr50, LOSS.TR20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_TR1, GLRM_TR2, GLRM_TR3, GLRM_TR4, GLRM_TR5,  GLRM_TR6, GLRM_TR7, GLRM_TR8, GLRM_TR9, GLRM_TR10, 
GLRM_TR11, GLRM_TR12, GLRM_TR13, GLRM_TR14, GLRM_TR15,  GLRM_TR16, GLRM_TR17, GLRM_TR18, GLRM_TR19, GLRM_TR20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.TR1, LOSS.TR2, LOSS.TR3, LOSS.TR4, LOSS.TR5,  LOSS.TR6, LOSS.TR7, LOSS.TR8, LOSS.TR9, LOSS.TR10, 
LOSS.TR11, LOSS.TR12, LOSS.TR13, LOSS.TR14, LOSS.TR15,  LOSS.TR16, LOSS.TR17, LOSS.TR18, LOSS.TR19, LOSS.TR20 )


######## 60

glrm1 =  read.csv("Trace_GSS60_MI.csv")
glrm2 =  read.csv("Trace_GSS60_MI (1).csv")
glrm3 =  read.csv("Trace_GSS60_MI (2).csv")
glrm4 =  read.csv("Trace_GSS60_MI (3).csv")
glrm5 =  read.csv("Trace_GSS60_MI (4).csv")
glrm6 =  read.csv("Trace_GSS60_MI (5).csv")
glrm7 =  read.csv("Trace_GSS60_MI (6).csv")
glrm8 =  read.csv("Trace_GSS60_MI (7).csv")
glrm9 =  read.csv("Trace_GSS60_MI (8).csv")
glrm10 = read.csv("Trace_GSS60_MI (9).csv")
glrm11 = read.csv("Trace_GSS60_MI (10).csv")
glrm12 = read.csv("Trace_GSS60_MI (11).csv")
glrm13 = read.csv("Trace_GSS60_MI (12).csv")
glrm14 = read.csv("Trace_GSS60_MI (13).csv")
glrm15 = read.csv("Trace_GSS60_MI (14).csv")
glrm16 = read.csv("Trace_GSS60_MI (15).csv")
glrm17 = read.csv("Trace_GSS60_MI (16).csv")
glrm18 = read.csv("Trace_GSS60_MI (17).csv")
glrm19 = read.csv("Trace_GSS60_MI (18).csv")
glrm20 = read.csv("Trace_GSS60_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Trace_s60.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Trace_s60 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Trace_s60 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Trace_s60 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Trace_s60 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Trace_s60 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Trace_s60 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Trace_s60 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Trace_s60 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Trace_s60 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Trace_s60 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Trace_s60 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Trace_s60 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Trace_s60 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Trace_s60 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Trace_s60 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Trace_s60 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Trace_s60 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Trace_s60 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Trace_s60 (19).csv", header = FALSE)))


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

LOSS.TR1  = loss(GLRM_TR1,df.base,sdf1,cat1,ord1, real1 )
LOSS.TR2  = loss(GLRM_TR2,df.base,sdf2,cat1,ord1, real1 )
LOSS.TR3  = loss(GLRM_TR3,df.base,sdf3,cat1,ord1, real1 )
LOSS.TR4  = loss(GLRM_TR4,df.base,sdf4,cat1,ord1, real1 )
LOSS.TR5  = loss(GLRM_TR5,df.base,sdf5,cat1,ord1, real1 )
LOSS.TR6  = loss(GLRM_TR6,df.base,sdf6,cat1,ord1, real1 )
LOSS.TR7  = loss(GLRM_TR7,df.base,sdf7,cat1,ord1, real1 )
LOSS.TR8  = loss(GLRM_TR8,df.base,sdf8,cat1,ord1, real1 )
LOSS.TR9  = loss(GLRM_TR9,df.base,sdf9,cat1,ord1, real1 )
LOSS.TR10  = loss(GLRM_TR10,df.base,sdf10,cat1,ord1, real1 )
LOSS.TR11  = loss(GLRM_TR11,df.base,sdf11,cat1,ord1, real1 )
LOSS.TR12  = loss(GLRM_TR12,df.base,sdf12,cat1,ord1, real1 )
LOSS.TR13  = loss(GLRM_TR13,df.base,sdf13,cat1,ord1, real1 )
LOSS.TR14  = loss(GLRM_TR14,df.base,sdf14,cat1,ord1, real1 )
LOSS.TR15  = loss(GLRM_TR15,df.base,sdf15,cat1,ord1, real1 )
LOSS.TR16  = loss(GLRM_TR16,df.base,sdf16,cat1,ord1, real1 )
LOSS.TR17  = loss(GLRM_TR17,df.base,sdf17,cat1,ord1, real1 )
LOSS.TR18  = loss(GLRM_TR18,df.base,sdf18,cat1,ord1, real1 )
LOSS.TR19  = loss(GLRM_TR19,df.base,sdf19,cat1,ord1, real1 )
LOSS.TR20  = loss(GLRM_TR20,df.base,sdf20,cat1,ord1, real1 )


l.tr60 = c(l.tr60, LOSS.TR1$loss)
l.tr60 = c(l.tr60, LOSS.TR2$loss)
l.tr60 = c(l.tr60, LOSS.TR3$loss)
l.tr60 = c(l.tr60, LOSS.TR4$loss)
l.tr60 = c(l.tr60, LOSS.TR5$loss)
l.tr60 = c(l.tr60, LOSS.TR6$loss)
l.tr60 = c(l.tr60, LOSS.TR7$loss)
l.tr60 = c(l.tr60, LOSS.TR8$loss)
l.tr60 = c(l.tr60, LOSS.TR9$loss)
l.tr60 = c(l.tr60, LOSS.TR10$loss)
l.tr60 = c(l.tr60, LOSS.TR11$loss)
l.tr60 = c(l.tr60, LOSS.TR12$loss)
l.tr60 = c(l.tr60, LOSS.TR13$loss)
l.tr60 = c(l.tr60, LOSS.TR14$loss)
l.tr60 = c(l.tr60, LOSS.TR15$loss)
l.tr60 = c(l.tr60, LOSS.TR16$loss)
l.tr60 = c(l.tr60, LOSS.TR17$loss)
l.tr60 = c(l.tr60, LOSS.TR18$loss)
l.tr60 = c(l.tr60, LOSS.TR19$loss)
l.tr60 = c(l.tr60, LOSS.TR20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_TR1, GLRM_TR2, GLRM_TR3, GLRM_TR4, GLRM_TR5,  GLRM_TR6, GLRM_TR7, GLRM_TR8, GLRM_TR9, GLRM_TR10, 
GLRM_TR11, GLRM_TR12, GLRM_TR13, GLRM_TR14, GLRM_TR15,  GLRM_TR16, GLRM_TR17, GLRM_TR18, GLRM_TR19, GLRM_TR20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.TR1, LOSS.TR2, LOSS.TR3, LOSS.TR4, LOSS.TR5,  LOSS.TR6, LOSS.TR7, LOSS.TR8, LOSS.TR9, LOSS.TR10, 
LOSS.TR11, LOSS.TR12, LOSS.TR13, LOSS.TR14, LOSS.TR15,  LOSS.TR16, LOSS.TR17, LOSS.TR18, LOSS.TR19, LOSS.TR20 )


######## 70

glrm1 =  read.csv("Trace_GSS70_MI.csv")
glrm2 =  read.csv("Trace_GSS70_MI (1).csv")
glrm3 =  read.csv("Trace_GSS70_MI (2).csv")
glrm4 =  read.csv("Trace_GSS70_MI (3).csv")
glrm5 =  read.csv("Trace_GSS70_MI (4).csv")
glrm6 =  read.csv("Trace_GSS70_MI (5).csv")
glrm7 =  read.csv("Trace_GSS70_MI (6).csv")
glrm8 =  read.csv("Trace_GSS70_MI (7).csv")
glrm9 =  read.csv("Trace_GSS70_MI (8).csv")
glrm10 = read.csv("Trace_GSS70_MI (9).csv")
glrm11 = read.csv("Trace_GSS70_MI (10).csv")
glrm12 = read.csv("Trace_GSS70_MI (11).csv")
glrm13 = read.csv("Trace_GSS70_MI (12).csv")
glrm14 = read.csv("Trace_GSS70_MI (13).csv")
glrm15 = read.csv("Trace_GSS70_MI (14).csv")
glrm16 = read.csv("Trace_GSS70_MI (15).csv")
glrm17 = read.csv("Trace_GSS70_MI (16).csv")
glrm18 = read.csv("Trace_GSS70_MI (17).csv")
glrm19 = read.csv("Trace_GSS70_MI (18).csv")
glrm20 = read.csv("Trace_GSS70_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Trace_s70.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Trace_s70 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Trace_s70 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Trace_s70 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Trace_s70 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Trace_s70 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Trace_s70 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Trace_s70 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Trace_s70 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Trace_s70 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Trace_s70 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Trace_s70 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Trace_s70 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Trace_s70 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Trace_s70 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Trace_s70 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Trace_s70 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Trace_s70 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Trace_s70 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Trace_s70 (19).csv", header = FALSE)))


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

LOSS.TR1  = loss(GLRM_TR1,df.base,sdf1,cat1,ord1, real1 )
LOSS.TR2  = loss(GLRM_TR2,df.base,sdf2,cat1,ord1, real1 )
LOSS.TR3  = loss(GLRM_TR3,df.base,sdf3,cat1,ord1, real1 )
LOSS.TR4  = loss(GLRM_TR4,df.base,sdf4,cat1,ord1, real1 )
LOSS.TR5  = loss(GLRM_TR5,df.base,sdf5,cat1,ord1, real1 )
LOSS.TR6  = loss(GLRM_TR6,df.base,sdf6,cat1,ord1, real1 )
LOSS.TR7  = loss(GLRM_TR7,df.base,sdf7,cat1,ord1, real1 )
LOSS.TR8  = loss(GLRM_TR8,df.base,sdf8,cat1,ord1, real1 )
LOSS.TR9  = loss(GLRM_TR9,df.base,sdf9,cat1,ord1, real1 )
LOSS.TR10  = loss(GLRM_TR10,df.base,sdf10,cat1,ord1, real1 )
LOSS.TR11  = loss(GLRM_TR11,df.base,sdf11,cat1,ord1, real1 )
LOSS.TR12  = loss(GLRM_TR12,df.base,sdf12,cat1,ord1, real1 )
LOSS.TR13  = loss(GLRM_TR13,df.base,sdf13,cat1,ord1, real1 )
LOSS.TR14  = loss(GLRM_TR14,df.base,sdf14,cat1,ord1, real1 )
LOSS.TR15  = loss(GLRM_TR15,df.base,sdf15,cat1,ord1, real1 )
LOSS.TR16  = loss(GLRM_TR16,df.base,sdf16,cat1,ord1, real1 )
LOSS.TR17  = loss(GLRM_TR17,df.base,sdf17,cat1,ord1, real1 )
LOSS.TR18  = loss(GLRM_TR18,df.base,sdf18,cat1,ord1, real1 )
LOSS.TR19  = loss(GLRM_TR19,df.base,sdf19,cat1,ord1, real1 )
LOSS.TR20  = loss(GLRM_TR20,df.base,sdf20,cat1,ord1, real1 )


l.tr70 = c(l.tr70, LOSS.TR1$loss)
l.tr70 = c(l.tr70, LOSS.TR2$loss)
l.tr70 = c(l.tr70, LOSS.TR3$loss)
l.tr70 = c(l.tr70, LOSS.TR4$loss)
l.tr70 = c(l.tr70, LOSS.TR5$loss)
l.tr70 = c(l.tr70, LOSS.TR6$loss)
l.tr70 = c(l.tr70, LOSS.TR7$loss)
l.tr70 = c(l.tr70, LOSS.TR8$loss)
l.tr70 = c(l.tr70, LOSS.TR9$loss)
l.tr70 = c(l.tr70, LOSS.TR10$loss)
l.tr70 = c(l.tr70, LOSS.TR11$loss)
l.tr70 = c(l.tr70, LOSS.TR12$loss)
l.tr70 = c(l.tr70, LOSS.TR13$loss)
l.tr70 = c(l.tr70, LOSS.TR14$loss)
l.tr70 = c(l.tr70, LOSS.TR15$loss)
l.tr70 = c(l.tr70, LOSS.TR16$loss)
l.tr70 = c(l.tr70, LOSS.TR17$loss)
l.tr70 = c(l.tr70, LOSS.TR18$loss)
l.tr70 = c(l.tr70, LOSS.TR19$loss)
l.tr70 = c(l.tr70, LOSS.TR20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_TR1, GLRM_TR2, GLRM_TR3, GLRM_TR4, GLRM_TR5,  GLRM_TR6, GLRM_TR7, GLRM_TR8, GLRM_TR9, GLRM_TR10, 
GLRM_TR11, GLRM_TR12, GLRM_TR13, GLRM_TR14, GLRM_TR15,  GLRM_TR16, GLRM_TR17, GLRM_TR18, GLRM_TR19, GLRM_TR20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.TR1, LOSS.TR2, LOSS.TR3, LOSS.TR4, LOSS.TR5,  LOSS.TR6, LOSS.TR7, LOSS.TR8, LOSS.TR9, LOSS.TR10, 
LOSS.TR11, LOSS.TR12, LOSS.TR13, LOSS.TR14, LOSS.TR15,  LOSS.TR16, LOSS.TR17, LOSS.TR18, LOSS.TR19, LOSS.TR20 )



######## 80

glrm1 =  read.csv("Trace_GSS80_MI.csv")
glrm2 =  read.csv("Trace_GSS80_MI (1).csv")
glrm3 =  read.csv("Trace_GSS80_MI (2).csv")
glrm4 =  read.csv("Trace_GSS80_MI (3).csv")
glrm5 =  read.csv("Trace_GSS80_MI (4).csv")
glrm6 =  read.csv("Trace_GSS80_MI (5).csv")
glrm7 =  read.csv("Trace_GSS80_MI (6).csv")
glrm8 =  read.csv("Trace_GSS80_MI (7).csv")
glrm9 =  read.csv("Trace_GSS80_MI (8).csv")
glrm10 = read.csv("Trace_GSS80_MI (9).csv")
glrm11 = read.csv("Trace_GSS80_MI (10).csv")
glrm12 = read.csv("Trace_GSS80_MI (11).csv")
glrm13 = read.csv("Trace_GSS80_MI (12).csv")
glrm14 = read.csv("Trace_GSS80_MI (13).csv")
glrm15 = read.csv("Trace_GSS80_MI (14).csv")
glrm16 = read.csv("Trace_GSS80_MI (15).csv")
glrm17 = read.csv("Trace_GSS80_MI (16).csv")
glrm18 = read.csv("Trace_GSS80_MI (17).csv")
glrm19 = read.csv("Trace_GSS80_MI (18).csv")
glrm20 = read.csv("Trace_GSS80_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Trace_s80.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Trace_s80 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Trace_s80 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Trace_s80 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Trace_s80 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Trace_s80 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Trace_s80 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Trace_s80 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Trace_s80 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Trace_s80 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Trace_s80 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Trace_s80 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Trace_s80 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Trace_s80 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Trace_s80 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Trace_s80 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Trace_s80 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Trace_s80 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Trace_s80 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Trace_s80 (19).csv", header = FALSE)))


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

LOSS.TR1  = loss(GLRM_TR1,df.base,sdf1,cat1,ord1, real1 )
LOSS.TR2  = loss(GLRM_TR2,df.base,sdf2,cat1,ord1, real1 )
LOSS.TR3  = loss(GLRM_TR3,df.base,sdf3,cat1,ord1, real1 )
LOSS.TR4  = loss(GLRM_TR4,df.base,sdf4,cat1,ord1, real1 )
LOSS.TR5  = loss(GLRM_TR5,df.base,sdf5,cat1,ord1, real1 )
LOSS.TR6  = loss(GLRM_TR6,df.base,sdf6,cat1,ord1, real1 )
LOSS.TR7  = loss(GLRM_TR7,df.base,sdf7,cat1,ord1, real1 )
LOSS.TR8  = loss(GLRM_TR8,df.base,sdf8,cat1,ord1, real1 )
LOSS.TR9  = loss(GLRM_TR9,df.base,sdf9,cat1,ord1, real1 )
LOSS.TR10  = loss(GLRM_TR10,df.base,sdf10,cat1,ord1, real1 )
LOSS.TR11  = loss(GLRM_TR11,df.base,sdf11,cat1,ord1, real1 )
LOSS.TR12  = loss(GLRM_TR12,df.base,sdf12,cat1,ord1, real1 )
LOSS.TR13  = loss(GLRM_TR13,df.base,sdf13,cat1,ord1, real1 )
LOSS.TR14  = loss(GLRM_TR14,df.base,sdf14,cat1,ord1, real1 )
LOSS.TR15  = loss(GLRM_TR15,df.base,sdf15,cat1,ord1, real1 )
LOSS.TR16  = loss(GLRM_TR16,df.base,sdf16,cat1,ord1, real1 )
LOSS.TR17  = loss(GLRM_TR17,df.base,sdf17,cat1,ord1, real1 )
LOSS.TR18  = loss(GLRM_TR18,df.base,sdf18,cat1,ord1, real1 )
LOSS.TR19  = loss(GLRM_TR19,df.base,sdf19,cat1,ord1, real1 )
LOSS.TR20  = loss(GLRM_TR20,df.base,sdf20,cat1,ord1, real1 )


l.tr80 = c(l.tr80, LOSS.TR1$loss)
l.tr80 = c(l.tr80, LOSS.TR2$loss)
l.tr80 = c(l.tr80, LOSS.TR3$loss)
l.tr80 = c(l.tr80, LOSS.TR4$loss)
l.tr80 = c(l.tr80, LOSS.TR5$loss)
l.tr80 = c(l.tr80, LOSS.TR6$loss)
l.tr80 = c(l.tr80, LOSS.TR7$loss)
l.tr80 = c(l.tr80, LOSS.TR8$loss)
l.tr80 = c(l.tr80, LOSS.TR9$loss)
l.tr80 = c(l.tr80, LOSS.TR10$loss)
l.tr80 = c(l.tr80, LOSS.TR11$loss)
l.tr80 = c(l.tr80, LOSS.TR12$loss)
l.tr80 = c(l.tr80, LOSS.TR13$loss)
l.tr80 = c(l.tr80, LOSS.TR14$loss)
l.tr80 = c(l.tr80, LOSS.TR15$loss)
l.tr80 = c(l.tr80, LOSS.TR16$loss)
l.tr80 = c(l.tr80, LOSS.TR17$loss)
l.tr80 = c(l.tr80, LOSS.TR18$loss)
l.tr80 = c(l.tr80, LOSS.TR19$loss)
l.tr80 = c(l.tr80, LOSS.TR20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_TR1, GLRM_TR2, GLRM_TR3, GLRM_TR4, GLRM_TR5,  GLRM_TR6, GLRM_TR7, GLRM_TR8, GLRM_TR9, GLRM_TR10, 
GLRM_TR11, GLRM_TR12, GLRM_TR13, GLRM_TR14, GLRM_TR15,  GLRM_TR16, GLRM_TR17, GLRM_TR18, GLRM_TR19, GLRM_TR20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.TR1, LOSS.TR2, LOSS.TR3, LOSS.TR4, LOSS.TR5,  LOSS.TR6, LOSS.TR7, LOSS.TR8, LOSS.TR9, LOSS.TR10, 
LOSS.TR11, LOSS.TR12, LOSS.TR13, LOSS.TR14, LOSS.TR15,  LOSS.TR16, LOSS.TR17, LOSS.TR18, LOSS.TR19, LOSS.TR20 )


######## 90

glrm1 =  read.csv("Trace_GSS90_MI.csv")
glrm2 =  read.csv("Trace_GSS90_MI (1).csv")
glrm3 =  read.csv("Trace_GSS90_MI (2).csv")
glrm4 =  read.csv("Trace_GSS90_MI (3).csv")
glrm5 =  read.csv("Trace_GSS90_MI (4).csv")
glrm6 =  read.csv("Trace_GSS90_MI (5).csv")
glrm7 =  read.csv("Trace_GSS90_MI (6).csv")
glrm8 =  read.csv("Trace_GSS90_MI (7).csv")
glrm9 =  read.csv("Trace_GSS90_MI (8).csv")
glrm10 = read.csv("Trace_GSS90_MI (9).csv")
glrm11 = read.csv("Trace_GSS90_MI (10).csv")
glrm12 = read.csv("Trace_GSS90_MI (11).csv")
glrm13 = read.csv("Trace_GSS90_MI (12).csv")
glrm14 = read.csv("Trace_GSS90_MI (13).csv")
glrm15 = read.csv("Trace_GSS90_MI (14).csv")
glrm16 = read.csv("Trace_GSS90_MI (15).csv")
glrm17 = read.csv("Trace_GSS90_MI (16).csv")
glrm18 = read.csv("Trace_GSS90_MI (17).csv")
glrm19 = read.csv("Trace_GSS90_MI (18).csv")
glrm20 = read.csv("Trace_GSS90_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Trace_s90.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Trace_s90 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Trace_s90 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Trace_s90 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Trace_s90 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Trace_s90 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Trace_s90 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Trace_s90 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Trace_s90 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Trace_s90 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Trace_s90 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Trace_s90 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Trace_s90 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Trace_s90 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Trace_s90 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Trace_s90 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Trace_s90 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Trace_s90 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Trace_s90 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Trace_s90 (19).csv", header = FALSE)))


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

LOSS.TR1  = loss(GLRM_TR1,df.base,sdf1,cat1,ord1, real1 )
LOSS.TR2  = loss(GLRM_TR2,df.base,sdf2,cat1,ord1, real1 )
LOSS.TR3  = loss(GLRM_TR3,df.base,sdf3,cat1,ord1, real1 )
LOSS.TR4  = loss(GLRM_TR4,df.base,sdf4,cat1,ord1, real1 )
LOSS.TR5  = loss(GLRM_TR5,df.base,sdf5,cat1,ord1, real1 )
LOSS.TR6  = loss(GLRM_TR6,df.base,sdf6,cat1,ord1, real1 )
LOSS.TR7  = loss(GLRM_TR7,df.base,sdf7,cat1,ord1, real1 )
LOSS.TR8  = loss(GLRM_TR8,df.base,sdf8,cat1,ord1, real1 )
LOSS.TR9  = loss(GLRM_TR9,df.base,sdf9,cat1,ord1, real1 )
LOSS.TR10  = loss(GLRM_TR10,df.base,sdf10,cat1,ord1, real1 )
LOSS.TR11  = loss(GLRM_TR11,df.base,sdf11,cat1,ord1, real1 )
LOSS.TR12  = loss(GLRM_TR12,df.base,sdf12,cat1,ord1, real1 )
LOSS.TR13  = loss(GLRM_TR13,df.base,sdf13,cat1,ord1, real1 )
LOSS.TR14  = loss(GLRM_TR14,df.base,sdf14,cat1,ord1, real1 )
LOSS.TR15  = loss(GLRM_TR15,df.base,sdf15,cat1,ord1, real1 )
LOSS.TR16  = loss(GLRM_TR16,df.base,sdf16,cat1,ord1, real1 )
LOSS.TR17  = loss(GLRM_TR17,df.base,sdf17,cat1,ord1, real1 )
LOSS.TR18  = loss(GLRM_TR18,df.base,sdf18,cat1,ord1, real1 )
LOSS.TR19  = loss(GLRM_TR19,df.base,sdf19,cat1,ord1, real1 )
LOSS.TR20  = loss(GLRM_TR20,df.base,sdf20,cat1,ord1, real1 )


l.tr90 = c(l.tr90, LOSS.TR1$loss)
l.tr90 = c(l.tr90, LOSS.TR2$loss)
l.tr90 = c(l.tr90, LOSS.TR3$loss)
l.tr90 = c(l.tr90, LOSS.TR4$loss)
l.tr90 = c(l.tr90, LOSS.TR5$loss)
l.tr90 = c(l.tr90, LOSS.TR6$loss)
l.tr90 = c(l.tr90, LOSS.TR7$loss)
l.tr90 = c(l.tr90, LOSS.TR8$loss)
l.tr90 = c(l.tr90, LOSS.TR9$loss)
l.tr90 = c(l.tr90, LOSS.TR10$loss)
l.tr90 = c(l.tr90, LOSS.TR11$loss)
l.tr90 = c(l.tr90, LOSS.TR12$loss)
l.tr90 = c(l.tr90, LOSS.TR13$loss)
l.tr90 = c(l.tr90, LOSS.TR14$loss)
l.tr90 = c(l.tr90, LOSS.TR15$loss)
l.tr90 = c(l.tr90, LOSS.TR16$loss)
l.tr90 = c(l.tr90, LOSS.TR17$loss)
l.tr90 = c(l.tr90, LOSS.TR18$loss)
l.tr90 = c(l.tr90, LOSS.TR19$loss)
l.tr90 = c(l.tr90, LOSS.TR20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_TR1, GLRM_TR2, GLRM_TR3, GLRM_TR4, GLRM_TR5,  GLRM_TR6, GLRM_TR7, GLRM_TR8, GLRM_TR9, GLRM_TR10, 
GLRM_TR11, GLRM_TR12, GLRM_TR13, GLRM_TR14, GLRM_TR15,  GLRM_TR16, GLRM_TR17, GLRM_TR18, GLRM_TR19, GLRM_TR20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.TR1, LOSS.TR2, LOSS.TR3, LOSS.TR4, LOSS.TR5,  LOSS.TR6, LOSS.TR7, LOSS.TR8, LOSS.TR9, LOSS.TR10, 
LOSS.TR11, LOSS.TR12, LOSS.TR13, LOSS.TR14, LOSS.TR15,  LOSS.TR16, LOSS.TR17, LOSS.TR18, LOSS.TR19, LOSS.TR20 )

save(l.tr10, l.tr20, l.tr30, l.tr40, l.tr50, l.tr60, l.tr70, l.tr80, l.tr90 , file = "trace_consistency.RData")

