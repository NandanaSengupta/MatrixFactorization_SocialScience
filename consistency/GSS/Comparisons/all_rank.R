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
	
# converting indices to markix row and columns
	r = ((ind-1) %% n) + 1 
	c = floor((ind-1) / n) + 1
	
# for categorical columns loss is whether or not mis-classified  (also keeping rkack of number of factor levels here)

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
l.rk10 = numeric()
l.rk20 = numeric()
l.rk30 = numeric()
l.rk40 = numeric()
l.rk50 = numeric()
l.rk60 = numeric()
l.rk70 = numeric()
l.rk80 = numeric()
l.rk90 = numeric()

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/GSS/RANK")


######## 10

glrm1 =  read.csv("Rank_GSS10_MI.csv")
glrm2 =  read.csv("Rank_GSS10_MI (1).csv")
glrm3 =  read.csv("Rank_GSS10_MI (2).csv")
glrm4 =  read.csv("Rank_GSS10_MI (3).csv")
glrm5 =  read.csv("Rank_GSS10_MI (4).csv")
glrm6 =  read.csv("Rank_GSS10_MI (5).csv")
glrm7 =  read.csv("Rank_GSS10_MI (6).csv")
glrm8 =  read.csv("Rank_GSS10_MI (7).csv")
glrm9 =  read.csv("Rank_GSS10_MI (8).csv")
glrm10 = read.csv("Rank_GSS10_MI (9).csv")
glrm11 = read.csv("Rank_GSS10_MI (10).csv")
glrm12 = read.csv("Rank_GSS10_MI (11).csv")
glrm13 = read.csv("Rank_GSS10_MI (12).csv")
glrm14 = read.csv("Rank_GSS10_MI (13).csv")
glrm15 = read.csv("Rank_GSS10_MI (14).csv")
glrm16 = read.csv("Rank_GSS10_MI (15).csv")
glrm17 = read.csv("Rank_GSS10_MI (16).csv")
glrm18 = read.csv("Rank_GSS10_MI (17).csv")
glrm19 = read.csv("Rank_GSS10_MI (18).csv")
glrm20 = read.csv("Rank_GSS10_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Rank_s10.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Rank_s10 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Rank_s10 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Rank_s10 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Rank_s10 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Rank_s10 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Rank_s10 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Rank_s10 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Rank_s10 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Rank_s10 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Rank_s10 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Rank_s10 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Rank_s10 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Rank_s10 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Rank_s10 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Rank_s10 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Rank_s10 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Rank_s10 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Rank_s10 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Rank_s10 (19).csv", header = FALSE)))


GLRM_RK1 = factorLevels(glrm1, df.base, cat1)
GLRM_RK2 = factorLevels(glrm2, df.base, cat1)
GLRM_RK3 = factorLevels(glrm3, df.base, cat1)
GLRM_RK4 = factorLevels(glrm4, df.base, cat1)
GLRM_RK5 = factorLevels(glrm5, df.base, cat1)
GLRM_RK6 = factorLevels(glrm6, df.base, cat1)
GLRM_RK7 = factorLevels(glrm7, df.base, cat1)
GLRM_RK8 = factorLevels(glrm8, df.base, cat1)
GLRM_RK9 = factorLevels(glrm9, df.base, cat1)
GLRM_RK10 = factorLevels(glrm10, df.base, cat1)
GLRM_RK11 = factorLevels(glrm11, df.base, cat1)
GLRM_RK12 = factorLevels(glrm12, df.base, cat1)
GLRM_RK13 = factorLevels(glrm13, df.base, cat1)
GLRM_RK14 = factorLevels(glrm14, df.base, cat1)
GLRM_RK15 = factorLevels(glrm15, df.base, cat1)
GLRM_RK16 = factorLevels(glrm16, df.base, cat1)
GLRM_RK17 = factorLevels(glrm17, df.base, cat1)
GLRM_RK18 = factorLevels(glrm18, df.base, cat1)
GLRM_RK19 = factorLevels(glrm19, df.base, cat1)
GLRM_RK20 = factorLevels(glrm20, df.base, cat1)

LOSS.RK1  = loss(GLRM_RK1,df.base,sdf1,cat1,ord1, real1 )
LOSS.RK2  = loss(GLRM_RK2,df.base,sdf2,cat1,ord1, real1 )
LOSS.RK3  = loss(GLRM_RK3,df.base,sdf3,cat1,ord1, real1 )
LOSS.RK4  = loss(GLRM_RK4,df.base,sdf4,cat1,ord1, real1 )
LOSS.RK5  = loss(GLRM_RK5,df.base,sdf5,cat1,ord1, real1 )
LOSS.RK6  = loss(GLRM_RK6,df.base,sdf6,cat1,ord1, real1 )
LOSS.RK7  = loss(GLRM_RK7,df.base,sdf7,cat1,ord1, real1 )
LOSS.RK8  = loss(GLRM_RK8,df.base,sdf8,cat1,ord1, real1 )
LOSS.RK9  = loss(GLRM_RK9,df.base,sdf9,cat1,ord1, real1 )
LOSS.RK10  = loss(GLRM_RK10,df.base,sdf10,cat1,ord1, real1 )
LOSS.RK11  = loss(GLRM_RK11,df.base,sdf11,cat1,ord1, real1 )
LOSS.RK12  = loss(GLRM_RK12,df.base,sdf12,cat1,ord1, real1 )
LOSS.RK13  = loss(GLRM_RK13,df.base,sdf13,cat1,ord1, real1 )
LOSS.RK14  = loss(GLRM_RK14,df.base,sdf14,cat1,ord1, real1 )
LOSS.RK15  = loss(GLRM_RK15,df.base,sdf15,cat1,ord1, real1 )
LOSS.RK16  = loss(GLRM_RK16,df.base,sdf16,cat1,ord1, real1 )
LOSS.RK17  = loss(GLRM_RK17,df.base,sdf17,cat1,ord1, real1 )
LOSS.RK18  = loss(GLRM_RK18,df.base,sdf18,cat1,ord1, real1 )
LOSS.RK19  = loss(GLRM_RK19,df.base,sdf19,cat1,ord1, real1 )
LOSS.RK20  = loss(GLRM_RK20,df.base,sdf20,cat1,ord1, real1 )


l.rk10 = c(l.rk10, LOSS.RK1$loss)
l.rk10 = c(l.rk10, LOSS.RK2$loss)
l.rk10 = c(l.rk10, LOSS.RK3$loss)
l.rk10 = c(l.rk10, LOSS.RK4$loss)
l.rk10 = c(l.rk10, LOSS.RK5$loss)
l.rk10 = c(l.rk10, LOSS.RK6$loss)
l.rk10 = c(l.rk10, LOSS.RK7$loss)
l.rk10 = c(l.rk10, LOSS.RK8$loss)
l.rk10 = c(l.rk10, LOSS.RK9$loss)
l.rk10 = c(l.rk10, LOSS.RK10$loss)
l.rk10 = c(l.rk10, LOSS.RK11$loss)
l.rk10 = c(l.rk10, LOSS.RK12$loss)
l.rk10 = c(l.rk10, LOSS.RK13$loss)
l.rk10 = c(l.rk10, LOSS.RK14$loss)
l.rk10 = c(l.rk10, LOSS.RK15$loss)
l.rk10 = c(l.rk10, LOSS.RK16$loss)
l.rk10 = c(l.rk10, LOSS.RK17$loss)
l.rk10 = c(l.rk10, LOSS.RK18$loss)
l.rk10 = c(l.rk10, LOSS.RK19$loss)
l.rk10 = c(l.rk10, LOSS.RK20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_RK1, GLRM_RK2, GLRM_RK3, GLRM_RK4, GLRM_RK5,  GLRM_RK6, GLRM_RK7, GLRM_RK8, GLRM_RK9, GLRM_RK10, 
GLRM_RK11, GLRM_RK12, GLRM_RK13, GLRM_RK14, GLRM_RK15,  GLRM_RK16, GLRM_RK17, GLRM_RK18, GLRM_RK19, GLRM_RK20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.RK1, LOSS.RK2, LOSS.RK3, LOSS.RK4, LOSS.RK5,  LOSS.RK6, LOSS.RK7, LOSS.RK8, LOSS.RK9, LOSS.RK10, 
LOSS.RK11, LOSS.RK12, LOSS.RK13, LOSS.RK14, LOSS.RK15,  LOSS.RK16, LOSS.RK17, LOSS.RK18, LOSS.RK19, LOSS.RK20 )


######## 20

glrm1 =  read.csv("Rank_GSS20_MI.csv")
glrm2 =  read.csv("Rank_GSS20_MI (1).csv")
glrm3 =  read.csv("Rank_GSS20_MI (2).csv")
glrm4 =  read.csv("Rank_GSS20_MI (3).csv")
glrm5 =  read.csv("Rank_GSS20_MI (4).csv")
glrm6 =  read.csv("Rank_GSS20_MI (5).csv")
glrm7 =  read.csv("Rank_GSS20_MI (6).csv")
glrm8 =  read.csv("Rank_GSS20_MI (7).csv")
glrm9 =  read.csv("Rank_GSS20_MI (8).csv")
glrm10 = read.csv("Rank_GSS20_MI (9).csv")
glrm11 = read.csv("Rank_GSS20_MI (10).csv")
glrm12 = read.csv("Rank_GSS20_MI (11).csv")
glrm13 = read.csv("Rank_GSS20_MI (12).csv")
glrm14 = read.csv("Rank_GSS20_MI (13).csv")
glrm15 = read.csv("Rank_GSS20_MI (14).csv")
glrm16 = read.csv("Rank_GSS20_MI (15).csv")
glrm17 = read.csv("Rank_GSS20_MI (16).csv")
glrm18 = read.csv("Rank_GSS20_MI (17).csv")
glrm19 = read.csv("Rank_GSS20_MI (18).csv")
glrm20 = read.csv("Rank_GSS20_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Rank_s20.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Rank_s20 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Rank_s20 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Rank_s20 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Rank_s20 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Rank_s20 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Rank_s20 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Rank_s20 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Rank_s20 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Rank_s20 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Rank_s20 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Rank_s20 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Rank_s20 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Rank_s20 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Rank_s20 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Rank_s20 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Rank_s20 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Rank_s20 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Rank_s20 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Rank_s20 (19).csv", header = FALSE)))


GLRM_RK1 = factorLevels(glrm1, df.base, cat1)
GLRM_RK2 = factorLevels(glrm2, df.base, cat1)
GLRM_RK3 = factorLevels(glrm3, df.base, cat1)
GLRM_RK4 = factorLevels(glrm4, df.base, cat1)
GLRM_RK5 = factorLevels(glrm5, df.base, cat1)
GLRM_RK6 = factorLevels(glrm6, df.base, cat1)
GLRM_RK7 = factorLevels(glrm7, df.base, cat1)
GLRM_RK8 = factorLevels(glrm8, df.base, cat1)
GLRM_RK9 = factorLevels(glrm9, df.base, cat1)
GLRM_RK10 = factorLevels(glrm10, df.base, cat1)
GLRM_RK11 = factorLevels(glrm11, df.base, cat1)
GLRM_RK12 = factorLevels(glrm12, df.base, cat1)
GLRM_RK13 = factorLevels(glrm13, df.base, cat1)
GLRM_RK14 = factorLevels(glrm14, df.base, cat1)
GLRM_RK15 = factorLevels(glrm15, df.base, cat1)
GLRM_RK16 = factorLevels(glrm16, df.base, cat1)
GLRM_RK17 = factorLevels(glrm17, df.base, cat1)
GLRM_RK18 = factorLevels(glrm18, df.base, cat1)
GLRM_RK19 = factorLevels(glrm19, df.base, cat1)
GLRM_RK20 = factorLevels(glrm20, df.base, cat1)

LOSS.RK1  = loss(GLRM_RK1,df.base,sdf1,cat1,ord1, real1 )
LOSS.RK2  = loss(GLRM_RK2,df.base,sdf2,cat1,ord1, real1 )
LOSS.RK3  = loss(GLRM_RK3,df.base,sdf3,cat1,ord1, real1 )
LOSS.RK4  = loss(GLRM_RK4,df.base,sdf4,cat1,ord1, real1 )
LOSS.RK5  = loss(GLRM_RK5,df.base,sdf5,cat1,ord1, real1 )
LOSS.RK6  = loss(GLRM_RK6,df.base,sdf6,cat1,ord1, real1 )
LOSS.RK7  = loss(GLRM_RK7,df.base,sdf7,cat1,ord1, real1 )
LOSS.RK8  = loss(GLRM_RK8,df.base,sdf8,cat1,ord1, real1 )
LOSS.RK9  = loss(GLRM_RK9,df.base,sdf9,cat1,ord1, real1 )
LOSS.RK10  = loss(GLRM_RK10,df.base,sdf10,cat1,ord1, real1 )
LOSS.RK11  = loss(GLRM_RK11,df.base,sdf11,cat1,ord1, real1 )
LOSS.RK12  = loss(GLRM_RK12,df.base,sdf12,cat1,ord1, real1 )
LOSS.RK13  = loss(GLRM_RK13,df.base,sdf13,cat1,ord1, real1 )
LOSS.RK14  = loss(GLRM_RK14,df.base,sdf14,cat1,ord1, real1 )
LOSS.RK15  = loss(GLRM_RK15,df.base,sdf15,cat1,ord1, real1 )
LOSS.RK16  = loss(GLRM_RK16,df.base,sdf16,cat1,ord1, real1 )
LOSS.RK17  = loss(GLRM_RK17,df.base,sdf17,cat1,ord1, real1 )
LOSS.RK18  = loss(GLRM_RK18,df.base,sdf18,cat1,ord1, real1 )
LOSS.RK19  = loss(GLRM_RK19,df.base,sdf19,cat1,ord1, real1 )
LOSS.RK20  = loss(GLRM_RK20,df.base,sdf20,cat1,ord1, real1 )


l.rk20 = c(l.rk20, LOSS.RK1$loss)
l.rk20 = c(l.rk20, LOSS.RK2$loss)
l.rk20 = c(l.rk20, LOSS.RK3$loss)
l.rk20 = c(l.rk20, LOSS.RK4$loss)
l.rk20 = c(l.rk20, LOSS.RK5$loss)
l.rk20 = c(l.rk20, LOSS.RK6$loss)
l.rk20 = c(l.rk20, LOSS.RK7$loss)
l.rk20 = c(l.rk20, LOSS.RK8$loss)
l.rk20 = c(l.rk20, LOSS.RK9$loss)
l.rk20 = c(l.rk20, LOSS.RK10$loss)
l.rk20 = c(l.rk20, LOSS.RK11$loss)
l.rk20 = c(l.rk20, LOSS.RK12$loss)
l.rk20 = c(l.rk20, LOSS.RK13$loss)
l.rk20 = c(l.rk20, LOSS.RK14$loss)
l.rk20 = c(l.rk20, LOSS.RK15$loss)
l.rk20 = c(l.rk20, LOSS.RK16$loss)
l.rk20 = c(l.rk20, LOSS.RK17$loss)
l.rk20 = c(l.rk20, LOSS.RK18$loss)
l.rk20 = c(l.rk20, LOSS.RK19$loss)
l.rk20 = c(l.rk20, LOSS.RK20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_RK1, GLRM_RK2, GLRM_RK3, GLRM_RK4, GLRM_RK5,  GLRM_RK6, GLRM_RK7, GLRM_RK8, GLRM_RK9, GLRM_RK10, 
GLRM_RK11, GLRM_RK12, GLRM_RK13, GLRM_RK14, GLRM_RK15,  GLRM_RK16, GLRM_RK17, GLRM_RK18, GLRM_RK19, GLRM_RK20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.RK1, LOSS.RK2, LOSS.RK3, LOSS.RK4, LOSS.RK5,  LOSS.RK6, LOSS.RK7, LOSS.RK8, LOSS.RK9, LOSS.RK10, 
LOSS.RK11, LOSS.RK12, LOSS.RK13, LOSS.RK14, LOSS.RK15,  LOSS.RK16, LOSS.RK17, LOSS.RK18, LOSS.RK19, LOSS.RK20 )


######## 30

glrm1 =  read.csv("Rank_GSS30_MI.csv")
glrm2 =  read.csv("Rank_GSS30_MI (1).csv")
glrm3 =  read.csv("Rank_GSS30_MI (2).csv")
glrm4 =  read.csv("Rank_GSS30_MI (3).csv")
glrm5 =  read.csv("Rank_GSS30_MI (4).csv")
glrm6 =  read.csv("Rank_GSS30_MI (5).csv")
glrm7 =  read.csv("Rank_GSS30_MI (6).csv")
glrm8 =  read.csv("Rank_GSS30_MI (7).csv")
glrm9 =  read.csv("Rank_GSS30_MI (8).csv")
glrm10 = read.csv("Rank_GSS30_MI (9).csv")
glrm11 = read.csv("Rank_GSS30_MI (10).csv")
glrm12 = read.csv("Rank_GSS30_MI (11).csv")
glrm13 = read.csv("Rank_GSS30_MI (12).csv")
glrm14 = read.csv("Rank_GSS30_MI (13).csv")
glrm15 = read.csv("Rank_GSS30_MI (14).csv")
glrm16 = read.csv("Rank_GSS30_MI (15).csv")
glrm17 = read.csv("Rank_GSS30_MI (16).csv")
glrm18 = read.csv("Rank_GSS30_MI (17).csv")
glrm19 = read.csv("Rank_GSS30_MI (18).csv")
glrm20 = read.csv("Rank_GSS30_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Rank_s30.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Rank_s30 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Rank_s30 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Rank_s30 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Rank_s30 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Rank_s30 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Rank_s30 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Rank_s30 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Rank_s30 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Rank_s30 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Rank_s30 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Rank_s30 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Rank_s30 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Rank_s30 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Rank_s30 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Rank_s30 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Rank_s30 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Rank_s30 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Rank_s30 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Rank_s30 (19).csv", header = FALSE)))


GLRM_RK1 = factorLevels(glrm1, df.base, cat1)
GLRM_RK2 = factorLevels(glrm2, df.base, cat1)
GLRM_RK3 = factorLevels(glrm3, df.base, cat1)
GLRM_RK4 = factorLevels(glrm4, df.base, cat1)
GLRM_RK5 = factorLevels(glrm5, df.base, cat1)
GLRM_RK6 = factorLevels(glrm6, df.base, cat1)
GLRM_RK7 = factorLevels(glrm7, df.base, cat1)
GLRM_RK8 = factorLevels(glrm8, df.base, cat1)
GLRM_RK9 = factorLevels(glrm9, df.base, cat1)
GLRM_RK10 = factorLevels(glrm10, df.base, cat1)
GLRM_RK11 = factorLevels(glrm11, df.base, cat1)
GLRM_RK12 = factorLevels(glrm12, df.base, cat1)
GLRM_RK13 = factorLevels(glrm13, df.base, cat1)
GLRM_RK14 = factorLevels(glrm14, df.base, cat1)
GLRM_RK15 = factorLevels(glrm15, df.base, cat1)
GLRM_RK16 = factorLevels(glrm16, df.base, cat1)
GLRM_RK17 = factorLevels(glrm17, df.base, cat1)
GLRM_RK18 = factorLevels(glrm18, df.base, cat1)
GLRM_RK19 = factorLevels(glrm19, df.base, cat1)
GLRM_RK20 = factorLevels(glrm20, df.base, cat1)

LOSS.RK1  = loss(GLRM_RK1,df.base,sdf1,cat1,ord1, real1 )
LOSS.RK2  = loss(GLRM_RK2,df.base,sdf2,cat1,ord1, real1 )
LOSS.RK3  = loss(GLRM_RK3,df.base,sdf3,cat1,ord1, real1 )
LOSS.RK4  = loss(GLRM_RK4,df.base,sdf4,cat1,ord1, real1 )
LOSS.RK5  = loss(GLRM_RK5,df.base,sdf5,cat1,ord1, real1 )
LOSS.RK6  = loss(GLRM_RK6,df.base,sdf6,cat1,ord1, real1 )
LOSS.RK7  = loss(GLRM_RK7,df.base,sdf7,cat1,ord1, real1 )
LOSS.RK8  = loss(GLRM_RK8,df.base,sdf8,cat1,ord1, real1 )
LOSS.RK9  = loss(GLRM_RK9,df.base,sdf9,cat1,ord1, real1 )
LOSS.RK10  = loss(GLRM_RK10,df.base,sdf10,cat1,ord1, real1 )
LOSS.RK11  = loss(GLRM_RK11,df.base,sdf11,cat1,ord1, real1 )
LOSS.RK12  = loss(GLRM_RK12,df.base,sdf12,cat1,ord1, real1 )
LOSS.RK13  = loss(GLRM_RK13,df.base,sdf13,cat1,ord1, real1 )
LOSS.RK14  = loss(GLRM_RK14,df.base,sdf14,cat1,ord1, real1 )
LOSS.RK15  = loss(GLRM_RK15,df.base,sdf15,cat1,ord1, real1 )
LOSS.RK16  = loss(GLRM_RK16,df.base,sdf16,cat1,ord1, real1 )
LOSS.RK17  = loss(GLRM_RK17,df.base,sdf17,cat1,ord1, real1 )
LOSS.RK18  = loss(GLRM_RK18,df.base,sdf18,cat1,ord1, real1 )
LOSS.RK19  = loss(GLRM_RK19,df.base,sdf19,cat1,ord1, real1 )
LOSS.RK20  = loss(GLRM_RK20,df.base,sdf20,cat1,ord1, real1 )


l.rk30 = c(l.rk30, LOSS.RK1$loss)
l.rk30 = c(l.rk30, LOSS.RK2$loss)
l.rk30 = c(l.rk30, LOSS.RK3$loss)
l.rk30 = c(l.rk30, LOSS.RK4$loss)
l.rk30 = c(l.rk30, LOSS.RK5$loss)
l.rk30 = c(l.rk30, LOSS.RK6$loss)
l.rk30 = c(l.rk30, LOSS.RK7$loss)
l.rk30 = c(l.rk30, LOSS.RK8$loss)
l.rk30 = c(l.rk30, LOSS.RK9$loss)
l.rk30 = c(l.rk30, LOSS.RK10$loss)
l.rk30 = c(l.rk30, LOSS.RK11$loss)
l.rk30 = c(l.rk30, LOSS.RK12$loss)
l.rk30 = c(l.rk30, LOSS.RK13$loss)
l.rk30 = c(l.rk30, LOSS.RK14$loss)
l.rk30 = c(l.rk30, LOSS.RK15$loss)
l.rk30 = c(l.rk30, LOSS.RK16$loss)
l.rk30 = c(l.rk30, LOSS.RK17$loss)
l.rk30 = c(l.rk30, LOSS.RK18$loss)
l.rk30 = c(l.rk30, LOSS.RK19$loss)
l.rk30 = c(l.rk30, LOSS.RK20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_RK1, GLRM_RK2, GLRM_RK3, GLRM_RK4, GLRM_RK5,  GLRM_RK6, GLRM_RK7, GLRM_RK8, GLRM_RK9, GLRM_RK10, 
GLRM_RK11, GLRM_RK12, GLRM_RK13, GLRM_RK14, GLRM_RK15,  GLRM_RK16, GLRM_RK17, GLRM_RK18, GLRM_RK19, GLRM_RK20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.RK1, LOSS.RK2, LOSS.RK3, LOSS.RK4, LOSS.RK5,  LOSS.RK6, LOSS.RK7, LOSS.RK8, LOSS.RK9, LOSS.RK10, 
LOSS.RK11, LOSS.RK12, LOSS.RK13, LOSS.RK14, LOSS.RK15,  LOSS.RK16, LOSS.RK17, LOSS.RK18, LOSS.RK19, LOSS.RK20 )



######## 40

glrm1 =  read.csv("Rank_GSS40_MI.csv")
glrm2 =  read.csv("Rank_GSS40_MI (1).csv")
glrm3 =  read.csv("Rank_GSS40_MI (2).csv")
glrm4 =  read.csv("Rank_GSS40_MI (3).csv")
glrm5 =  read.csv("Rank_GSS40_MI (4).csv")
glrm6 =  read.csv("Rank_GSS40_MI (5).csv")
glrm7 =  read.csv("Rank_GSS40_MI (6).csv")
glrm8 =  read.csv("Rank_GSS40_MI (7).csv")
glrm9 =  read.csv("Rank_GSS40_MI (8).csv")
glrm10 = read.csv("Rank_GSS40_MI (9).csv")
glrm11 = read.csv("Rank_GSS40_MI (10).csv")
glrm12 = read.csv("Rank_GSS40_MI (11).csv")
glrm13 = read.csv("Rank_GSS40_MI (12).csv")
glrm14 = read.csv("Rank_GSS40_MI (13).csv")
glrm15 = read.csv("Rank_GSS40_MI (14).csv")
glrm16 = read.csv("Rank_GSS40_MI (15).csv")
glrm17 = read.csv("Rank_GSS40_MI (16).csv")
glrm18 = read.csv("Rank_GSS40_MI (17).csv")
glrm19 = read.csv("Rank_GSS40_MI (18).csv")
glrm20 = read.csv("Rank_GSS40_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Rank_s40.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Rank_s40 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Rank_s40 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Rank_s40 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Rank_s40 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Rank_s40 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Rank_s40 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Rank_s40 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Rank_s40 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Rank_s40 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Rank_s40 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Rank_s40 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Rank_s40 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Rank_s40 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Rank_s40 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Rank_s40 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Rank_s40 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Rank_s40 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Rank_s40 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Rank_s40 (19).csv", header = FALSE)))


GLRM_RK1 = factorLevels(glrm1, df.base, cat1)
GLRM_RK2 = factorLevels(glrm2, df.base, cat1)
GLRM_RK3 = factorLevels(glrm3, df.base, cat1)
GLRM_RK4 = factorLevels(glrm4, df.base, cat1)
GLRM_RK5 = factorLevels(glrm5, df.base, cat1)
GLRM_RK6 = factorLevels(glrm6, df.base, cat1)
GLRM_RK7 = factorLevels(glrm7, df.base, cat1)
GLRM_RK8 = factorLevels(glrm8, df.base, cat1)
GLRM_RK9 = factorLevels(glrm9, df.base, cat1)
GLRM_RK10 = factorLevels(glrm10, df.base, cat1)
GLRM_RK11 = factorLevels(glrm11, df.base, cat1)
GLRM_RK12 = factorLevels(glrm12, df.base, cat1)
GLRM_RK13 = factorLevels(glrm13, df.base, cat1)
GLRM_RK14 = factorLevels(glrm14, df.base, cat1)
GLRM_RK15 = factorLevels(glrm15, df.base, cat1)
GLRM_RK16 = factorLevels(glrm16, df.base, cat1)
GLRM_RK17 = factorLevels(glrm17, df.base, cat1)
GLRM_RK18 = factorLevels(glrm18, df.base, cat1)
GLRM_RK19 = factorLevels(glrm19, df.base, cat1)
GLRM_RK20 = factorLevels(glrm20, df.base, cat1)

LOSS.RK1  = loss(GLRM_RK1,df.base,sdf1,cat1,ord1, real1 )
LOSS.RK2  = loss(GLRM_RK2,df.base,sdf2,cat1,ord1, real1 )
LOSS.RK3  = loss(GLRM_RK3,df.base,sdf3,cat1,ord1, real1 )
LOSS.RK4  = loss(GLRM_RK4,df.base,sdf4,cat1,ord1, real1 )
LOSS.RK5  = loss(GLRM_RK5,df.base,sdf5,cat1,ord1, real1 )
LOSS.RK6  = loss(GLRM_RK6,df.base,sdf6,cat1,ord1, real1 )
LOSS.RK7  = loss(GLRM_RK7,df.base,sdf7,cat1,ord1, real1 )
LOSS.RK8  = loss(GLRM_RK8,df.base,sdf8,cat1,ord1, real1 )
LOSS.RK9  = loss(GLRM_RK9,df.base,sdf9,cat1,ord1, real1 )
LOSS.RK10  = loss(GLRM_RK10,df.base,sdf10,cat1,ord1, real1 )
LOSS.RK11  = loss(GLRM_RK11,df.base,sdf11,cat1,ord1, real1 )
LOSS.RK12  = loss(GLRM_RK12,df.base,sdf12,cat1,ord1, real1 )
LOSS.RK13  = loss(GLRM_RK13,df.base,sdf13,cat1,ord1, real1 )
LOSS.RK14  = loss(GLRM_RK14,df.base,sdf14,cat1,ord1, real1 )
LOSS.RK15  = loss(GLRM_RK15,df.base,sdf15,cat1,ord1, real1 )
LOSS.RK16  = loss(GLRM_RK16,df.base,sdf16,cat1,ord1, real1 )
LOSS.RK17  = loss(GLRM_RK17,df.base,sdf17,cat1,ord1, real1 )
LOSS.RK18  = loss(GLRM_RK18,df.base,sdf18,cat1,ord1, real1 )
LOSS.RK19  = loss(GLRM_RK19,df.base,sdf19,cat1,ord1, real1 )
LOSS.RK20  = loss(GLRM_RK20,df.base,sdf20,cat1,ord1, real1 )


l.rk40 = c(l.rk40, LOSS.RK1$loss)
l.rk40 = c(l.rk40, LOSS.RK2$loss)
l.rk40 = c(l.rk40, LOSS.RK3$loss)
l.rk40 = c(l.rk40, LOSS.RK4$loss)
l.rk40 = c(l.rk40, LOSS.RK5$loss)
l.rk40 = c(l.rk40, LOSS.RK6$loss)
l.rk40 = c(l.rk40, LOSS.RK7$loss)
l.rk40 = c(l.rk40, LOSS.RK8$loss)
l.rk40 = c(l.rk40, LOSS.RK9$loss)
l.rk40 = c(l.rk40, LOSS.RK10$loss)
l.rk40 = c(l.rk40, LOSS.RK11$loss)
l.rk40 = c(l.rk40, LOSS.RK12$loss)
l.rk40 = c(l.rk40, LOSS.RK13$loss)
l.rk40 = c(l.rk40, LOSS.RK14$loss)
l.rk40 = c(l.rk40, LOSS.RK15$loss)
l.rk40 = c(l.rk40, LOSS.RK16$loss)
l.rk40 = c(l.rk40, LOSS.RK17$loss)
l.rk40 = c(l.rk40, LOSS.RK18$loss)
l.rk40 = c(l.rk40, LOSS.RK19$loss)
l.rk40 = c(l.rk40, LOSS.RK20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_RK1, GLRM_RK2, GLRM_RK3, GLRM_RK4, GLRM_RK5,  GLRM_RK6, GLRM_RK7, GLRM_RK8, GLRM_RK9, GLRM_RK10, 
GLRM_RK11, GLRM_RK12, GLRM_RK13, GLRM_RK14, GLRM_RK15,  GLRM_RK16, GLRM_RK17, GLRM_RK18, GLRM_RK19, GLRM_RK20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.RK1, LOSS.RK2, LOSS.RK3, LOSS.RK4, LOSS.RK5,  LOSS.RK6, LOSS.RK7, LOSS.RK8, LOSS.RK9, LOSS.RK10, 
LOSS.RK11, LOSS.RK12, LOSS.RK13, LOSS.RK14, LOSS.RK15,  LOSS.RK16, LOSS.RK17, LOSS.RK18, LOSS.RK19, LOSS.RK20 )


######## 50

glrm1 =  read.csv("Rank_GSS50_MI.csv")
glrm2 =  read.csv("Rank_GSS50_MI (1).csv")
glrm3 =  read.csv("Rank_GSS50_MI (2).csv")
glrm4 =  read.csv("Rank_GSS50_MI (3).csv")
glrm5 =  read.csv("Rank_GSS50_MI (4).csv")
glrm6 =  read.csv("Rank_GSS50_MI (5).csv")
glrm7 =  read.csv("Rank_GSS50_MI (6).csv")
glrm8 =  read.csv("Rank_GSS50_MI (7).csv")
glrm9 =  read.csv("Rank_GSS50_MI (8).csv")
glrm10 = read.csv("Rank_GSS50_MI (9).csv")
glrm11 = read.csv("Rank_GSS50_MI (10).csv")
glrm12 = read.csv("Rank_GSS50_MI (11).csv")
glrm13 = read.csv("Rank_GSS50_MI (12).csv")
glrm14 = read.csv("Rank_GSS50_MI (13).csv")
glrm15 = read.csv("Rank_GSS50_MI (14).csv")
glrm16 = read.csv("Rank_GSS50_MI (15).csv")
glrm17 = read.csv("Rank_GSS50_MI (16).csv")
glrm18 = read.csv("Rank_GSS50_MI (17).csv")
glrm19 = read.csv("Rank_GSS50_MI (18).csv")
glrm20 = read.csv("Rank_GSS50_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Rank_s50.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Rank_s50 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Rank_s50 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Rank_s50 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Rank_s50 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Rank_s50 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Rank_s50 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Rank_s50 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Rank_s50 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Rank_s50 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Rank_s50 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Rank_s50 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Rank_s50 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Rank_s50 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Rank_s50 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Rank_s50 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Rank_s50 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Rank_s50 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Rank_s50 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Rank_s50 (19).csv", header = FALSE)))


GLRM_RK1 = factorLevels(glrm1, df.base, cat1)
GLRM_RK2 = factorLevels(glrm2, df.base, cat1)
GLRM_RK3 = factorLevels(glrm3, df.base, cat1)
GLRM_RK4 = factorLevels(glrm4, df.base, cat1)
GLRM_RK5 = factorLevels(glrm5, df.base, cat1)
GLRM_RK6 = factorLevels(glrm6, df.base, cat1)
GLRM_RK7 = factorLevels(glrm7, df.base, cat1)
GLRM_RK8 = factorLevels(glrm8, df.base, cat1)
GLRM_RK9 = factorLevels(glrm9, df.base, cat1)
GLRM_RK10 = factorLevels(glrm10, df.base, cat1)
GLRM_RK11 = factorLevels(glrm11, df.base, cat1)
GLRM_RK12 = factorLevels(glrm12, df.base, cat1)
GLRM_RK13 = factorLevels(glrm13, df.base, cat1)
GLRM_RK14 = factorLevels(glrm14, df.base, cat1)
GLRM_RK15 = factorLevels(glrm15, df.base, cat1)
GLRM_RK16 = factorLevels(glrm16, df.base, cat1)
GLRM_RK17 = factorLevels(glrm17, df.base, cat1)
GLRM_RK18 = factorLevels(glrm18, df.base, cat1)
GLRM_RK19 = factorLevels(glrm19, df.base, cat1)
GLRM_RK20 = factorLevels(glrm20, df.base, cat1)

LOSS.RK1  = loss(GLRM_RK1,df.base,sdf1,cat1,ord1, real1 )
LOSS.RK2  = loss(GLRM_RK2,df.base,sdf2,cat1,ord1, real1 )
LOSS.RK3  = loss(GLRM_RK3,df.base,sdf3,cat1,ord1, real1 )
LOSS.RK4  = loss(GLRM_RK4,df.base,sdf4,cat1,ord1, real1 )
LOSS.RK5  = loss(GLRM_RK5,df.base,sdf5,cat1,ord1, real1 )
LOSS.RK6  = loss(GLRM_RK6,df.base,sdf6,cat1,ord1, real1 )
LOSS.RK7  = loss(GLRM_RK7,df.base,sdf7,cat1,ord1, real1 )
LOSS.RK8  = loss(GLRM_RK8,df.base,sdf8,cat1,ord1, real1 )
LOSS.RK9  = loss(GLRM_RK9,df.base,sdf9,cat1,ord1, real1 )
LOSS.RK10  = loss(GLRM_RK10,df.base,sdf10,cat1,ord1, real1 )
LOSS.RK11  = loss(GLRM_RK11,df.base,sdf11,cat1,ord1, real1 )
LOSS.RK12  = loss(GLRM_RK12,df.base,sdf12,cat1,ord1, real1 )
LOSS.RK13  = loss(GLRM_RK13,df.base,sdf13,cat1,ord1, real1 )
LOSS.RK14  = loss(GLRM_RK14,df.base,sdf14,cat1,ord1, real1 )
LOSS.RK15  = loss(GLRM_RK15,df.base,sdf15,cat1,ord1, real1 )
LOSS.RK16  = loss(GLRM_RK16,df.base,sdf16,cat1,ord1, real1 )
LOSS.RK17  = loss(GLRM_RK17,df.base,sdf17,cat1,ord1, real1 )
LOSS.RK18  = loss(GLRM_RK18,df.base,sdf18,cat1,ord1, real1 )
LOSS.RK19  = loss(GLRM_RK19,df.base,sdf19,cat1,ord1, real1 )
LOSS.RK20  = loss(GLRM_RK20,df.base,sdf20,cat1,ord1, real1 )


l.rk50 = c(l.rk50, LOSS.RK1$loss)
l.rk50 = c(l.rk50, LOSS.RK2$loss)
l.rk50 = c(l.rk50, LOSS.RK3$loss)
l.rk50 = c(l.rk50, LOSS.RK4$loss)
l.rk50 = c(l.rk50, LOSS.RK5$loss)
l.rk50 = c(l.rk50, LOSS.RK6$loss)
l.rk50 = c(l.rk50, LOSS.RK7$loss)
l.rk50 = c(l.rk50, LOSS.RK8$loss)
l.rk50 = c(l.rk50, LOSS.RK9$loss)
l.rk50 = c(l.rk50, LOSS.RK10$loss)
l.rk50 = c(l.rk50, LOSS.RK11$loss)
l.rk50 = c(l.rk50, LOSS.RK12$loss)
l.rk50 = c(l.rk50, LOSS.RK13$loss)
l.rk50 = c(l.rk50, LOSS.RK14$loss)
l.rk50 = c(l.rk50, LOSS.RK15$loss)
l.rk50 = c(l.rk50, LOSS.RK16$loss)
l.rk50 = c(l.rk50, LOSS.RK17$loss)
l.rk50 = c(l.rk50, LOSS.RK18$loss)
l.rk50 = c(l.rk50, LOSS.RK19$loss)
l.rk50 = c(l.rk50, LOSS.RK20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_RK1, GLRM_RK2, GLRM_RK3, GLRM_RK4, GLRM_RK5,  GLRM_RK6, GLRM_RK7, GLRM_RK8, GLRM_RK9, GLRM_RK10, 
GLRM_RK11, GLRM_RK12, GLRM_RK13, GLRM_RK14, GLRM_RK15,  GLRM_RK16, GLRM_RK17, GLRM_RK18, GLRM_RK19, GLRM_RK20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.RK1, LOSS.RK2, LOSS.RK3, LOSS.RK4, LOSS.RK5,  LOSS.RK6, LOSS.RK7, LOSS.RK8, LOSS.RK9, LOSS.RK10, 
LOSS.RK11, LOSS.RK12, LOSS.RK13, LOSS.RK14, LOSS.RK15,  LOSS.RK16, LOSS.RK17, LOSS.RK18, LOSS.RK19, LOSS.RK20 )


######## 60

glrm1 =  read.csv("Rank_GSS60_MI.csv")
glrm2 =  read.csv("Rank_GSS60_MI (1).csv")
glrm3 =  read.csv("Rank_GSS60_MI (2).csv")
glrm4 =  read.csv("Rank_GSS60_MI (3).csv")
glrm5 =  read.csv("Rank_GSS60_MI (4).csv")
glrm6 =  read.csv("Rank_GSS60_MI (5).csv")
glrm7 =  read.csv("Rank_GSS60_MI (6).csv")
glrm8 =  read.csv("Rank_GSS60_MI (7).csv")
glrm9 =  read.csv("Rank_GSS60_MI (8).csv")
glrm10 = read.csv("Rank_GSS60_MI (9).csv")
glrm11 = read.csv("Rank_GSS60_MI (10).csv")
glrm12 = read.csv("Rank_GSS60_MI (11).csv")
glrm13 = read.csv("Rank_GSS60_MI (12).csv")
glrm14 = read.csv("Rank_GSS60_MI (13).csv")
glrm15 = read.csv("Rank_GSS60_MI (14).csv")
glrm16 = read.csv("Rank_GSS60_MI (15).csv")
glrm17 = read.csv("Rank_GSS60_MI (16).csv")
glrm18 = read.csv("Rank_GSS60_MI (17).csv")
glrm19 = read.csv("Rank_GSS60_MI (18).csv")
glrm20 = read.csv("Rank_GSS60_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Rank_s60.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Rank_s60 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Rank_s60 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Rank_s60 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Rank_s60 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Rank_s60 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Rank_s60 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Rank_s60 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Rank_s60 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Rank_s60 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Rank_s60 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Rank_s60 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Rank_s60 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Rank_s60 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Rank_s60 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Rank_s60 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Rank_s60 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Rank_s60 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Rank_s60 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Rank_s60 (19).csv", header = FALSE)))


GLRM_RK1 = factorLevels(glrm1, df.base, cat1)
GLRM_RK2 = factorLevels(glrm2, df.base, cat1)
GLRM_RK3 = factorLevels(glrm3, df.base, cat1)
GLRM_RK4 = factorLevels(glrm4, df.base, cat1)
GLRM_RK5 = factorLevels(glrm5, df.base, cat1)
GLRM_RK6 = factorLevels(glrm6, df.base, cat1)
GLRM_RK7 = factorLevels(glrm7, df.base, cat1)
GLRM_RK8 = factorLevels(glrm8, df.base, cat1)
GLRM_RK9 = factorLevels(glrm9, df.base, cat1)
GLRM_RK10 = factorLevels(glrm10, df.base, cat1)
GLRM_RK11 = factorLevels(glrm11, df.base, cat1)
GLRM_RK12 = factorLevels(glrm12, df.base, cat1)
GLRM_RK13 = factorLevels(glrm13, df.base, cat1)
GLRM_RK14 = factorLevels(glrm14, df.base, cat1)
GLRM_RK15 = factorLevels(glrm15, df.base, cat1)
GLRM_RK16 = factorLevels(glrm16, df.base, cat1)
GLRM_RK17 = factorLevels(glrm17, df.base, cat1)
GLRM_RK18 = factorLevels(glrm18, df.base, cat1)
GLRM_RK19 = factorLevels(glrm19, df.base, cat1)
GLRM_RK20 = factorLevels(glrm20, df.base, cat1)

LOSS.RK1  = loss(GLRM_RK1,df.base,sdf1,cat1,ord1, real1 )
LOSS.RK2  = loss(GLRM_RK2,df.base,sdf2,cat1,ord1, real1 )
LOSS.RK3  = loss(GLRM_RK3,df.base,sdf3,cat1,ord1, real1 )
LOSS.RK4  = loss(GLRM_RK4,df.base,sdf4,cat1,ord1, real1 )
LOSS.RK5  = loss(GLRM_RK5,df.base,sdf5,cat1,ord1, real1 )
LOSS.RK6  = loss(GLRM_RK6,df.base,sdf6,cat1,ord1, real1 )
LOSS.RK7  = loss(GLRM_RK7,df.base,sdf7,cat1,ord1, real1 )
LOSS.RK8  = loss(GLRM_RK8,df.base,sdf8,cat1,ord1, real1 )
LOSS.RK9  = loss(GLRM_RK9,df.base,sdf9,cat1,ord1, real1 )
LOSS.RK10  = loss(GLRM_RK10,df.base,sdf10,cat1,ord1, real1 )
LOSS.RK11  = loss(GLRM_RK11,df.base,sdf11,cat1,ord1, real1 )
LOSS.RK12  = loss(GLRM_RK12,df.base,sdf12,cat1,ord1, real1 )
LOSS.RK13  = loss(GLRM_RK13,df.base,sdf13,cat1,ord1, real1 )
LOSS.RK14  = loss(GLRM_RK14,df.base,sdf14,cat1,ord1, real1 )
LOSS.RK15  = loss(GLRM_RK15,df.base,sdf15,cat1,ord1, real1 )
LOSS.RK16  = loss(GLRM_RK16,df.base,sdf16,cat1,ord1, real1 )
LOSS.RK17  = loss(GLRM_RK17,df.base,sdf17,cat1,ord1, real1 )
LOSS.RK18  = loss(GLRM_RK18,df.base,sdf18,cat1,ord1, real1 )
LOSS.RK19  = loss(GLRM_RK19,df.base,sdf19,cat1,ord1, real1 )
LOSS.RK20  = loss(GLRM_RK20,df.base,sdf20,cat1,ord1, real1 )


l.rk60 = c(l.rk60, LOSS.RK1$loss)
l.rk60 = c(l.rk60, LOSS.RK2$loss)
l.rk60 = c(l.rk60, LOSS.RK3$loss)
l.rk60 = c(l.rk60, LOSS.RK4$loss)
l.rk60 = c(l.rk60, LOSS.RK5$loss)
l.rk60 = c(l.rk60, LOSS.RK6$loss)
l.rk60 = c(l.rk60, LOSS.RK7$loss)
l.rk60 = c(l.rk60, LOSS.RK8$loss)
l.rk60 = c(l.rk60, LOSS.RK9$loss)
l.rk60 = c(l.rk60, LOSS.RK10$loss)
l.rk60 = c(l.rk60, LOSS.RK11$loss)
l.rk60 = c(l.rk60, LOSS.RK12$loss)
l.rk60 = c(l.rk60, LOSS.RK13$loss)
l.rk60 = c(l.rk60, LOSS.RK14$loss)
l.rk60 = c(l.rk60, LOSS.RK15$loss)
l.rk60 = c(l.rk60, LOSS.RK16$loss)
l.rk60 = c(l.rk60, LOSS.RK17$loss)
l.rk60 = c(l.rk60, LOSS.RK18$loss)
l.rk60 = c(l.rk60, LOSS.RK19$loss)
l.rk60 = c(l.rk60, LOSS.RK20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_RK1, GLRM_RK2, GLRM_RK3, GLRM_RK4, GLRM_RK5,  GLRM_RK6, GLRM_RK7, GLRM_RK8, GLRM_RK9, GLRM_RK10, 
GLRM_RK11, GLRM_RK12, GLRM_RK13, GLRM_RK14, GLRM_RK15,  GLRM_RK16, GLRM_RK17, GLRM_RK18, GLRM_RK19, GLRM_RK20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.RK1, LOSS.RK2, LOSS.RK3, LOSS.RK4, LOSS.RK5,  LOSS.RK6, LOSS.RK7, LOSS.RK8, LOSS.RK9, LOSS.RK10, 
LOSS.RK11, LOSS.RK12, LOSS.RK13, LOSS.RK14, LOSS.RK15,  LOSS.RK16, LOSS.RK17, LOSS.RK18, LOSS.RK19, LOSS.RK20 )


######## 70

glrm1 =  read.csv("Rank_GSS70_MI.csv")
glrm2 =  read.csv("Rank_GSS70_MI (1).csv")
glrm3 =  read.csv("Rank_GSS70_MI (2).csv")
glrm4 =  read.csv("Rank_GSS70_MI (3).csv")
glrm5 =  read.csv("Rank_GSS70_MI (4).csv")
glrm6 =  read.csv("Rank_GSS70_MI (5).csv")
glrm7 =  read.csv("Rank_GSS70_MI (6).csv")
glrm8 =  read.csv("Rank_GSS70_MI (7).csv")
glrm9 =  read.csv("Rank_GSS70_MI (8).csv")
glrm10 = read.csv("Rank_GSS70_MI (9).csv")
glrm11 = read.csv("Rank_GSS70_MI (10).csv")
glrm12 = read.csv("Rank_GSS70_MI (11).csv")
glrm13 = read.csv("Rank_GSS70_MI (12).csv")
glrm14 = read.csv("Rank_GSS70_MI (13).csv")
glrm15 = read.csv("Rank_GSS70_MI (14).csv")
glrm16 = read.csv("Rank_GSS70_MI (15).csv")
glrm17 = read.csv("Rank_GSS70_MI (16).csv")
glrm18 = read.csv("Rank_GSS70_MI (17).csv")
glrm19 = read.csv("Rank_GSS70_MI (18).csv")
glrm20 = read.csv("Rank_GSS70_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Rank_s70.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Rank_s70 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Rank_s70 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Rank_s70 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Rank_s70 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Rank_s70 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Rank_s70 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Rank_s70 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Rank_s70 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Rank_s70 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Rank_s70 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Rank_s70 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Rank_s70 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Rank_s70 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Rank_s70 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Rank_s70 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Rank_s70 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Rank_s70 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Rank_s70 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Rank_s70 (19).csv", header = FALSE)))


GLRM_RK1 = factorLevels(glrm1, df.base, cat1)
GLRM_RK2 = factorLevels(glrm2, df.base, cat1)
GLRM_RK3 = factorLevels(glrm3, df.base, cat1)
GLRM_RK4 = factorLevels(glrm4, df.base, cat1)
GLRM_RK5 = factorLevels(glrm5, df.base, cat1)
GLRM_RK6 = factorLevels(glrm6, df.base, cat1)
GLRM_RK7 = factorLevels(glrm7, df.base, cat1)
GLRM_RK8 = factorLevels(glrm8, df.base, cat1)
GLRM_RK9 = factorLevels(glrm9, df.base, cat1)
GLRM_RK10 = factorLevels(glrm10, df.base, cat1)
GLRM_RK11 = factorLevels(glrm11, df.base, cat1)
GLRM_RK12 = factorLevels(glrm12, df.base, cat1)
GLRM_RK13 = factorLevels(glrm13, df.base, cat1)
GLRM_RK14 = factorLevels(glrm14, df.base, cat1)
GLRM_RK15 = factorLevels(glrm15, df.base, cat1)
GLRM_RK16 = factorLevels(glrm16, df.base, cat1)
GLRM_RK17 = factorLevels(glrm17, df.base, cat1)
GLRM_RK18 = factorLevels(glrm18, df.base, cat1)
GLRM_RK19 = factorLevels(glrm19, df.base, cat1)
GLRM_RK20 = factorLevels(glrm20, df.base, cat1)

LOSS.RK1  = loss(GLRM_RK1,df.base,sdf1,cat1,ord1, real1 )
LOSS.RK2  = loss(GLRM_RK2,df.base,sdf2,cat1,ord1, real1 )
LOSS.RK3  = loss(GLRM_RK3,df.base,sdf3,cat1,ord1, real1 )
LOSS.RK4  = loss(GLRM_RK4,df.base,sdf4,cat1,ord1, real1 )
LOSS.RK5  = loss(GLRM_RK5,df.base,sdf5,cat1,ord1, real1 )
LOSS.RK6  = loss(GLRM_RK6,df.base,sdf6,cat1,ord1, real1 )
LOSS.RK7  = loss(GLRM_RK7,df.base,sdf7,cat1,ord1, real1 )
LOSS.RK8  = loss(GLRM_RK8,df.base,sdf8,cat1,ord1, real1 )
LOSS.RK9  = loss(GLRM_RK9,df.base,sdf9,cat1,ord1, real1 )
LOSS.RK10  = loss(GLRM_RK10,df.base,sdf10,cat1,ord1, real1 )
LOSS.RK11  = loss(GLRM_RK11,df.base,sdf11,cat1,ord1, real1 )
LOSS.RK12  = loss(GLRM_RK12,df.base,sdf12,cat1,ord1, real1 )
LOSS.RK13  = loss(GLRM_RK13,df.base,sdf13,cat1,ord1, real1 )
LOSS.RK14  = loss(GLRM_RK14,df.base,sdf14,cat1,ord1, real1 )
LOSS.RK15  = loss(GLRM_RK15,df.base,sdf15,cat1,ord1, real1 )
LOSS.RK16  = loss(GLRM_RK16,df.base,sdf16,cat1,ord1, real1 )
LOSS.RK17  = loss(GLRM_RK17,df.base,sdf17,cat1,ord1, real1 )
LOSS.RK18  = loss(GLRM_RK18,df.base,sdf18,cat1,ord1, real1 )
LOSS.RK19  = loss(GLRM_RK19,df.base,sdf19,cat1,ord1, real1 )
LOSS.RK20  = loss(GLRM_RK20,df.base,sdf20,cat1,ord1, real1 )


l.rk70 = c(l.rk70, LOSS.RK1$loss)
l.rk70 = c(l.rk70, LOSS.RK2$loss)
l.rk70 = c(l.rk70, LOSS.RK3$loss)
l.rk70 = c(l.rk70, LOSS.RK4$loss)
l.rk70 = c(l.rk70, LOSS.RK5$loss)
l.rk70 = c(l.rk70, LOSS.RK6$loss)
l.rk70 = c(l.rk70, LOSS.RK7$loss)
l.rk70 = c(l.rk70, LOSS.RK8$loss)
l.rk70 = c(l.rk70, LOSS.RK9$loss)
l.rk70 = c(l.rk70, LOSS.RK10$loss)
l.rk70 = c(l.rk70, LOSS.RK11$loss)
l.rk70 = c(l.rk70, LOSS.RK12$loss)
l.rk70 = c(l.rk70, LOSS.RK13$loss)
l.rk70 = c(l.rk70, LOSS.RK14$loss)
l.rk70 = c(l.rk70, LOSS.RK15$loss)
l.rk70 = c(l.rk70, LOSS.RK16$loss)
l.rk70 = c(l.rk70, LOSS.RK17$loss)
l.rk70 = c(l.rk70, LOSS.RK18$loss)
l.rk70 = c(l.rk70, LOSS.RK19$loss)
l.rk70 = c(l.rk70, LOSS.RK20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_RK1, GLRM_RK2, GLRM_RK3, GLRM_RK4, GLRM_RK5,  GLRM_RK6, GLRM_RK7, GLRM_RK8, GLRM_RK9, GLRM_RK10, 
GLRM_RK11, GLRM_RK12, GLRM_RK13, GLRM_RK14, GLRM_RK15,  GLRM_RK16, GLRM_RK17, GLRM_RK18, GLRM_RK19, GLRM_RK20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.RK1, LOSS.RK2, LOSS.RK3, LOSS.RK4, LOSS.RK5,  LOSS.RK6, LOSS.RK7, LOSS.RK8, LOSS.RK9, LOSS.RK10, 
LOSS.RK11, LOSS.RK12, LOSS.RK13, LOSS.RK14, LOSS.RK15,  LOSS.RK16, LOSS.RK17, LOSS.RK18, LOSS.RK19, LOSS.RK20 )



######## 80

glrm1 =  read.csv("Rank_GSS80_MI.csv")
glrm2 =  read.csv("Rank_GSS80_MI (1).csv")
glrm3 =  read.csv("Rank_GSS80_MI (2).csv")
glrm4 =  read.csv("Rank_GSS80_MI (3).csv")
glrm5 =  read.csv("Rank_GSS80_MI (4).csv")
glrm6 =  read.csv("Rank_GSS80_MI (5).csv")
glrm7 =  read.csv("Rank_GSS80_MI (6).csv")
glrm8 =  read.csv("Rank_GSS80_MI (7).csv")
glrm9 =  read.csv("Rank_GSS80_MI (8).csv")
glrm10 = read.csv("Rank_GSS80_MI (9).csv")
glrm11 = read.csv("Rank_GSS80_MI (10).csv")
glrm12 = read.csv("Rank_GSS80_MI (11).csv")
glrm13 = read.csv("Rank_GSS80_MI (12).csv")
glrm14 = read.csv("Rank_GSS80_MI (13).csv")
glrm15 = read.csv("Rank_GSS80_MI (14).csv")
glrm16 = read.csv("Rank_GSS80_MI (15).csv")
glrm17 = read.csv("Rank_GSS80_MI (16).csv")
glrm18 = read.csv("Rank_GSS80_MI (17).csv")
glrm19 = read.csv("Rank_GSS80_MI (18).csv")
glrm20 = read.csv("Rank_GSS80_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Rank_s80.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Rank_s80 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Rank_s80 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Rank_s80 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Rank_s80 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Rank_s80 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Rank_s80 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Rank_s80 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Rank_s80 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Rank_s80 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Rank_s80 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Rank_s80 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Rank_s80 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Rank_s80 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Rank_s80 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Rank_s80 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Rank_s80 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Rank_s80 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Rank_s80 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Rank_s80 (19).csv", header = FALSE)))


GLRM_RK1 = factorLevels(glrm1, df.base, cat1)
GLRM_RK2 = factorLevels(glrm2, df.base, cat1)
GLRM_RK3 = factorLevels(glrm3, df.base, cat1)
GLRM_RK4 = factorLevels(glrm4, df.base, cat1)
GLRM_RK5 = factorLevels(glrm5, df.base, cat1)
GLRM_RK6 = factorLevels(glrm6, df.base, cat1)
GLRM_RK7 = factorLevels(glrm7, df.base, cat1)
GLRM_RK8 = factorLevels(glrm8, df.base, cat1)
GLRM_RK9 = factorLevels(glrm9, df.base, cat1)
GLRM_RK10 = factorLevels(glrm10, df.base, cat1)
GLRM_RK11 = factorLevels(glrm11, df.base, cat1)
GLRM_RK12 = factorLevels(glrm12, df.base, cat1)
GLRM_RK13 = factorLevels(glrm13, df.base, cat1)
GLRM_RK14 = factorLevels(glrm14, df.base, cat1)
GLRM_RK15 = factorLevels(glrm15, df.base, cat1)
GLRM_RK16 = factorLevels(glrm16, df.base, cat1)
GLRM_RK17 = factorLevels(glrm17, df.base, cat1)
GLRM_RK18 = factorLevels(glrm18, df.base, cat1)
GLRM_RK19 = factorLevels(glrm19, df.base, cat1)
GLRM_RK20 = factorLevels(glrm20, df.base, cat1)

LOSS.RK1  = loss(GLRM_RK1,df.base,sdf1,cat1,ord1, real1 )
LOSS.RK2  = loss(GLRM_RK2,df.base,sdf2,cat1,ord1, real1 )
LOSS.RK3  = loss(GLRM_RK3,df.base,sdf3,cat1,ord1, real1 )
LOSS.RK4  = loss(GLRM_RK4,df.base,sdf4,cat1,ord1, real1 )
LOSS.RK5  = loss(GLRM_RK5,df.base,sdf5,cat1,ord1, real1 )
LOSS.RK6  = loss(GLRM_RK6,df.base,sdf6,cat1,ord1, real1 )
LOSS.RK7  = loss(GLRM_RK7,df.base,sdf7,cat1,ord1, real1 )
LOSS.RK8  = loss(GLRM_RK8,df.base,sdf8,cat1,ord1, real1 )
LOSS.RK9  = loss(GLRM_RK9,df.base,sdf9,cat1,ord1, real1 )
LOSS.RK10  = loss(GLRM_RK10,df.base,sdf10,cat1,ord1, real1 )
LOSS.RK11  = loss(GLRM_RK11,df.base,sdf11,cat1,ord1, real1 )
LOSS.RK12  = loss(GLRM_RK12,df.base,sdf12,cat1,ord1, real1 )
LOSS.RK13  = loss(GLRM_RK13,df.base,sdf13,cat1,ord1, real1 )
LOSS.RK14  = loss(GLRM_RK14,df.base,sdf14,cat1,ord1, real1 )
LOSS.RK15  = loss(GLRM_RK15,df.base,sdf15,cat1,ord1, real1 )
LOSS.RK16  = loss(GLRM_RK16,df.base,sdf16,cat1,ord1, real1 )
LOSS.RK17  = loss(GLRM_RK17,df.base,sdf17,cat1,ord1, real1 )
LOSS.RK18  = loss(GLRM_RK18,df.base,sdf18,cat1,ord1, real1 )
LOSS.RK19  = loss(GLRM_RK19,df.base,sdf19,cat1,ord1, real1 )
LOSS.RK20  = loss(GLRM_RK20,df.base,sdf20,cat1,ord1, real1 )


l.rk80 = c(l.rk80, LOSS.RK1$loss)
l.rk80 = c(l.rk80, LOSS.RK2$loss)
l.rk80 = c(l.rk80, LOSS.RK3$loss)
l.rk80 = c(l.rk80, LOSS.RK4$loss)
l.rk80 = c(l.rk80, LOSS.RK5$loss)
l.rk80 = c(l.rk80, LOSS.RK6$loss)
l.rk80 = c(l.rk80, LOSS.RK7$loss)
l.rk80 = c(l.rk80, LOSS.RK8$loss)
l.rk80 = c(l.rk80, LOSS.RK9$loss)
l.rk80 = c(l.rk80, LOSS.RK10$loss)
l.rk80 = c(l.rk80, LOSS.RK11$loss)
l.rk80 = c(l.rk80, LOSS.RK12$loss)
l.rk80 = c(l.rk80, LOSS.RK13$loss)
l.rk80 = c(l.rk80, LOSS.RK14$loss)
l.rk80 = c(l.rk80, LOSS.RK15$loss)
l.rk80 = c(l.rk80, LOSS.RK16$loss)
l.rk80 = c(l.rk80, LOSS.RK17$loss)
l.rk80 = c(l.rk80, LOSS.RK18$loss)
l.rk80 = c(l.rk80, LOSS.RK19$loss)
l.rk80 = c(l.rk80, LOSS.RK20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_RK1, GLRM_RK2, GLRM_RK3, GLRM_RK4, GLRM_RK5,  GLRM_RK6, GLRM_RK7, GLRM_RK8, GLRM_RK9, GLRM_RK10, 
GLRM_RK11, GLRM_RK12, GLRM_RK13, GLRM_RK14, GLRM_RK15,  GLRM_RK16, GLRM_RK17, GLRM_RK18, GLRM_RK19, GLRM_RK20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.RK1, LOSS.RK2, LOSS.RK3, LOSS.RK4, LOSS.RK5,  LOSS.RK6, LOSS.RK7, LOSS.RK8, LOSS.RK9, LOSS.RK10, 
LOSS.RK11, LOSS.RK12, LOSS.RK13, LOSS.RK14, LOSS.RK15,  LOSS.RK16, LOSS.RK17, LOSS.RK18, LOSS.RK19, LOSS.RK20 )


######## 90

glrm1 =  read.csv("Rank_GSS90_MI.csv")
glrm2 =  read.csv("Rank_GSS90_MI (1).csv")
glrm3 =  read.csv("Rank_GSS90_MI (2).csv")
glrm4 =  read.csv("Rank_GSS90_MI (3).csv")
glrm5 =  read.csv("Rank_GSS90_MI (4).csv")
glrm6 =  read.csv("Rank_GSS90_MI (5).csv")
glrm7 =  read.csv("Rank_GSS90_MI (6).csv")
glrm8 =  read.csv("Rank_GSS90_MI (7).csv")
glrm9 =  read.csv("Rank_GSS90_MI (8).csv")
glrm10 = read.csv("Rank_GSS90_MI (9).csv")
glrm11 = read.csv("Rank_GSS90_MI (10).csv")
glrm12 = read.csv("Rank_GSS90_MI (11).csv")
glrm13 = read.csv("Rank_GSS90_MI (12).csv")
glrm14 = read.csv("Rank_GSS90_MI (13).csv")
glrm15 = read.csv("Rank_GSS90_MI (14).csv")
glrm16 = read.csv("Rank_GSS90_MI (15).csv")
glrm17 = read.csv("Rank_GSS90_MI (16).csv")
glrm18 = read.csv("Rank_GSS90_MI (17).csv")
glrm19 = read.csv("Rank_GSS90_MI (18).csv")
glrm20 = read.csv("Rank_GSS90_MI (19).csv")

sdf1 =  as.numeric(unlist(read.csv("Rank_s90.csv", header = FALSE)))
sdf2 =  as.numeric(unlist(read.csv("Rank_s90 (1).csv", header = FALSE)))
sdf3 =  as.numeric(unlist(read.csv("Rank_s90 (2).csv", header = FALSE)))
sdf4 =  as.numeric(unlist(read.csv("Rank_s90 (3).csv", header = FALSE)))
sdf5 =  as.numeric(unlist(read.csv("Rank_s90 (4).csv", header = FALSE)))
sdf6 =  as.numeric(unlist(read.csv("Rank_s90 (5).csv", header = FALSE)))
sdf7 =  as.numeric(unlist(read.csv("Rank_s90 (6).csv", header = FALSE)))
sdf8 =  as.numeric(unlist(read.csv("Rank_s90 (7).csv", header = FALSE)))
sdf9 =  as.numeric(unlist(read.csv("Rank_s90 (8).csv", header = FALSE)))
sdf10 = as.numeric(unlist(read.csv("Rank_s90 (9).csv", header = FALSE)))
sdf11 = as.numeric(unlist(read.csv("Rank_s90 (10).csv", header = FALSE)))
sdf12 = as.numeric(unlist(read.csv("Rank_s90 (11).csv", header = FALSE)))
sdf13 = as.numeric(unlist(read.csv("Rank_s90 (12).csv", header = FALSE)))
sdf14 = as.numeric(unlist(read.csv("Rank_s90 (13).csv", header = FALSE)))
sdf15 = as.numeric(unlist(read.csv("Rank_s90 (14).csv", header = FALSE)))
sdf16 = as.numeric(unlist(read.csv("Rank_s90 (15).csv", header = FALSE)))
sdf17 = as.numeric(unlist(read.csv("Rank_s90 (16).csv", header = FALSE)))
sdf18 = as.numeric(unlist(read.csv("Rank_s90 (17).csv", header = FALSE)))
sdf19 = as.numeric(unlist(read.csv("Rank_s90 (18).csv", header = FALSE)))
sdf20 = as.numeric(unlist(read.csv("Rank_s90 (19).csv", header = FALSE)))


GLRM_RK1 = factorLevels(glrm1, df.base, cat1)
GLRM_RK2 = factorLevels(glrm2, df.base, cat1)
GLRM_RK3 = factorLevels(glrm3, df.base, cat1)
GLRM_RK4 = factorLevels(glrm4, df.base, cat1)
GLRM_RK5 = factorLevels(glrm5, df.base, cat1)
GLRM_RK6 = factorLevels(glrm6, df.base, cat1)
GLRM_RK7 = factorLevels(glrm7, df.base, cat1)
GLRM_RK8 = factorLevels(glrm8, df.base, cat1)
GLRM_RK9 = factorLevels(glrm9, df.base, cat1)
GLRM_RK10 = factorLevels(glrm10, df.base, cat1)
GLRM_RK11 = factorLevels(glrm11, df.base, cat1)
GLRM_RK12 = factorLevels(glrm12, df.base, cat1)
GLRM_RK13 = factorLevels(glrm13, df.base, cat1)
GLRM_RK14 = factorLevels(glrm14, df.base, cat1)
GLRM_RK15 = factorLevels(glrm15, df.base, cat1)
GLRM_RK16 = factorLevels(glrm16, df.base, cat1)
GLRM_RK17 = factorLevels(glrm17, df.base, cat1)
GLRM_RK18 = factorLevels(glrm18, df.base, cat1)
GLRM_RK19 = factorLevels(glrm19, df.base, cat1)
GLRM_RK20 = factorLevels(glrm20, df.base, cat1)

LOSS.RK1  = loss(GLRM_RK1,df.base,sdf1,cat1,ord1, real1 )
LOSS.RK2  = loss(GLRM_RK2,df.base,sdf2,cat1,ord1, real1 )
LOSS.RK3  = loss(GLRM_RK3,df.base,sdf3,cat1,ord1, real1 )
LOSS.RK4  = loss(GLRM_RK4,df.base,sdf4,cat1,ord1, real1 )
LOSS.RK5  = loss(GLRM_RK5,df.base,sdf5,cat1,ord1, real1 )
LOSS.RK6  = loss(GLRM_RK6,df.base,sdf6,cat1,ord1, real1 )
LOSS.RK7  = loss(GLRM_RK7,df.base,sdf7,cat1,ord1, real1 )
LOSS.RK8  = loss(GLRM_RK8,df.base,sdf8,cat1,ord1, real1 )
LOSS.RK9  = loss(GLRM_RK9,df.base,sdf9,cat1,ord1, real1 )
LOSS.RK10  = loss(GLRM_RK10,df.base,sdf10,cat1,ord1, real1 )
LOSS.RK11  = loss(GLRM_RK11,df.base,sdf11,cat1,ord1, real1 )
LOSS.RK12  = loss(GLRM_RK12,df.base,sdf12,cat1,ord1, real1 )
LOSS.RK13  = loss(GLRM_RK13,df.base,sdf13,cat1,ord1, real1 )
LOSS.RK14  = loss(GLRM_RK14,df.base,sdf14,cat1,ord1, real1 )
LOSS.RK15  = loss(GLRM_RK15,df.base,sdf15,cat1,ord1, real1 )
LOSS.RK16  = loss(GLRM_RK16,df.base,sdf16,cat1,ord1, real1 )
LOSS.RK17  = loss(GLRM_RK17,df.base,sdf17,cat1,ord1, real1 )
LOSS.RK18  = loss(GLRM_RK18,df.base,sdf18,cat1,ord1, real1 )
LOSS.RK19  = loss(GLRM_RK19,df.base,sdf19,cat1,ord1, real1 )
LOSS.RK20  = loss(GLRM_RK20,df.base,sdf20,cat1,ord1, real1 )


l.rk90 = c(l.rk90, LOSS.RK1$loss)
l.rk90 = c(l.rk90, LOSS.RK2$loss)
l.rk90 = c(l.rk90, LOSS.RK3$loss)
l.rk90 = c(l.rk90, LOSS.RK4$loss)
l.rk90 = c(l.rk90, LOSS.RK5$loss)
l.rk90 = c(l.rk90, LOSS.RK6$loss)
l.rk90 = c(l.rk90, LOSS.RK7$loss)
l.rk90 = c(l.rk90, LOSS.RK8$loss)
l.rk90 = c(l.rk90, LOSS.RK9$loss)
l.rk90 = c(l.rk90, LOSS.RK10$loss)
l.rk90 = c(l.rk90, LOSS.RK11$loss)
l.rk90 = c(l.rk90, LOSS.RK12$loss)
l.rk90 = c(l.rk90, LOSS.RK13$loss)
l.rk90 = c(l.rk90, LOSS.RK14$loss)
l.rk90 = c(l.rk90, LOSS.RK15$loss)
l.rk90 = c(l.rk90, LOSS.RK16$loss)
l.rk90 = c(l.rk90, LOSS.RK17$loss)
l.rk90 = c(l.rk90, LOSS.RK18$loss)
l.rk90 = c(l.rk90, LOSS.RK19$loss)
l.rk90 = c(l.rk90, LOSS.RK20$loss)

rm(glrm1, glrm2, glrm3, glrm4, glrm5,  glrm6, glrm7, glrm8, glrm9, glrm10, 
glrm11, glrm12, glrm13, glrm14, glrm15,  glrm16, glrm17, glrm18, glrm19, glrm20, 
GLRM_RK1, GLRM_RK2, GLRM_RK3, GLRM_RK4, GLRM_RK5,  GLRM_RK6, GLRM_RK7, GLRM_RK8, GLRM_RK9, GLRM_RK10, 
GLRM_RK11, GLRM_RK12, GLRM_RK13, GLRM_RK14, GLRM_RK15,  GLRM_RK16, GLRM_RK17, GLRM_RK18, GLRM_RK19, GLRM_RK20, 
sdf1, sdf2, sdf3, sdf4, sdf5,  sdf6, sdf7, sdf8, sdf9, sdf10, 
sdf11, sdf12, sdf13, sdf14, sdf15,  sdf16, sdf17, sdf18, sdf19, sdf20,
LOSS.RK1, LOSS.RK2, LOSS.RK3, LOSS.RK4, LOSS.RK5,  LOSS.RK6, LOSS.RK7, LOSS.RK8, LOSS.RK9, LOSS.RK10, 
LOSS.RK11, LOSS.RK12, LOSS.RK13, LOSS.RK14, LOSS.RK15,  LOSS.RK16, LOSS.RK17, LOSS.RK18, LOSS.RK19, LOSS.RK20 )

save(l.rk10, l.rk20, l.rk30, l.rk40, l.rk50, l.rk60, l.rk70, l.rk80, l.rk90 , file = "rank_consistency.RData")

