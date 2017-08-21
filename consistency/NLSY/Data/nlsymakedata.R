setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/Data")

rm(list = ls())

library("polycor")

nlsy.cat = read.csv("nlsy2011cat.csv")[, -1]

nlsy.int = read.csv("nlsy2011int.csv")[, -1]

idlist = c(1, 37, 41, 42, 45)

novarlist = which(apply(nlsy.int, 2, var, na.rm = TRUE) == 0)

catlist = c(2, 4, 6, 7, 8, 14, 15, 16, 18, 19, 33, 36, 40, 44,  46, 47, 48, 49, 50, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 87, 86, 88, 89, 90, 91,92, 93, 94, 95, 100, 101, 102, 103) # removed one column that was common with novarlist -- 38

remlist = c(idlist, novarlist, catlist)

nlsy.cat = nlsy.cat[, catlist]

nlsy.int = nlsy.int[, -unique(remlist)]

nlsy.full = cbind(nlsy.int, nlsy.cat )

check = nlsy.full

col.remove = numeric()
for (i in 1:dim(check)[2]){
	column.check = sum(is.na(check[,i]))
	
	if (column.check>0.50*dim(check)[1]){col.remove = c(col.remove, i)}}

l = length(col.remove)

check = check[, -col.remove]



# nominal variables (factors)
fac = which(sapply(check,class) == "factor")


### checking factors with more than 10 levels
n.fac = numeric()
fac.remove = numeric()
for (k in 1: length(fac) ){
	n.fac = c( n.fac, nlevels(check[, fac[k]]))
	if (n.fac[k] > 10) {fac.remove = c(fac.remove, fac[k])}} 

l = l+ length(fac.remove)

check = check[, -fac.remove]

df = check

maxcol= dim(df)[2]
j = 1

while (j < maxcol){
	
	corvec = numeric()
	
	for (k in (j+1): dim(df)[2]){
		print(c(j, k, dim(df)[2]))	
	   try({  
	   	a= hetcor(df[,j] , df[,k], std.err = FALSE )
#	    if(a$correlations[1,2]>0.7)
	    if(abs(a$correlations[1,2])>0.7)
	    
	    {corvec = c(corvec, k)}}
	    )}
	    
	   if(length(corvec)>0)  {df = df[, - corvec]}
	maxcol = dim(df)[2]
	j = j+1}


 df[,which(names(df) == "YSAQ.282A2_2011")][df[,which(names(df) == "YSAQ.282A2_2011")] == TRUE] = "YES"
 df[,which(names(df) == "YSAQ.282A2_2011")][df[,which(names(df) == "YSAQ.282A2_2011")] == FALSE] = "NO"
 df[,which(names(df) == "YSAQ.282A3_2011")][df[,which(names(df) == "YSAQ.282A3_2011")] == TRUE] = "YES"
 df[,which(names(df) == "YSAQ.282A3_2011")][df[,which(names(df) == "YSAQ.282A3_2011")] == FALSE] = "NO"
 df[,which(names(df) == "YSAQ.282A5_2011")][df[,which(names(df) == "YSAQ.282A5_2011")] == TRUE] = "YES"
 df[,which(names(df) == "YSAQ.282A5_2011")][df[,which(names(df) == "YSAQ.282A5_2011")] == FALSE] = "NO"

noms = which(sapply(df,class) == "factor"|sapply(df,class) == "character" )
ords = c(1, 4, 5, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24)


# removing rows with more than 75% missing data


numna = function(df){
	
	colna = numeric()
	for (i in 1: dim(df)[1]){
	print(i)		
		colna = c(colna, (sum(is.na(df[i,]))/dim(df)[2]))
	}

return(colna)
}

check = numna(df)

length(check[which(check>0.90)])

dim(df)[1]- length(check[which(check>0.75)])



df = df[which(check<0.75),]


write.csv(ords, file = "NLSYordinals.csv")
write.csv(noms, file = "NLSYcategoricals.csv")
write.csv(df, file = "NLSYcleaned.csv")

######## SPARSITY


# make sparse versions 20%, 30%, ... , 90% and save 

xdf = c(1:(dim(df)[1]*dim(df)[2]))
sdf10 = sample(xdf, round(0.1*length(xdf)))
sdf20 = sample(xdf, round(0.2*length(xdf)))
sdf30 = sample(xdf, round(0.3*length(xdf)))
sdf40 = sample(xdf, round(0.4*length(xdf)))
sdf50 = sample(xdf, round(0.5*length(xdf)))
sdf60 = sample(xdf, round(0.6*length(xdf)))
sdf70 = sample(xdf, round(0.7*length(xdf)))
sdf80 = sample(xdf, round(0.8*length(xdf)))
sdf90 = sample(xdf, round(0.9*length(xdf)))

ndf = nrow(df)
df10 = df
df20 = df
df30 = df
df40 = df
df50 = df
df60 = df
df70 = df
df80 = df 
df90 = df 


for ( i in 1:length(sdf10)){
	ind = sdf10[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	df10[r,c] = NA}	



for ( i in 1:length(sdf20)){
	ind = sdf20[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	df20[r,c] = NA}	


for ( i in 1:length(sdf30)){
	ind = sdf30[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	df30[r,c] = NA}	

for ( i in 1:length(sdf40)){
	ind = sdf40[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	df40[r,c] = NA}
	
for ( i in 1:length(sdf50)){
	ind = sdf50[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	df50[r,c] = NA}

for ( i in 1:length(sdf60)){
	ind = sdf60[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	df60[r,c] = NA}

for ( i in 1:length(sdf70)){
	ind = sdf70[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	df70[r,c] = NA}
	
for ( i in 1:length(sdf80)){
	ind = sdf80[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	df80[r,c] = NA}
	

for ( i in 1:length(sdf90)){
	ind = sdf90[i]
	r = ((ind-1) %% ndf) + 1 
	c = floor((ind-1) / ndf) + 1
	df90[r,c] = NA}


sum(is.na(df10))/length(xdf)
sum(is.na(df20))/length(xdf)
sum(is.na(df30))/length(xdf)
sum(is.na(df40))/length(xdf)
sum(is.na(df50))/length(xdf)
sum(is.na(df60))/length(xdf)
sum(is.na(df70))/length(xdf)
sum(is.na(df80))/length(xdf)
sum(is.na(df90))/length(xdf)



write.csv(df10, "NLSY10.csv")
write.csv(df20, "NLSY20.csv")
write.csv(df30, "NLSY30.csv")
write.csv(df40, "NLSY40.csv")
write.csv(df50, "NLSY50.csv")
write.csv(df60, "NLSY60.csv")
write.csv(df70, "NLSY70.csv")
write.csv(df80, "NLSY80.csv")
write.csv(df90, "NLSY90.csv")

