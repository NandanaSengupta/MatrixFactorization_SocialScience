#################################################################
#### This code creates a dataset out of the original GSS2014 data -- 

# GSS2014cleaned -- removes all columns with 1) more than 33% entries missing & 2) high correlation ( > 0.75) and factor columns with more than 10 levels

### We also generate two lists of column indices per cleaned dataset -- list of ordinal variables, list of nonordinal categorical variables


##################################################################

rm(list = ls())

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

check = read.csv("GSS2014.csv")


library("polycor")


col1 = dim(check)[2]

# GSS2014clean

#removing columns with more than 33% missing values

col.remove = numeric()
for (i in 1:dim(check)[2]){
	column.check = sum(is.na(check[,i]))
	
	if (column.check>0.33*dim(check)[1]){col.remove = c(col.remove, i)}}

l = length(col.remove)

check = check[, -col.remove]

# nonvarying variables
novary = which(names(check)=="away1" | names(check)=="away2" | names(check) =="away3" | names(check) == "away4"| names(check)=="away5" | names(check)=="formwt"| names(check) == "gender10"| names(check)=="mar9" | names(check)=="mar10"| names(check)=="relate1" | names(check) =="relate8" | names(check) == "relhh1"| names(check)=="relhhd1" | names(check)=="relhhd8"| names(check) == "relsp2" )

l = l+ length(novary)

check = check[, -novary]


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


# id variables
ids = which(names(check)=="dateintv" | names(check)=="year" | names(check) =="id" | names(check) == "X"| names(check)=="vstrat" | names(check)=="vpsu"| names(check) == "wtss"| names(check)=="wtssnr" | names(check)=="wtssall"| names(check)=="intrwt" | names(check) =="oversamp"| names(check) == "formwt"| names(check)== "intage" |  names(check)== "intid" | names(check)== "isco88"| names(check)== "lngthinv" |names(check)== "paisco88"| names(check)== "sampcode")

l = l+length(ids)

check = check[, - ids]

dim(check)[2] - (col1 - l) == 0



df = check
# 641

maxcol= dim(df)[2]
j = 1

while (j < maxcol){
	
	corvec = numeric()
	corr.check = numeric()
	na.meth = numeric()
	
	for (k in (j+1): dim(df)[2]){
		print(c(j, k, dim(df)[2]))	
	   try({  
	   	a= hetcor(df[,j] , df[,k], std.err = FALSE )
	    corr.check = c(corr.check, a$correlations[1,2])
	    na.meth = c(na.meth, a$NA.method)
#	    if(a$correlations[1,2]>0.7)
	    if(abs(a$correlations[1,2])>0.7)
	    
	    {corvec = c(corvec, k)}}
	    )}
	    
	   if(length(corvec)>0)  {df = df[, - corvec]}
	maxcol = dim(df)[2]
	j = j+1}



dim(df)

# list of factor variables
fac = which(sapply(df,class) == "factor")


##################################################################

# recode ordinal variables

# list of factor variables
ord = c(1, 6, 7, 12, 13, 14, 15, 16, 21, 23, 26, 35,  36, 38, 41, 42, 47, 51, 57, 58, 59, 60, 61, 62, 63,  68, 70, 71,  76, 78, 79, 82, 85, 88, 89, 93, 94, 96, 100, 102, 103)


noms = fac[!is.element(fac, ord)]
VARNOMS = df[, noms]

VARORDS = numeric()
#VARNAME= recode(ORDNOMS3[,], "'' = 1; ''=2; ''= 3; '' = 4; '' = 5; ''=6 ")

library("car")

ADULTS = df[,ord[1]]

ATTEND = as.numeric(recode(df[,ord[2]], "'never' = 1; 'LT ONCE A YEAR'= 2; 'ONCE A YEAR'=3; 'SEVRL TIMES A YR'= 4; 'ONCE A MONTH' = 5; '2-3X A MONTH' = 6; 'NRLY EVERY WEEK'=7; 'EVERY WEEK'= 8 ; 'MORE THN ONCE WK'= 9"))


BABIES = df[, ord[3]]

CHILDS = df[, ord[4]]

CLASS = as.numeric(recode(df[,ord[5]], "'LOWER CLASS' = 1; 'WORKING CLASS'=2; 'MIDDLE CLASS'= 3; 'UPPER CLASS' = 4 "))


CLOSEBLK = df[, ord[6]]

CLOSEWHT = df[,ord[7]]

COMPREND = as.numeric(recode(df[,ord[8]], "'poor' = 1; 'fair'=2; 'good'= 3"))

COURTS = as.numeric(recode(df[,ord[9]], "'NOT HARSH ENOUGH' = 1; 'ABOUT RIGHT'=2; 'TOO HARSH'= 3"))

DEGREE = as.numeric(recode(df[,ord[10]], "'LT HIGH SCHOOL' = 1; 'HIGH SCHOOL'=2; 'JUNIOR COLLEGE'= 3; 'bachelor' = 4; 'graduate' = 5 "))

EARNRS = df[,ord[11]]

FINALTER = as.numeric(recode(df[,ord[12]], "'worse' = 1; 'STAYED SAME'=2; 'better'= 3 "))

FINRELA = as.numeric(recode(df[,ord[13]], "'FAR BELOW AVERAGE' = 1; 'BELOW AVERAGE'=2; 'average'= 3; 'ABOVE AVERAGE' = 4; 'FAR ABOVE AVERAGE' = 5 "))

FUND = as.numeric(recode(df[,ord[14]], "'fundamentalist' = 3; 'moderate'=2; 'liberal'= 1"))

HAPPY = as.numeric(recode(df[,ord[15]], "'NOT TOO HAPPY' = 1; 'PRETTY HAPPY'=2; 'VERY HAPPY'= 3 "))

HEALTH = as.numeric(recode(df[,ord[16]], "'poor' = 1; 'fair'=2; 'good'= 3; 'excellent' = 4 "))

INCOM16 = as.numeric(recode(df[,ord[17]], "'FAR BELOW AVERAGE' = 1; 'BELOW AVERAGE'=2; 'average'= 3; 'ABOVE AVERAGE' = 4; 'FAR ABOVE AVERAGE' = 5 "))


MADEG = as.numeric(recode(df[,ord[18]], "'LT HIGH SCHOOL' = 1; 'HIGH SCHOOL'=2; 'JUNIOR COLLEGE'= 3; 'bachelor' = 4; 'graduate' = 5 "))

NATCHLD = as.numeric(recode(df[,ord[19]], "'TOO LITTLE' = 1; 'ABOUT RIGHT'=2; 'TOO MUCH'= 3 "))

NATENRGY = as.numeric(recode(df[,ord[20]], "'Too little' = 1; 'About right'=2; 'Too much'= 3 "))

NATMASS = as.numeric(recode(df[,ord[21]], "'TOO LITTLE' = 1; 'ABOUT RIGHT'=2; 'TOO MUCH'= 3 "))

NATPARK =as.numeric( recode(df[,ord[22]], "'TOO LITTLE' = 1; 'ABOUT RIGHT'=2; 'TOO MUCH'= 3 "))

NATROAD = as.numeric(recode(df[,ord[23]], "'TOO LITTLE' = 1; 'ABOUT RIGHT'=2; 'TOO MUCH'= 3 "))

NATSCI = as.numeric(recode(df[,ord[24]], "'TOO LITTLE' = 1; 'ABOUT RIGHT'=2; 'TOO MUCH'= 3 "))

NATSOC = as.numeric(recode(df[,ord[25]], "'TOO LITTLE' = 1; 'ABOUT RIGHT'=2; 'TOO MUCH'= 3 "))

PADEG = as.numeric(recode(df[,ord[26]], "'LT HIGH SCHOOL' = 1; 'HIGH SCHOOL'=2; 'JUNIOR COLLEGE'= 3; 'bachelor' = 4; 'graduate' = 5 "))

PARTNERS=as.numeric( recode(df[,ord[27]], "'NO PARTNERS' = 1; '1 PARTNER'=2; '2 PARTNERS'= 3; '3 PARTNERS' = 4; '4 PARTNERS' = 5; '5-10 PARTNERS'=6; '11-20 PARTNERS'=6; '21-100 PARTNERS' = 6; else = NA   "))

PARTNERS5=as.numeric( recode(df[,ord[28]], "'NO PARTNERS' = 1; '1 PARTNER'=2; '2 PARTNERS'= 3; '3 PARTNERS' = 4; '4 PARTNERS' = 5; '5-10 PARTNERS'=6; '11-20 PARTNERS'=6; '21-100 PARTNERS' = 6; else = NA   "))


POLVIEWS = as.numeric(recode(df[,ord[29]], "'EXTREMELY LIBERAL' = 1; 'liberal'=2; 'SLIGHTLY LIBERAL'= 3; 'moderate' = 4; 'conservative' = 6; 'SLGHTLY CONSERVATIVE'=5; 'EXTRMLY CONSERVATIVE'=7 "))

PRAY = as.numeric(recode(df[,ord[30]], "'never' = 1; 'LT ONCE A WEEK'= 2; 'ONCE A WEEK'=3; 'SEVERAL TIMES A WEEK'= 4; 'ONCE A DAY' = 5; 'SEVERAL TIMES A DAY' = 6"))

PRETEEN = df[,ord[31]]

RANK = df[,ord[32]]

RELACTIV = as.numeric(recode(df[,ord[33]], "'never' = 1; 'LESS THAN ONCE A YEAR'= 2; 'ABOUT ONCE OR TWICE A YEAR'=3; 'SEVERAL TIMES A YEAR'= 4; 'ABOUT ONCE A MONTH' = 5; '2-3 TIMES A MONTH' = 6; 'NEARLY EVERY WEEK'=7; 'EVERY WEEK'= 8 ; 'SEVERAL TIMES A WEEK'= 9; 'ONCE A DAY' =9"))

RELITEN = as.numeric(df[,ord[34]])

RELPERSN = as.numeric(recode(df[,ord[35]], "'NOT RELIGIOUS' = 1; 'MODRTE RELIGIOUS'=3; 'SLIGHT RELIGIOUS'= 2; 'VERY RELIGIOUS' = 4 "))

SATFIN = as.numeric(recode(df[,ord[36]], "'NOT AT ALL SAT' = 1; 'MORE OR LESS'=2; 'satisfied'= 3 "))

SATJOB = as.numeric(recode(df[,ord[37]], "'VERY DISSATISFIED' = 1; 'A LITTLE DISSAT'=2; 'MOD. SATISFIED'= 3; 'VERY SATISFIED' = 4"))

SEXFREQ = as.numeric(recode(df[,ord[38]], "'NOT AT ALL' = 1; 'ONCE OR TWICE'=2; 'ONCE A MONTH'= 3; '2-3 TIMES A MONTH' = 4; 'weekly' = 5; '2-3 PER WEEK'=6; '4+ PER WEEK'=7 "))


SPRTPRSN = as.numeric(recode(df[,ord[39]], "'NOT SPIRITUAL' = 1; 'MODEATE SPIRTUAL'=3; 'SLIGHT SPIRITUAL'= 2; 'VERY SPIRITUAL' = 4 "))


TEENS = df[,ord[40]]

VETYEARS = as.numeric(recode(df[,ord[41]], "'none' = 1; 'LESS THAN 2 YRS'=2; '2 TO 4  YEARS'= 3; 'MORE THAN 4 YRS' = 4; else = NA"))


VARORDS = cbind(ADULTS, ATTEND, BABIES, CLASS, CLOSEBLK, CLOSEWHT, COMPREND, COURTS, DEGREE, EARNRS, FINALTER, FINRELA, FUND, HAPPY, HEALTH, INCOM16, MADEG, NATCHLD, NATENRGY, NATMASS, NATPARK, NATROAD, NATSCI, NATSOC, PADEG, PARTNERS, PARTNERS5, POLVIEWS, PRAY, PRETEEN, RANK, RELACTIV, RELITEN, RELPERSN, SATFIN, SATJOB, SEXFREQ, SPRTPRSN, TEENS, VETYEARS)

df = df[, -unique(c(ord, fac)) ]
df = cbind(df, VARORDS, VARNOMS)


# final cleanup

var.rem = which(names(df) == "intethn"| names(df) == "inthisp" | names(df) == "intsex"| names(df) == "form"| names(df) == "hefinfo" | names(df) == "phase"| names(df) == "respnum"    )
df = df[, - var.rem]

df[,which(names(df) == "nummen")][df[,which(names(df) == "nummen")]>900] = NA
df[,which(names(df) == "numwomen")][df[,which(names(df) == "numwomen")]>900] = NA
df[,which(names(df) == "old2")][df[,which(names(df) == "old2")]>300] = NA


noms = which(sapply(df,class) == "factor")
ords = seq(10,49)

# saving data
write.csv(ords, file = "GSSordinals.csv")
write.csv(noms, file = "GSScategoricals.csv")
write.csv(df, file = "GSScleaned.csv")


### SPARSE DATASETS 

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



write.csv(df10, "GSS10.csv")
write.csv(df20, "GSS20.csv")
write.csv(df30, "GSS30.csv")
write.csv(df40, "GSS40.csv")
write.csv(df50, "GSS50.csv")
write.csv(df60, "GSS60.csv")
write.csv(df70, "GSS70.csv")
write.csv(df80, "GSS80.csv")
write.csv(df90, "GSS90.csv")


