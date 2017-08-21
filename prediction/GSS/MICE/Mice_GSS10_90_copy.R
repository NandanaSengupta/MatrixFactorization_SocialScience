# MICE

rm(list = ls())

library("mice")


#### GSS10 

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")
df = read.csv("GSS10.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W11 =  complete(imp, action = 1)
W12 = complete(imp, action = 2)
W13 = complete(imp, action = 3)
W14 = complete(imp, action = 4)
W15 = complete(imp, action = 5)
W16 = complete(imp, action = 6)
W17 = complete(imp, action = 7)
W18 = complete(imp, action = 8)
W19 = complete(imp, action = 9)
W20 = complete(imp, action = 10)

save(W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, file = "MICE_GSS10_copy.RData" )

rm(list = ls())


#### GSS20 

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS20.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W11 =  complete(imp, action = 1)
W12 = complete(imp, action = 2)
W13 = complete(imp, action = 3)
W14 = complete(imp, action = 4)
W15 = complete(imp, action = 5)
W16 = complete(imp, action = 6)
W17 = complete(imp, action = 7)
W18 = complete(imp, action = 8)
W19 = complete(imp, action = 9)
W20 = complete(imp, action = 10)

save(W11, W12, W13, W14, W15, W16, W17, W18, W19, W20,  file = "MICE_GSS20_copy.RData" )

rm(list = ls())


#### GSS30 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS30.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W11 =  complete(imp, action = 1)
W12 = complete(imp, action = 2)
W13 = complete(imp, action = 3)
W14 = complete(imp, action = 4)
W15 = complete(imp, action = 5)
W16 = complete(imp, action = 6)
W17 = complete(imp, action = 7)
W18 = complete(imp, action = 8)
W19 = complete(imp, action = 9)
W20 = complete(imp, action = 10)

save(W11, W12, W13, W14, W15, W16, W17, W18, W19, W20,  file = "MICE_GSS30_copy.RData" )

rm(list = ls())


#### GSS40 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS40.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W11 =  complete(imp, action = 1)
W12 = complete(imp, action = 2)
W13 = complete(imp, action = 3)
W14 = complete(imp, action = 4)
W15 = complete(imp, action = 5)
W16 = complete(imp, action = 6)
W17 = complete(imp, action = 7)
W18 = complete(imp, action = 8)
W19 = complete(imp, action = 9)
W20 = complete(imp, action = 10)

save(W11, W12, W13, W14, W15, W16, W17, W18, W19, W20,  file = "MICE_GSS40_copy.RData" )

rm(list = ls())


#### GSS50 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS50.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W11 =  complete(imp, action = 1)
W12 = complete(imp, action = 2)
W13 = complete(imp, action = 3)
W14 = complete(imp, action = 4)
W15 = complete(imp, action = 5)
W16 = complete(imp, action = 6)
W17 = complete(imp, action = 7)
W18 = complete(imp, action = 8)
W19 = complete(imp, action = 9)
W20 = complete(imp, action = 10)

save(W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, file = "MICE_GSS50_copy.RData" )

rm(list = ls())



#### GSS60 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS60.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W11 =  complete(imp, action = 1)
W12 = complete(imp, action = 2)
W13 = complete(imp, action = 3)
W14 = complete(imp, action = 4)
W15 = complete(imp, action = 5)
W16 = complete(imp, action = 6)
W17 = complete(imp, action = 7)
W18 = complete(imp, action = 8)
W19 = complete(imp, action = 9)
W20 = complete(imp, action = 10)

save(W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, file = "MICE_GSS60_copy.RData" )

rm(list = ls())


#### GSS70 

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS70.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W11 =  complete(imp, action = 1)
W12 = complete(imp, action = 2)
W13 = complete(imp, action = 3)
W14 = complete(imp, action = 4)
W15 = complete(imp, action = 5)
W16 = complete(imp, action = 6)
W17 = complete(imp, action = 7)
W18 = complete(imp, action = 8)
W19 = complete(imp, action = 9)
W20 = complete(imp, action = 10)

save(W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, file = "MICE_GSS70_copy.RData" )

rm(list = ls())


#### GSS80 

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")


df = read.csv("GSS80.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W11 =  complete(imp, action = 1)
W12 = complete(imp, action = 2)
W13 = complete(imp, action = 3)
W14 = complete(imp, action = 4)
W15 = complete(imp, action = 5)
W16 = complete(imp, action = 6)
W17 = complete(imp, action = 7)
W18 = complete(imp, action = 8)
W19 = complete(imp, action = 9)
W20 = complete(imp, action = 10)

save(W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, file = "MICE_GSS80_copy.RData" )

rm(list = ls())


#### GSS90

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data") 

df = read.csv("GSS90.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W11 =  complete(imp, action = 1)
W12 = complete(imp, action = 2)
W13 = complete(imp, action = 3)
W14 = complete(imp, action = 4)
W15 = complete(imp, action = 5)
W16 = complete(imp, action = 6)
W17 = complete(imp, action = 7)
W18 = complete(imp, action = 8)
W19 = complete(imp, action = 9)
W20 = complete(imp, action = 10)

save(W11, W12, W13, W14, W15, W16, W17, W18, W19, W20, file = "MICE_GSS90_copy.RData" )

rm(list = ls())
