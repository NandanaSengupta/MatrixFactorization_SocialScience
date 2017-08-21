# MICE

rm(list = ls())

library("mice")


#### NLSY10 

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/Data")
df = read.csv("NLSY10.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W1 = complete(imp, action = 1)
W2 = complete(imp, action = 2)
W3 = complete(imp, action = 3)
W4 = complete(imp, action = 4)
W5 = complete(imp, action = 5)
W6 = complete(imp, action = 6)
W7 = complete(imp, action = 7)
W8 = complete(imp, action = 8)
W9 = complete(imp, action = 9)
W10 = complete(imp, action = 10)

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, file = "MICE_NLSY10.RData" )

rm(list = ls())


#### NLSY20 

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/Data")

df = read.csv("NLSY20.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W1 = complete(imp, action = 1)
W2 = complete(imp, action = 2)
W3 = complete(imp, action = 3)
W4 = complete(imp, action = 4)
W5 = complete(imp, action = 5)
W6 = complete(imp, action = 6)
W7 = complete(imp, action = 7)
W8 = complete(imp, action = 8)
W9 = complete(imp, action = 9)
W10 = complete(imp, action = 10)

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10,  file = "MICE_NLSY20.RData" )

rm(list = ls())


#### NLSY30 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/Data")

df = read.csv("NLSY30.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W1 = complete(imp, action = 1)
W2 = complete(imp, action = 2)
W3 = complete(imp, action = 3)
W4 = complete(imp, action = 4)
W5 = complete(imp, action = 5)
W6 = complete(imp, action = 6)
W7 = complete(imp, action = 7)
W8 = complete(imp, action = 8)
W9 = complete(imp, action = 9)
W10 = complete(imp, action = 10)

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10,  file = "MICE_NLSY30.RData" )

rm(list = ls())


#### NLSY40 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/Data")

df = read.csv("NLSY40.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W1 = complete(imp, action = 1)
W2 = complete(imp, action = 2)
W3 = complete(imp, action = 3)
W4 = complete(imp, action = 4)
W5 = complete(imp, action = 5)
W6 = complete(imp, action = 6)
W7 = complete(imp, action = 7)
W8 = complete(imp, action = 8)
W9 = complete(imp, action = 9)
W10 = complete(imp, action = 10)

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10,  file = "MICE_NLSY40.RData" )

rm(list = ls())


#### NLSY50 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/Data")

df = read.csv("NLSY50.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W1 = complete(imp, action = 1)
W2 = complete(imp, action = 2)
W3 = complete(imp, action = 3)
W4 = complete(imp, action = 4)
W5 = complete(imp, action = 5)
W6 = complete(imp, action = 6)
W7 = complete(imp, action = 7)
W8 = complete(imp, action = 8)
W9 = complete(imp, action = 9)
W10 = complete(imp, action = 10)

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, file = "MICE_NLSY50.RData" )

rm(list = ls())



#### NLSY60 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/Data")

df = read.csv("NLSY60.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W1 = complete(imp, action = 1)
W2 = complete(imp, action = 2)
W3 = complete(imp, action = 3)
W4 = complete(imp, action = 4)
W5 = complete(imp, action = 5)
W6 = complete(imp, action = 6)
W7 = complete(imp, action = 7)
W8 = complete(imp, action = 8)
W9 = complete(imp, action = 9)
W10 = complete(imp, action = 10)

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, file = "MICE_NLSY60.RData" )

rm(list = ls())


#### NLSY70 

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/Data")

df = read.csv("NLSY70.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W1 = complete(imp, action = 1)
W2 = complete(imp, action = 2)
W3 = complete(imp, action = 3)
W4 = complete(imp, action = 4)
W5 = complete(imp, action = 5)
W6 = complete(imp, action = 6)
W7 = complete(imp, action = 7)
W8 = complete(imp, action = 8)
W9 = complete(imp, action = 9)
W10 = complete(imp, action = 10)

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, file = "MICE_NLSY70.RData" )

rm(list = ls())


#### NLSY80 

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/Data")


df = read.csv("NLSY80.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W1 = complete(imp, action = 1)
W2 = complete(imp, action = 2)
W3 = complete(imp, action = 3)
W4 = complete(imp, action = 4)
W5 = complete(imp, action = 5)
W6 = complete(imp, action = 6)
W7 = complete(imp, action = 7)
W8 = complete(imp, action = 8)
W9 = complete(imp, action = 9)
W10 = complete(imp, action = 10)

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, file = "MICE_NLSY80.RData" )

rm(list = ls())


#### NLSY90

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/Data") 

df = read.csv("NLSY90.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/NLSY/MICE")

imp  = mice(df, m = 10, pred = quickpred(df), maxit = 20)

W1 = complete(imp, action = 1)
W2 = complete(imp, action = 2)
W3 = complete(imp, action = 3)
W4 = complete(imp, action = 4)
W5 = complete(imp, action = 5)
W6 = complete(imp, action = 6)
W7 = complete(imp, action = 7)
W8 = complete(imp, action = 8)
W9 = complete(imp, action = 9)
W10 = complete(imp, action = 10)

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, file = "MICE_NLSY90.RData" )

rm(list = ls())
