# MICE

rm(list = ls())
library("mice")


#### GSS10 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS10.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

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

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10,  file = "MICE_GSS10.RData" )

rm(list = ls())


#### GSS20 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS20.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

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

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, file = "MICE_GSS20.RData" )

rm(list = ls())


#### GSS30 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS30.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

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

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, file = "MICE_GSS30.RData" )

rm(list = ls())


#### GSS40 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS40.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

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

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10,  file = "MICE_GSS40.RData" )

rm(list = ls())


#### GSS50 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS50.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

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

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10,  file = "MICE_GSS50.RData" )

rm(list = ls())



#### GSS60 
setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS60.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

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

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10,  file = "MICE_GSS60.RData" )

rm(list = ls())


#### GSS70 

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")

df = read.csv("GSS70.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

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

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10,  file = "MICE_GSS70.RData" )

rm(list = ls())


#### GSS80 

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data")


df = read.csv("GSS80.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

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

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, file = "MICE_GSS80.RData" )

rm(list = ls())


#### GSS90

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/Data") 

df = read.csv("GSS90.csv")[, -1]

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Code for replication/GSS/MICE")

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

save(W1, W2, W3, W4, W5, W6, W7, W8, W9, W10, file = "MICE_GSS90.RData" )

rm(list = ls())
