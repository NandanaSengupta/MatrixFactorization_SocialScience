
install.packages("Amelia", repos="http://r.iq.harvard.edu", type = "source")

library("Amelia")

df = read.csv("GSS60.csv")[, -1]

noms = read.csv("GSScategoricals.csv")[, -1]
ords = read.csv("GSSordinals.csv")[, -1]

a = amelia(df, m = 5, noms = noms, ords = ords, p2s = 2, tolerance = 0.001) 

am1 = as.data.frame(a$imputations[1])
am2 = as.data.frame(a$imputations[2])
am3 = as.data.frame(a$imputations[3])
am4 = as.data.frame(a$imputations[4])
am5 = as.data.frame(a$imputations[5])

save(am1, am2, am3, am4, am5, file = "amelia_GSS60.RData" )



