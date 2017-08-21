
rm(list = ls())

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/GSS/AMELIA/")
load('amelia_consistency.RData')


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/GSS/MICE/")
load('mice_consistency.RData')


setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/GSS/TRACE/")
load('trace_consistency.RData')

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/GSS/RANK/")
load('rank_consistency.RData')


TR = cbind(l.tr10, l.tr20, l.tr30, l.tr40, l.tr50, l.tr60, l.tr70, l.tr80, c(l.tr90, NA))
RK = cbind(l.rk10, l.rk20, l.rk30, l.rk40, l.rk50, l.rk60, l.rk70, l.rk80, l.rk90)

MC = cbind(l.mice10, l.mice20, l.mice30, l.mice40, l.mice50, l.mice60, l.mice70, l.mice80, rep(NA, 20))


AM = cbind(c(l.aml10, rep(NA, 20-length(l.aml10))), c(l.aml20, rep(NA, 20-length(l.aml20))), c(l.aml30, rep(NA, 20-length(l.aml30))), c(l.aml40, rep(NA, 20-length(l.aml40))), c(l.aml50, rep(NA, 20-length(l.aml50))), c(l.aml60, rep(NA, 20-length(l.aml60))),  c(l.aml70, rep(NA, 20-length(l.aml70))), c(l.aml80, rep(NA, 20-length(l.aml80))), c(l.aml90, rep(NA, 20-length(l.aml90)))) 

mat = rbind(TR, RK, MC, AM)
rownames(mat) = c(rep("trace", 20), rep("rank", 20), rep("mice", 20), rep("amelia", 20))
colnames(mat) = c("10%", "20%","30%","40%","50%","60%","70%","80%","90%")

setwd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/GSS/Comparisons")



#write.csv(mat, "GSSsparsitycomp.csv")

#mat = read.csv("GSSsparsitycomp.csv")
#mat = mat[,-1]

avg.tr = round(apply(mat[1:20 ,], 2 , mean, na.rm = TRUE),3)
avg.rk = round(apply(mat[21:40 ,], 2 , mean, na.rm = TRUE),3)
avg.mice = round(apply(mat[41:60 , ], 2 , mean, na.rm = TRUE),3)
avg.aml = round(apply(mat[61:80 ,], 2 , mean, na.rm = TRUE),3)
imp.mice = 100*((avg.mice- avg.tr)/avg.mice)
imp.aml = 100*(avg.aml- avg.tr)/avg.aml

prop.na = function(vec){
	prop.na = paste(round(100*(sum(is.na(vec))/length(vec))), "%")
}


ci = function(vec){
	if(sum(!is.na(vec))>1){
	ci = paste("(", round(t.test(vec)$conf.int[1], 2), " ",  round(t.test(vec)$conf.int[2],2), ")" , sep = "")}
	
}



avg.aml =round(apply(AM, 2, mean, na.rm = TRUE),3)
avg.mice =round(apply(MC, 2,mean, na.rm = TRUE),3)
avg.tr = round(apply(TR, 2, mean, na.rm = TRUE),3)
avg.rk = round(apply(RK, 2, mean, na.rm = TRUE),3)



prop.na.aml =apply(AM, 2, prop.na)
prop.na.mice =apply(MC, 2, prop.na)
prop.na.tr = apply(TR, 2, prop.na)
prop.na.rk = apply(RK, 2, prop.na)


ci.aml = unlist(as.character(apply(AM, 2, ci)))
ci.mice = unlist(as.character(apply(MC, 2, ci)))
ci.tr = unlist(as.character(apply(TR, 2, ci)))
ci.rk = unlist(as.character(apply(RK, 2, ci)))


summ_mat = rbind(avg.aml, ci.aml, prop.na.aml, avg.mice, ci.mice,  prop.na.mice , avg.tr, ci.tr,  
 prop.na.tr, avg.rk, ci.rk,  prop.na.rk)



library("xtable")
summTab = xtable(summ_mat, align = "|l|c|c|c|c|c|c|c|c|c|")
print(summTab, type = "latex", file = "GSS_summary_with_ci.tex")


missing_prop = seq(10, 90, by = 10)
ymax = 1.05*max(max(mat, na.rm = TRUE))
ymin = 0.95*min(min(mat, na.rm = TRUE)) 


check = c(rep(10, 20), rep(20, 20), rep(30, 20), rep(40, 20), rep(50, 20), rep(60, 20) , rep(70, 20), rep(80, 20), rep(90, 20)  )


pdf("sparsity_boxplots_gss_with_rk.pdf")

boxplot(as.vector(RK)~ check, col = "orange", ylim = c(ymin, ymax ), xlim = c(0.5, 9.5), main = "Multiple Imputations: GSS", xlab ="Percentage of entries missing", ylab = "Total Error Metric",   boxlwd = 1, boxcol = "orange", medlwd = 2, medcol = "black", whiskcol = "orange", outcol = "orange", staplecol = "orange")

boxplot(as.vector(AM) ~ check, add = TRUE, boxlwd = 1, boxcol = "red", medlwd = 2, medcol = "black", whiskcol = "red", outcol = "red", staplecol = "red", col ="red")


boxplot(as.vector(MC) ~check,  col = "darkgreen", add= TRUE,  boxlwd = 1, boxcol = "darkgreen", medlwd = 2, medcol = "black", whiskcol = "darkgreen", outcol = "darkgreen", staplecol = "darkgreen")



boxplot(as.vector(TR)~ check, col = "blue", add = TRUE,  boxlwd = 1, boxcol = "blue", medlwd = 2, medcol = "black", whiskcol = "blue", outcol = "blue", staplecol = "blue")

lines(seq(1,9), avg.aml,  type = "b", col = rgb(1,0,0), pch = 19, lwd = 1.5 )
lines(seq(1,9), avg.mice,  type = "b", col = "darkgreen", pch = 19, lwd = 1.5)
lines(seq(1,9), avg.tr, type = "b", col = rgb(0,0,1), pch = 19, lwd = 1.5)
lines(seq(1,9), avg.rk, type = "b", col = "orange", pch = 19, lwd = 1.5)

legend("topleft", c("Amelia", "MICE", "Rank", "Trace"),col =  c("red", "darkgreen", "orange", "blue"), lty = 1, pch = 19 ,pt.bg = 'white')

dev.off()


quartz()
#pdf("sparsity_gss.pdf")
plot(missing_prop, avg.aml, type = "b", col = rgb(1,0, 0), main = "Sparsity Analysis: GSS Subset", xlab ="Percentage of entries missing", ylab = "Scaled Loss", xlim = c(10, 90), ylim = c(ymin, ymax ), pch = 19, yaxt = "n")
lines(missing_prop, avg.mice,  type = "b", col = rgb(0,1,0), pch = 19)
lines(missing_prop, avg.tr, type = "b", col = rgb(0,0,1), pch = 19)
axis(1, at = seq(10, 90, by = 10))
axis(2, at = seq(10000, 200000, by = 50000))

legend("topleft", c("Amelia", "MICE", "Trace"),col =  c("red", "green", "blue"), lty = 1, pch = 19 ,pt.bg = 'white')
#dev.off()

