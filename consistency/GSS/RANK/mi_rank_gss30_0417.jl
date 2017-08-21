################################
####### ENVIRONMENT ############
################################

#cd("/Users/Nandana/Desktop/GitHub folder/MissingValues/Apr 2017/consistency/GSS/Data/")

println("adding processors")
addprocs(3)


println("importing modules")

import DataFrames, LowRankModels, StatsBase

@everywhere using DataFrames
@everywhere using LowRankModels
@everywhere using StatsBase


#############################################
############ FUNCTION DEFINITIONS ###########
#############################################

println("defining functions 1 & 2")


#############
# F1 and F2: function to send variables to the different parallel processes
#############

function sendto(p::Int; args...)nprocs()
    for (nm, val) in args
        @spawnat(p, eval(Main, Expr(:(=), nm, val)))
    end
end


function sendto(ps::Vector{Int}; args...)
    for p in ps
        sendto(p; args...)
    end
end


println("defining function 3")

########################
# F3: BOOTSTRAP INDICES #
#########################

@everywhere function bootstrap(df)

	nelements = size(df)[1]*size(df)[2]
	n = size(df)[1]


	# array of non NA values
	index = Array(Any, 0)

	for c = 1: size(df)[2]
		rows = find(!isna(df[c]))	
		if length(rows)>0
			index = vcat(index, map(r -> n*(c-1)+ r, rows))
		end
	end

	s = index[sample(1:length(index), length(index), replace = true)]

	return s

end

println("defining function 4")

###########################################################
# F4: TRACE NORM CROSS VALIDATION ON BOOTSTRAPPED INDICES #
###########################################################

@everywhere function trace_bootstrapcv(df, k, datatype, s)

uselosses = Dict{Symbol, Any}(
	  :real        => QuadLoss,
	  :bool        => LogisticLoss,
	  :ord         => BvSLoss,
	  :cat         => MultinomialLoss
	 )	  
	  
	 println("glrm setup")

glrm_trace = GLRM(df, k, datatype, loss_map = uselosses ) 
map( i -> glrm_trace.observed_examples[i] = [], collect(1:size(df)[2]))
map( j -> glrm_trace.observed_features[j] = [], collect(1:size(df)[1]))
n = size(df)[1]

println("bootstrap setup")


for j = 1:length(s)
	ind = s[j]
	r = Int64(((ind-1) % n) + 1)
	c = Int64(floor((ind-1) / n) + 1)	

	append!(glrm_trace.observed_examples[c], r)
	append!(glrm_trace.observed_features[r], c)
end

println("crossvalidation")

trn_err, tst_err, trn_g, tst_g = cross_validate(glrm_trace, nfolds = 3, params = ProxGradParams(1.0, max_iter=50, abs_tol=0.0001, rel_tol=0.001, min_stepsize=1e-9), init = init_svd!)
tstE_trace = mean(tst_err)
trnE_trace = mean(trn_err)
println(k)

return tstE_trace, trnE_trace

end

println("defining function 5")


##############################################
# F5: FINDING OPTIMAL TRACE NORM REGULARIZER #
##############################################

@everywhere function reg_search(df, s, datatype)
		
	#ranks = collect(2:1:size(df)[2])	
	ranks = collect(2:1:4)	
	println("running bootstrap crossvalidation")
	
	lowtrace1 = @time pmap(k -> trace_bootstrapcv(df, k, datatype, s),ranks)
	tstE1 = [x[1] for x in lowtrace1]
	trnE1 = [x[2] for x in lowtrace1]
	kstar1 = ranks[findmin(tstE1)[2]]

	println(kstar1)

	
return kstar1, tstE1, trnE1

end

println("defining function 6")

##########################################
# F6: TRACE NORM FIT GIVEN REG PARAMETER #
##########################################

@everywhere function trace_bootstrapfit(df, kfit, datatype,s)
	
	prob_losses = Dict{Symbol, Any}(
			  :real        => QuadLoss,
			  :bool        => LogisticLoss,
			  :ord         => BvSLoss,
			  :cat         => MultinomialLoss)
		  
	
	glrm_fin = GLRM(df, kfit , datatype, loss_map = prob_losses ) 
	map( i -> glrm_fin.observed_examples[i] = [], collect(1:size(df)[2]))
	map( j -> glrm_fin.observed_features[j] = [], collect(1:size(df)[1]))
	
	n = size(df)[1]
	
	for j = 1:length(s)
		#println(j)
		ind = s[j]
		r = Int64(((ind-1) % n) + 1)
		c = Int64(floor((ind-1) / n) + 1)	
		append!(glrm_fin.observed_examples[c], r)
		append!(glrm_fin.observed_features[r], c)
	end

	init_svd!(glrm_fin)
	X, Y, ch = fit!(glrm_fin, params = ProxGradParams(1.0, max_iter=200, abs_tol=0.00001, rel_tol=0.0001, min_stepsize=1e-10))

	return glrm_fin
	
end

println("defining function 7, 8 & 9")


###################################
# F7: CONVERT INT TO CATEGORICALS #
###################################

@everywhere function int2cat(df, dfimp, catcol)


dfcat = DataFrame(Any,size(df)[1],  length(catcol))


for j = 1: length(catcol)
	col = deepcopy(df[catcol[j]])
	colimp = deepcopy(dfimp[catcol[j]])
	levels = Set(col[!isna(col)])
	reversecolmap = Dict{Int, Any}(zip(1:length(levels), sort(collect(levels))))
	dfcat[j] = map( i -> reversecolmap[colimp[i]], collect(1:length(colimp)))
end

return dfcat

end

###############################
# F8: CONVERT INT TO ORDINALS #
###############################

@everywhere function int2ord(df, dfimp, ordcol)

dford = DataFrame(Any,size(df)[1],  length(ordcol))

for j = 1: length(ordcol)
	levmin = cummin(dropna(df[ordcol[j]]))[length(dropna(df[ordcol[j]]))]
	dford[j] = map( i -> (round(dfimp[i, ordcol[j]]) + levmin- 1), collect(1:size(df)[1]))	
end

return dford

end


############################################
# F9: FINAL DATAFRAME IN CONSISTENT FORMAT #
############################################

@everywhere function replace_obs(df, dfimp, catcol, ordcol)

	realcol = setdiff(collect(1: size(df)[2]),vcat(ordcol, catcol));
	
	dfcat = int2cat(df, dfimp, catcol)
	dford = int2ord(df, dfimp, ordcol)
	dfreal = convert(DataFrame, dfimp[realcol])
	df_fin = DataFrame(Any,size(df)[1],  size(df)[2])
	df_fin[catcol] = dfcat
	df_fin[ordcol] = dford
	df_fin[realcol] = dfreal

for j = 1: size(df)[2]
	s = find(!isna(df[j]))
	map(i-> df_fin[i,j] = df[i,j], s)
end

return df_fin

end


println("defining function 10 consolidates everything")

#####################################
# F10:  MULTIPLE IMPUTATION WRAPPER #
#####################################

@everywhere function mult_imp(df, m, catcol, ordcol)
	
	
	println("defining datatypes")
	
	realcol = setdiff(collect(1: size(df)[2]),vcat(ordcol, catcol));
	
	# setting variables to store	
	datastore = DataFrame( Any, size(df)[1], m*size(df)[2] )
	rankstore = []
	datatype = Array(Symbol, size(df)[2])
	map(r-> datatype[r] = :real, realcol )
	map(o-> datatype[o] = :ord, ordcol )
	map(c-> datatype[c] = :cat, catcol )
	
	println("sending to workers")
	
	sendto(workers(), datatype = datatype)
	sendto(workers(), df = df)		  
	
	for i = 1:m
		
		println(i)
		
		println("bootstrap")
		
		# bootstrapping to account for observation uncertainty
		s = bootstrap(df)
		sendto(workers(), s = s)
		
		println("finding optimal regularizer ")
		
		# finding optimal regularizer 
		summ_reg = reg_search(df, s, datatype) 
		kfit = summ_reg[1]#final multiplier 
		
		println("fitting final model")
		
		# fitting glrm on optimal regularization parameter
		glrm_fin = trace_bootstrapfit(df, kfit, datatype,s)
		
		println("sample missing")
		
		# final imputation including sampling uncertainty 
		dfimp = sample_missing(glrm_fin)
		
		println("replacing observations")
		
		# making completed matrix compatible with original data
		df_fin = replace_obs(df, dfimp, catcol, ordcol)
		
		println("saving data")
		
		# storing imputation data
		append!(rankstore, kfit)
		datastore[:, (((i-1)*size(df)[2])+1) : (i*size(df)[2])] = df_fin
	
	end

	return datastore, rankstore

end


println("loading data")

############### LOADING THE DATA AND RUNNING THE MI WRAPPER ###############

rho = 0.3
d = readtable("GSScleaned.csv");
df = d[:, 2:size(d)[2] ];

#import categorical index
catcol = readtable("GSScategoricals.csv")[:, 2];
ordcol = readtable("GSSordinals.csv")[:,2];

# remove rho*100% random indices 
nelements = size(df)[1]*size(df)[2]
n = size(df)[1]
xdf = collect(1:nelements)
s = sample(xdf, Int64(round(rho*length(xdf))), replace = false)

for j = 1:length(s)
	#println(j)
	ind = s[j]
	r = Int64(((ind-1) % n) + 1)
	c = Int64(floor((ind-1) / n) + 1)	
	df[r,c] = NA
end


println("running main code")


# number of imputations
m = 1; 
results = @time mult_imp(df, m , catcol, ordcol)

imps = results[1]
regs = results[2]

println("saving results")


############### SAVE RESULTS ############# 
writetable("Rank_GSS30_MI.csv", imps)
writedlm("Rank_s30.csv", s)
writedlm("Regs30.csv", regs)
