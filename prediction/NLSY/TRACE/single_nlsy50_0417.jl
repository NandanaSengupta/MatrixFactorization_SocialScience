################################
####### ENVIRONMENT ############
################################

addprocs(37)

import DataFrames, LowRankModels, StatsBase

@everywhere using DataFrames
@everywhere using LowRankModels
@everywhere using StatsBase


#############################################
############ FUNCTION DEFINITIONS ###########
#############################################

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


###########################################################
# F4: TRACE NORM CROSS VALIDATION ON BOOTSTRAPPED INDICES #
###########################################################

@everywhere function tracecv(df, k, datatype, a)

uselosses = Dict{Symbol, Any}(
	  :real        => QuadLoss,
	  :bool        => LogisticLoss,
	  :ord         => BvSLoss,
	  :cat         => MultinomialLoss
	 )	  
	  

glrm_trace = GLRM(df, k, datatype, loss_map = uselosses ) 
scale!(glrm_trace.rx, a)
scale!(glrm_trace.ry, a)
trn_err, tst_err, trn_g, tst_g = cross_validate(glrm_trace, nfolds = 5, params = ProxGradParams(1.0, max_iter=200, abs_tol=0.00001, rel_tol=0.0001, min_stepsize=1e-10), init = init_svd!)
tstE_trace = mean(tst_err)
trnE_trace = mean(trn_err)
println(k,a)

return tstE_trace, trnE_trace

end

##############################################
# F5: FINDING OPTIMAL TRACE NORM REGULARIZER #
##############################################

@everywhere function reg_search(df, datatype)
	
	nw = 37
	
	alpha1 = [0.01, 0.1, 1, 10, 100]
	lowtrace1 = @time pmap(a -> tracecv(df, min(size(df)[1], size(df)[2]), datatype, a),alpha1)
	tstE1 = [x[1] for x in lowtrace1]
	trnE1 = [x[2] for x in lowtrace1]
	astar1 = alpha1[findmin(tstE1)[2]]

	println(astar1)

	if astar1 == 0.01
		alpha2 = linspace(0.001, 0.1 , nw)
		lowtrace2 = @time pmap(a -> tracecv(df,min(size(df)[1], size(df)[2]), datatype, a),alpha2)
		tstE2 = [x[1] for x in lowtrace2]
		trnE2 = [x[2] for x in lowtrace2]
		astar2 = alpha2[findmin(tstE2)[2]]
	end


	if astar1 == 0.1	
		alpha2 = linspace(0.01, 1 , nw)
		lowtrace2 = @time pmap(a -> tracecv(df,  min(size(df)[1], size(df)[2]), datatype, a),alpha2)
		tstE2 = [x[1] for x in lowtrace2]
		trnE2 = [x[2] for x in lowtrace2]
		astar2 = alpha2[findmin(tstE2)[2]]
	end

	if astar1 == 1	
		alpha2 = linspace(0.1, 10 , nw)
		lowtrace2 = @time pmap(a -> tracecv(df, min(size(df)[1], size(df)[2]), datatype, a),alpha2)
		tstE2 = [x[1] for x in lowtrace2]
		trnE2 = [x[2] for x in lowtrace2]
		astar2 = alpha2[findmin(tstE2)[2]]
	end

	if astar1 == 10
		alpha2 = linspace(1, 100 , nw)
		lowtrace2 = @time pmap(a -> tracecv(df, min(size(df)[1], size(df)[2]), datatype, a),alpha2)
		tstE2 = [x[1] for x in lowtrace2]
		trnE2 = [x[2] for x in lowtrace2]
		astar2 = alpha2[findmin(tstE2)[2]]
	end

	if astar1 == 100
		alpha2 = linspace(10, 200 , nw)
		lowtrace2 = @time pmap(a -> tracecv(df, min(size(df)[1], size(df)[2]), datatype, a),alpha2)
		tstE2 = [x[1] for x in lowtrace2]
		trnE2 = [x[2] for x in lowtrace2]
		astar2 = alpha2[findmin(tstE2)[2]]
	end



return astar1, astar2, tstE1, trnE1, tstE2, trnE2 

end

##########################################
# F6: TRACE NORM FIT GIVEN REG PARAMETER #
##########################################

@everywhere function trace(df, datatype, afit)
	
	prob_losses = Dict{Symbol, Any}(
			  :real        => QuadLoss,
			  :bool        => LogisticLoss,
			  :ord         => BvSLoss,
			  :cat         => MultinomialLoss)
		  
	
	glrm_fin = GLRM(df, min(size(df)[1], size(df)[2]), datatype, loss_map = prob_losses ) 
	scale!(glrm_fin.rx, afit)
	scale!(glrm_fin.ry, afit)
	init_svd!(glrm_fin)
	X, Y, ch = fit!(glrm_fin, params = ProxGradParams(1.0, max_iter=200, abs_tol=0.00001, rel_tol=0.0001, min_stepsize=1e-10))
	dfimp = impute(glrm_fin)
	return dfimp
	
end

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

#####################################
# F10:  SINGLE IMPUTATION WRAPPER   #
#####################################

@everywhere function single_imp(df, m, catcol, ordcol)
	
	realcol = setdiff(collect(1: size(df)[2]),vcat(ordcol, catcol));
	
	# setting variables to store	
	datatype = Array(Symbol, size(df)[2])
	map(r-> datatype[r] = :real, realcol )
	map(o-> datatype[o] = :ord, ordcol )
	map(c-> datatype[c] = :cat, catcol )
	sendto(workers(), datatype = datatype)
	sendto(workers(), df = df)		  
	
	# finding optimal regularizer 
	summ_reg = reg_search(df, datatype) 
	afit = summ_reg[2]#final multiplier 
		
	# fitting glrm on optimal regularization parameter
	dfimp = convert(DataFrame, trace(df, datatype, afit))
		
	# making completed matrix compatible with original data
	df_fin = replace_obs(df, dfimp, catcol, ordcol)
		
	return df_fin, afit

end
	
############### LOADING THE DATA AND RUNNING THE MI WRAPPER ###############


d = readtable("NLSY50.csv");
df = d[:, 2:size(d)[2] ];

#import categorical index
catcol = readtable("NLSYcategoricals.csv")[:, 2];
ordcol = readtable("NLSYordinals.csv")[:,2];


# number of imputations
m = 1; 

results = @time single_imp(df, m , catcol, ordcol)

imps = results[1]
regs = results[2]
############### SAVE RESULTS ############# 
writetable("Trace_NLSY50.csv", imps)
writedlm("Regs.txt", regs)
