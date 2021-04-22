library('corrP')


df = iris
n.cores = 1
p.value = 0.05
verbose = TRUE
comp = 'g'
alternative = 'g'
cor.nn = "dcor"
cor.nc = "lm"
cor.cc = "cramersV"
parallel = TRUE
ptest.n.sum = 500
ptest.r = ptest.r = F
lm.args =lm.args=pearson.args=cramersV.args=dcor.args=pps.args=mic.args=uncoef.args = list()


pos_1=5
pos_2=1
type = 'g'
pv = 0.3

#ISOLATE
nx = names(df[pos_2])
ny = names(df[pos_1])
y = df[[pos_1]]
x = df[[pos_2]]

corlm(y,x, p.value = p.value,comp = 'g',verbose = F)
cramersvp(y,x, p.value = p.value,comp = 'g',verbose = F)
corperp(y,x, p.value = p.value,comp = 'g',verbose = F)
dcorp(y,x, p.value = p.value,comp = 'g',verbose = F)
uncorp(y,x, p.value = p.value,comp = 'g',verbose = F)
micorp(y,x, p.value = p.value,comp = 'g',verbose = F)

#Looping
dim = NCOL(df)
corp = matrix(data = NA,nrow = dim,ncol = dim)
for(i in 1:dim){
  for(j in 1:dim){
    corp[i,j] = .cor_fun(df = df,pos_1 = i,pos_2 = j,
                   p.value=p.value,
                   verbose = verbose,
                   alternative = alternative,
                   comp = comp,
                   cor.nn = cor.nn,
                   cor.nc = cor.nc,
                   cor.cc = cor.cc,
                   ptest.n.sum = ptest.n.sum,
                   ptest.r = ptest.r,
                   lm.args = lm.args,
                   pearson.args = pearson.args,
                   cramersV.args = cramersV.args,
                   dcor.args = dcor.args,
                   pps.args = pps.args,
                   mic.args = mic.args,
                   uncoef.args = uncoef.args

    )$infer.value
  }
}


#Parallel

nc = seq(NCOL(df))
res.grid = expand.grid(i = nc, j = nc, stringsAsFactors = FALSE)
temp_score = function(i) {
  cor_fun(df, pos_1 = param_grid[["i"]][k], pos_2 = param_grid[["j"]][k], ...)
}

cl = parallel::makeCluster(n.cores)
parallel::clusterExport(cl, varlist = as.list(.list_corrP ) )
scores = parallel::clusterApply(cl, seq_len(NROW(res.grid)),
                                function(k, ...){
                                cor_fun(df,
                                        pos_1 = res.grid[["i"]][k],
                                        pos_2 = res.grid[["j"]][k],
                                        p.value = p.value,
                                        verbose = verbose,
                                        alternative = alternative,
                                        comp = comp,
                                        cor.nn = cor.nn,
                                        cor.nc = cor.nc,
                                        cor.cc = cor.cc,
                                        ptest.n.sum = ptest.n.sum,
                                        ptest.r = ptest.r,
                                        lm.args = lm.args,
                                        pearson.args = pearson.args,
                                        cramersV.args = cramersV.args,
                                        dcor.args = dcor.args,
                                        pps.args = pps.args,
                                        mic.args = mic.args,
                                        uncoef.args = uncoef.args)
                                }
                            )

