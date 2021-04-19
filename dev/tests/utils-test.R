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


pos_1=1
pos_2=1
type = 'g'
pv = 0.3

#ISOLATE

corlm(df[pos_1],df[pos_2], p.value = p.value,comp = 'g',verbose = F)
cramersvp(df[pos_1],df[pos_2], p.value = p.value,comp = 'g',verbose = F)
corperp(df[pos_1],df[pos_2], p.value = p.value,comp = 'g',verbose = F)
dcorp(df[pos_1],df[pos_2], p.value = p.value,comp = 'g',verbose = F)
uncorp(df[pos_1],df[pos_2], p.value = p.value,comp = 'g',verbose = F)
micorp(df[pos_1],df[pos_2], p.value = p.value,comp = 'g',verbose = F)

#Looping
dim = NCOL(df)
corp = matrix(data = NA,nrow = dim,ncol = dim)
for(i in 1:dim){
  for(j in 1:dim){
    corp[i,j] = cor_fun(df = df,pos_1 = i,pos_2 = j,
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

    )$value
  }
}

