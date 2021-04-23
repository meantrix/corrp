library('corrP')


df = iris
n.cores = 1
p.value = 0.05
verbose = TRUE
comp = 'g'
alternative = 'g'
cor.nn = "pps"
cor.nc = "pps"
cor.cc = "pps"
parallel = TRUE
ptest.n.sum = 500
ptest.r = ptest.r = F
lm.args =lm.args=pearson.args=cramersV.args=dcor.args=pps.args=mic.args=uncoef.args = list()


#test pps unit
for(i in 1:NCOL(df)){
 for(j in 1:NCOL(df)){
    p = corr_fun(df,ny = colnames(df)[i],nx =colnames(df)[i] ,cor.nn = cor.nn,cor.nc = cor.nc,cor.cc = cor.cc)
    p2 = ppsr::score(df,y = colnames(df)[i],x =colnames(df)[i])
}}

p1 = corrp(df, cor.nn = cor.nn,cor.nc = cor.nc,cor.cc = cor.cc)
p1 = corr_matrix(p,cor.nn = cor.nn,cor.nc = cor.nc,cor.cc = cor.cc)

pps.test = corrp(df,cor.nn = cor.nn,cor.nc = cor.nc,cor.cc = cor.cc)

