library('corrp')


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
n.sum = 500
rk  = F
lm.args =lm.args=pearson.args=cramersV.args=dcor.args=pps.args=mic.args=uncoef.args = list()


#test pps unit
for(i in 1:NCOL(df)){
 for(j in 1:NCOL(df)){
    p = corr_fun(df,ny = colnames(df)[i],nx =colnames(df)[j] ,cor.nn = cor.nn,cor.nc = cor.nc,cor.cc = cor.cc)
    p2 = ppsr::score(df,y = colnames(df)[i],x =colnames(df)[i])
}}

p1 = corrp(df, cor.nn = cor.nn,cor.nc = cor.nc,cor.cc = cor.cc)
p1 = corr_matrix(p,cor.nn = cor.nn,cor.nc = cor.nc,cor.cc = cor.cc)

pps.test = corrp(df,cor.nn = cor.nn,cor.nc = cor.nc,cor.cc = cor.cc)


#ptest

x = iris[[1]]
y = iris[[2]]

x1 = ptest(x,y,FUN = function(x,y) cor(x,y),num.s = 2000 ,alternative = 't')
x2 = ptest(x,y,FUN = function(x,y) cor(x,y),num.s = 2000 ,alternative = 'g')
x3 = ptest(x,y,FUN = function(x,y) cor(x,y),num.s = 2000 ,alternative = 'l')

y1 = cor.test(x,y,alternative = 't')$p.value
y2 = cor.test(x,y,alternative = 'g')$p.value
y3 = cor.test(x,y,alternative = 'l')$p.value

x4 <- sample(1:30, 100, replace = TRUE)
y4 <- sample(0:1, 100, replace = TRUE)




