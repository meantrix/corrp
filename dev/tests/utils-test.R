df = iris
n.cores = 1
p.value = 0.05
verbose = TRUE
comp = 'g'
alternative = 'g'
cor.n = "pearson"
cor.nc = "lm"
cor.cc = "cramersV"
parallel = TRUE


pos_1=1
pos_2=2
type = 'g'
pv = 0.3

#ISOLATE

corlm(df[pos_1],df[pos_2], p.value = p.value,type = 'g',verbose = F)
cramersvp(df[pos_1],df[pos_2], p.value = p.value,type = 'g',verbose = F)
corperp(df[pos_1],df[pos_2], p.value = p.value,type = 'g',verbose = F)
dcorp(df[pos_1],df[pos_2], p.value = p.value,type = 'g',verbose = F)
uncorp(df[pos_1],df[pos_2], p.value = p.value,type = 'g',verbose = F)
micorp(df[pos_1],df[pos_2], p.value = p.value,type = 'g',verbose = F)

#Looping



