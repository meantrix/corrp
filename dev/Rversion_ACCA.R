# exploration function

corr = corrp(mtcars)
m = corr_matrix(corr)

#number of cluster k
k = 2

#'split corr_matrix in almost equal random clusters
rand.cluster = function(m,k){
v = colnames(m)
n = NCOL(m)

int = n %/% k + n %% k


clu = list()

while( !is.null(v)  ){

  if(length(v) > int ){
    sv = sample(v,int,replace = F)
    v = v[! v %in% sv]
    clu = c( clu,list(sv) )
  } else {
    clu = c( clu,list(v) )
    v = NULL
  }

}
return(clu)
}

nm = colnames(m)
spl =  rand.cluster(m,k)
which(colnames(corr))
#maxsingle element
sin.clu = lapply(1:k, function(x){

  my =  m[ spl[[x]],spl[[x]] ]
  rowm = rowMeans(my,na.rm = T)
  singel = names( which.max(rowm) )

  return(singel)

})

#add other remainer elements




