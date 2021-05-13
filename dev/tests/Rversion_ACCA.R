# exploration function ACCA
# library(corrp)
# corr = corrp()
# m = corr_matrix(corr)

#number of cluster k
tictoc::tic()
k = 4
niter = 500
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

res = list()
stop.now = 0
for(j in 1:niter) {

#maxsingle element
  sin.clu = lapply(1:k, function(x){

    my =  m[ spl[[x]],spl[[x]] ]
    if( is.null(dim(my)) ) rowm = my  else rowm =  rowMeans(my,na.rm = T)
    names(rowm) = spl[[x]]
    singel = names( which.max(rowm) )
    return(singel)

  })

  sin.clu.v = unlist(sin.clu)

  names(sin.clu) = sin.clu.v

  nm2 = nm[! nm %in% sin.clu.v]

  #add other remainer elements
  for(i in seq_along(nm2)){

            my  = m[ sin.clu.v, nm2[1:i] ]

            if( is.vector(my) ){
              singel = names( which.max(my) )
            } else {
              singel =  which.max(rowMeans(my,na.rm = T))
            }

            if(is.null(singel)){singel = sin.clu.v[1]}

            sin.clu[[singel]] = c(sin.clu[[singel]],nm2[i])

  }

spl = sin.clu

res[[j]] = spl

if( j > 1 && identical(res[[j]], res[[j-1]] ) ) {
  stop.now = stop.now + 1
}

if(stop.now >1000 ) {
  break
}

}
tictoc::toc()



