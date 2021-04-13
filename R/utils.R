#auxiliar functions

lmp = function(y,x,p.value){

  sum.res = summary(
    stats::lm(y ~ as.factor(x))
  )
  pv = stats::pf (sum.res$fstatistic[1],sum.res$fstatistic[2],
                  sum.res$fstatistic[3],lower.tail=F)

  if(pv<p.value) {
    r=sqrt(sum.res[["r.squared"]])
    cat(paste("alternative hypothesis: true correlation is not equal to 0","\n",
              "p-value: ",pv,"\n"))
  } else {
    r=0
    cat(paste("there is no correlation at the confidence level  p-value<",p.value,"\n",
              "p-value: ",pv,"\n"))
  }

  return(r)

}

cramersvp = function(y,x,p.value,simulate.p.value = TRUE){
  pv = stats::chisq.test(y,x,simulate.p.value=simulate.p.value)$p.value

  if(pv<p.value) {
    r=lsr::cramersV(y,x, simulate.p.value=simulate.p.value)
    cat(paste("alternative hypothesis: true correlation is not equal to 0","\n",
              "p-value: ",pv,"\n"))
  } else {
    r = 0
    cat(paste("there is no correlation at the confidence level  p-value<",p.value,"\n",
              "p-value: ",pv,"\n"))
  }

  return(r)

}

corperp = function(y,x,p.value,use){

  res = stats::cor.test(y,x,use,method="pearson",alternative = "two.sided")
  pv = res[["p.value"]]

  if(pv<p.value) {
    r=res[["estimate"]]
    cat(paste("alternative hypothesis: true correlation is not equal to 0","\n",
              "p-value: ",pv,"\n"))
  } else {
    r=0
    cat(paste("there is no correlation at the confidence level  p-value<",p.value,"\n",
              "p-value: ",pv,"\n"))
  }

  return(r)

}

#parallel corr matrix
cor_par = function (df,p.value) {
  `%dopar%` = foreach::`%dopar%`
  `%:%` = foreach::`%:%`
  dim=NCOL(df)
  corp = foreach::foreach(i=1:dim,.export='cor_fun') %:%
    foreach::foreach (j=1:dim) %dopar% {
      corp = cor_fun(df,i,j,p.value=p.value)
    }
  matrix(unlist(corp), ncol=ncol(df))
}

##############################################################################

cor_fun = function(df,pos_1, pos_2,p.value){

  # both are numeric

  if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
     class(df[[pos_2]]) %in% c("integer", "numeric")){
    r = try(corperp(df[[pos_1]]
                     , df[[pos_2]]
                     , p.value = p.value
                     , use = "pairwise.complete.obs")
    )
  }

  # one is numeric and other is a factor/character

  if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
     class(df[[pos_2]]) %in% c("factor", "character")){
    r = try(
      lmp(df[[pos_1]],df[[pos_2]],p.value = p.value)
    )
  }

  if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
     class(df[[pos_1]]) %in% c("factor", "character")){
    r = try(
      lmp(df[[pos_2]],df[[pos_1]],p.value = p.value)
    )
  }

  # both are factor/character

  if(class(df[[pos_1]]) %in% c("factor", "character") &&
     class(df[[pos_2]]) %in% c("factor", "character")){
    r = try(cramersvp(df[[pos_1]], df[[pos_2]],p.value = p.value,
                       simulate.p.value = TRUE))


  }


  if((class(r) %in% "try-error")){
    warnings(cat("some operations produces Nas values it will be replaced by NA.","\n",
                 pos_1, " FUN " ,pos_2,"\n"))
    r = NA
  }
  return(r)
}


