#auxiliar functions

lmp = function(y,x,p.value, verbose = TRUE , ...){

  sum.res = summary(
    stats::lm(y ~ as.factor(x))
  )
  pv = stats::pf (sum.res$fstatistic[1],sum.res$fstatistic[2],
                  sum.res$fstatistic[3],lower.tail=F)

  if(pv < p.value) {
    r = sqrt(sum.res[["r.squared"]])


    if(verbose){
      cat(paste("alternative hypothesis: true correlation is not equal to 0","\n",
              "p-value: ",pv,"\n"))
    }

  } else {
    r = NA

    if(verbose){
      cat(paste("there is no correlation at the confidence level  p-value < ",p.value,"\n",
              "p-value: ",pv,"\n"))
    }

  }

  return(r)

}

cramersvp = function(y,x,p.value,simulate.p.value = TRUE,verbose = TRUE, ...){
  pv = stats::chisq.test(y,x,simulate.p.value=simulate.p.value)$p.value

  if(pv<p.value) {
    r=lsr::cramersV(y,x, simulate.p.value=simulate.p.value)
    cat(paste("alternative hypothesis: true correlation is not equal to 0","\n",
              "p-value: ",pv,"\n"))
  } else {

    if(verbose)
    r = NA
    cat(paste("there is no correlation at the confidence level  p-value < ",p.value,"\n",
              "p-value: ",pv,"\n"))
  }

  return(r)

}

corperp = function(y,x,p.value,use,verbose = TRUE, ...){

  res = stats::cor.test(y,x,use,method="pearson",alternative = "two.sided")
  pv = res[["p.value"]]

  if(pv < p.value) {
    r = res[["estimate"]]

    if(verbose){
      cat(paste("alternative hypothesis: true correlation is not equal to 0","\n",
              "p-value: ",pv,"\n"))
    }

  } else {

    r = NA
    if(verbose){
      cat(paste("there is no correlation at the confidence level  p-value < ",p.value,"\n",
              "p-value: ",pv,"\n"))
    }

  }

  return(r)

}

#parallel corr matrix
cor_par = function (df,p.value,verbose = TRUE, ...) {
  `%dopar%` = foreach::`%dopar%`
  `%:%` = foreach::`%:%`
  dim=NCOL(df)
  corp = foreach::foreach(i=1:dim,.export='cor_fun') %:%
    foreach::foreach (j=1:dim) %dopar% {
      corp = cor_fun(df = df,pos_1 = i,pos_2 = j, p.value=p.value, verbose = verbose,...)
    }
  matrix(unlist(corp), ncol=ncol(df))
}

##############################################################################

cor_fun = function(df, pos_1, pos_2, p.value, ...){

  # both are numeric

  if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
     class(df[[pos_2]]) %in% c("integer", "numeric")){
    r = try(corperp(df[[pos_1]]
                     , df[[pos_2]]
                     , p.value = p.value
                     , use = "pairwise.complete.obs"
                     , verbose = verbose,...)
    )
  }

  # one is numeric and other is a factor/character

  if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
     class(df[[pos_2]]) %in% c("factor", "character")){
    r = try(
      lmp(df[[pos_1]],df[[pos_2]],p.value = p.value, verbose = verbose , ...)
    )
  }

  if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
     class(df[[pos_1]]) %in% c("factor", "character")){
    r = try(
      lmp(df[[pos_2]],df[[pos_1]],p.value = p.value, verbose = verbose , ...)
    )
  }

  # both are factor/character

  if(class(df[[pos_1]]) %in% c("factor", "character") &&
     class(df[[pos_2]]) %in% c("factor", "character")){
    r = try(
      cramersvp(df[[pos_1]], df[[pos_2]],p.value = p.value,
                       simulate.p.value = TRUE,
                       verbose = verbose, ...)
      )


  }

  if((class(r) %in% "try-error")){
    warnings(cat("ERROR: some operations produces Nas values.","\n",
                 pos_1, " FUN " ,pos_2,"\n"))
    r = NA
  }

  return(r)

}


