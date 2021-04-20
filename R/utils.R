#auxiliar functions

#linear regression Calculations
corlm = function(y, x, p.value, comp, verbose, lm.args =list(), ...){

  if( is.data.frame(x) ) x = x[[1]]
  if( is.data.frame(y) ) y = y[[1]]

  infer = "Linear Model"
  stat = "P-value"

  args = c(list(y ~ as.factor(x)), lm.args)

  sum.res = summary(
    do.call(stats::lm, args )
  )

  pv = stats::pf (sum.res$fstatistic[1],sum.res$fstatistic[2],
                  sum.res$fstatistic[3],lower.tail=F)

  compare = comparepv(x = pv,pv = p.value,comp = comp)
  r = sqrt(sum.res[["r.squared"]])
  msg = NULL


  if(compare$comp) {

    isig = "Yes"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true Uncertainty coefficient is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = "No"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "There is no correlation at the confidence level  p-value.",
                  "P-value:",p.value, compare$str ,"estimated p-value:",pv)

      message(msg)
    }

  }


  return( list( infer= infer , infer.value = r , stat = stat, stat.value = pv ,
                isig = isig, msg = msg , var1 = ny, var2 = nx ) )

}

#CramersV Calculations
cramersvp = function(y, x, p.value, comp, verbose, cramersV.args = list(), ...){

  if( is.data.frame(x) )  x = x[[1]]
  if( is.data.frame(y) )  y = y[[1]]

  infer = "Cramer's V"
  stat = "P-value"

  args = c(list(y),list(x),cramersV.args)

  pv = stats::chisq.test(y,x,simulate.p.value=TRUE)$p.value
  r = do.call(lsr::cramersV,args)
  compare = comparepv(x = pv,pv = p.value,comp = comp)
  msg = NULL


  if(compare$comp) {

    isig = "Yes"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true Uncertainty coefficient is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = "No"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "There is no correlation at the confidence level  p-value.",
                  "P-value:",p.value, compare$str ,"estimated p-value:",pv)

      message(msg)
    }

  }

  return( list( infer= infer , infer.value = r , stat = stat, stat.value = pv ,
                isig = isig, msg = msg , var1 = ny, var2 = nx ) )

}

# Distance Correlation Calculations
dcorp = function(y, x, p.value, comp, verbose, dcor.args = list(), ...){

  infer = "Distance Correlation"
  stat = "P-value"

  args = c(list(y),list(x),dcor.args)

  dc = do.call(energy::dcorT.test , args)
  pv = dc$p.value
  r = as.numeric(dc$estimate)
  compare = comparepv(x = pv,pv = p.value,comp = comp)
  msg = NULL


  if(compare$comp) {

    isig = "Yes"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true Uncertainty coefficient is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = "No"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "There is no correlation at the confidence level  p-value.",
                  "P-value:",p.value, compare$str ,"estimated p-value:",pv)

      message(msg)
    }

  }

  return( list( infer= infer , infer.value = r , stat = stat, stat.value = pv ,
                isig = isig, msg = msg , var1 = ny, var2 = nx ) )

}
# Pearson Calculations
corperp = function(y, x, p.value, comp, verbose,pearson.args = list(), ...){

  if( is.data.frame(x) ) x = x[[1]]
  if( is.data.frame(y) ) y = y[[1]]

  infer = "Pearson Correlation"
  stat = "P-value"

  pearson.args$alternative = alternative #from global
  pearson.args$method = "pearson"
  args = c(list(y),list(x),pearson.args)

  res = do.call(stats::cor.test,args = args)
  pv = res[["p.value"]]
  r = res[["estimate"]]
  compare = comparepv(x = pv,pv = p.value,comp = comp)
  msg = NULL


  if(compare$comp) {

    isig = "Yes"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true Uncertainty coefficient is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = "No"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "There is no correlation at the confidence level  p-value.",
                  "P-value:",p.value, compare$str ,"estimated p-value:",pv)

      message(msg)
    }

  }

  return( list( infer= infer , infer.value = r , stat = stat, stat.value = pv ,
                isig = isig, msg = msg , var1 = ny, var2 = nx ) )

}


#MIC calculations
micorp = function(y, x, p.value, comp, verbose, mic.args = list(), ...) {

  if( is.data.frame(x) ) x = x[[1]]
  if( is.data.frame(y) ) y = y[[1]]

  infer = "Maximal Information Coefficient"
  stat = "P-value"

  args = c(list(y),list(x),mic.args)

  pv = ptest(y,x,FUN = function(y,x){
  args = c(list(y),list(x),mic.args)
  do.call(function(...) {z = minerva::mine(...); return(z$MIC) } , args )
  })
  #ptest(y,x,FUN = function(y,x) {minerva::mine(y,x)$MIC} )
  compare = comparepv(x = pv,pv = p.value,comp = comp)
  msg = NULL
  r = do.call(function(...) {z = minerva::mine(...); return(z$MIC) } , args )

  if(compare$comp) {

    isig = "Yes"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true Uncertainty coefficient is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = "No"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "There is no correlation at the confidence level  p-value.",
                  "P-value:",p.value, compare$str ,"estimated p-value:",pv)

      message(msg)
    }

  }

  return( list( infer= infer , infer.value = r , stat = stat, stat.value = pv ,
                isig = isig, msg = msg , var1 = ny, var2 = nx ) )



}


#Uncertainty coefficient Calculations
uncorp = function(y, x, p.value, comp, verbose, uncoef.args = list(), ...) {

  if( is.data.frame(x) )  x = x[[1]]
  if( is.data.frame(y) )  y = y[[1]]

  args = c(list(y),list(x),uncoef.args)

  pv = ptest(y,x,FUN = function(y,x){
    args = c(list(y),list(x),uncoef.args)
    do.call(DescTools::UncertCoef , args )
  })
  #pv = ptest(y,x,FUN = function(y,x) DescTools::UncertCoef(y,x) )

  infer = "Uncertainty coefficient"
  stat = "P-value"
  compare = comparepv(x = pv,pv = p.value,comp = comp)
  msg = NULL
  r =do.call(DescTools::UncertCoef , args )



  if(compare$comp) {

    isig = "Yes"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true Uncertainty coefficient is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = "No"

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "There is no correlation at the confidence level  p-value.",
            "P-value:",p.value, compare$str ,"estimated p-value:",pv)

      message(msg)
    }

  }

  return( list( infer= infer , infer.value = r , stat = stat, stat.value = pv ,
                isig = isig, msg = msg , var1 = ny, var2 = nx ) )



}

#TODO
corpps = function(y, x, ...) {


  ppsr::score(y,x)


}



#parallel corr matrix
cor_par = function (...) {
  LoadVars()
  dim=NCOL(df)
  corp = foreach::foreach(i=1:dim,.export=c('cor_fun',"comp", "verbose","cor.n",
  "cor.nc" , "cor.cc", "lm.args","dcor.args","mic.args","pps.args",
  "cramersV.args","uncoef.args","verbose","p.test.n.sum","ptest.r") ) %:%
    foreach::foreach (j=1:dim) %dopar% {
      corp = cor_fun(df = df,pos_1 = i,pos_2 = j, p.value=p.value,...)
    }
  matrix(unlist(corp), ncol=ncol(df))
}


cor_fun = function(df,
                   pos_1,
                   pos_2,
                   p.value,
                   verbose,
                   alternative,
                   comp,
                   cor.nn,
                   cor.nc,
                   cor.cc,
                   ptest.n.sum,
                   ptest.r,
                   lm.args,
                   pearson.args,
                   cramersV.args,
                   dcor.args,
                   pps.args,
                   mic.args,
                   uncoef.args,...){

  y = df[pos_1]
  x = df[pos_2]
  cly = class(y[[1]])
  clx = class(x[[1]])
  nx = names(x)
  ny = names(y)

  # both are numeric

  cond.nn = ( cly %in% c("integer", "numeric") && clx %in% c("integer", "numeric") )
  cond.nc = ( cly %in% c("integer", "numeric") && clx %in% c("factor", "character") )
  cond.cc = ( cly %in% c("factor", "character") && clx %in% c("factor", "character") )

  cond.all = all( !cond.nn,!cond.nc,!cond.cc )

  if(cond.all){

    z = x
    nz =nx
    clz = clx

    x = y
    nx = ny
    clx = cly
    y = z
    ny = nz
    cly = clz

    cond.nn = ( cly %in% c("integer", "numeric") && clx %in% c("integer", "numeric") )
    cond.nc = ( cly %in% c("integer", "numeric") && clx %in% c("factor", "character") )
    cond.cc = ( cly %in% c("factor", "character") && clx %in% c("factor", "character") )



  }

  # both are numeric/integer
  if( cond.nn ){

    switch (cor.nn,
            "pearson" = {computeCorN = corperp
            },
            "mic" = { computeCorN = micorp

            },
            "dcor" = { computeCorN = dcorp

            },
            "corpps" = {computeCorN = corpps

            }
    )


    r = try(
      eval(body(computeCorN), list(), enclos=environment())
    )

  }

  # one is numeric and other is a factor/character
  if( cond.nc ){

    switch (cor.nc,
            "lm" = {computeCorN = corlm
            },
            "pps" = { computeCorN = corpps

            }
    )


    r = try(
      eval(body(computeCorN), list(), enclos=environment())
    )


  }

  # both are factor/character
  if( cond.cc ){

    switch (cor.cc,
            "cramersV" = {computeCorN =  cramersvp
            },
            "uncoef" = { computeCorN = uncorp
            },
            "corpps" = { computeCorN = corpps

            }
    )


    r = try(
      eval(body(computeCorN), list(), enclos=environment())
    )


  }

  if((class(r) %in% "try-error")){

    if(verbose){
      warnings(cat("ERROR: some operations produces Nas values.","\n",
                 ny, " FUN " ,nx,"\n"))
    }

    r =  list( infer= NA , infer.value = NA , stat = NA, stat.value = NA ,
               isig = 'No', msg = r , var1 = ny, var2 = nx )
  }

  return(r)

}


#compare p-value alternatives
comparepv = function(x,pv,comp = c('l','g')){

  comp = match.arg(comp)

  if(comp == 'g') {

    str = '<'
    comp = pv > x

  } else {

    str = '>'
    comp = pv < x

  }

  return( list( "comp" = comp, "str" = str) )

}

#Load All Parent Variables to Child Function
load_vars <- function(){
  variables <- ls(envir = parent.frame(2))

  for (var in variables) {
    assign(var, get(var, envir = parent.frame(2)),envir = parent.frame(1))
  }
}

