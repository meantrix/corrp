#auxiliar functions

#linear regression Calculations
.corlm = function(y, x, p.value, comp, verbose, lm.args =list(), ...){

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

  compare = .comparepv(x = pv,pv = p.value,comp = comp)
  r = sqrt(sum.res[["r.squared"]])
  msg = NULL


  if(compare$comp) {

    isig = TRUE

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true ",infer," is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = FALSE

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
.cramersvp = function(y, x, p.value, comp, verbose, cramersV.args = list(), ...){

  if( is.data.frame(x) )  x = x[[1]]
  if( is.data.frame(y) )  y = y[[1]]

  infer = "Cramer's V"
  stat = "P-value"

  args = c(list(y),list(x),cramersV.args)

  pv = stats::chisq.test(y,x,simulate.p.value=TRUE)$p.value
  r = do.call(lsr::cramersV,args)
  compare = .comparepv(x = pv,pv = p.value,comp = comp)
  msg = NULL


  if(compare$comp) {

    isig = TRUE

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true ",infer," is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = FALSE

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
.dcorp = function(y, x, p.value, comp, verbose, dcor.args = list(), ...){

  infer = "Distance Correlation"
  stat = "P-value"

  args = c(list(y),list(x),dcor.args)

  dc = do.call(energy::dcorT.test , args)
  pv = dc$p.value
  r = as.numeric(dc$estimate)
  compare = .comparepv(x = pv,pv = p.value,comp = comp)
  msg = NULL


  if(compare$comp) {

    isig = TRUE

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true ",infer," is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = FALSE

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
.corperp = function(y, x, p.value, comp, verbose,pearson.args = list(), ...){

  if( is.data.frame(x) ) x = x[[1]]
  if( is.data.frame(y) ) y = y[[1]]

  infer = "Pearson Correlation"
  stat = "P-value"

  pearson.args$alternative = alternative #from global
  pearson.args$method = "pearson"
  args = c(list(y),list(x),pearson.args)

  res = do.call(stats::cor.test,args = args)
  pv = res[["p.value"]]
  r = as.numeric(res[["estimate"]])
  compare = .comparepv(x = pv,pv = p.value,comp = comp)
  msg = NULL


  if(compare$comp) {

    isig = TRUE

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true ",infer," is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = FALSE

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
.micorp = function(y, x, p.value, comp, verbose, mic.args = list(), ...) {

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
  compare = .comparepv(x = pv,pv = p.value,comp = comp)
  msg = NULL
  r = do.call(function(...) {z = minerva::mine(...); return(z$MIC) } , args )

  if(compare$comp) {

    isig = TRUE

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true ",infer," is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = FALSE

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
.uncorp = function(y, x, p.value, comp, verbose, uncoef.args = list(), ...) {

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
  compare = .comparepv(x = pv,pv = p.value,comp = comp)
  msg = NULL
  r =do.call(DescTools::UncertCoef , args )



  if(compare$comp) {

    isig = TRUE

    if(verbose){
      msg = paste(ny,"vs.",nx,".",
                  "Alternative hypothesis: true ",infer," is not equal to 0.",
                  "P-value: ",pv,".")

      message(msg)
    }

  } else {

    isig = FALSE

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


#compare p-value alternatives
.comparepv = function(x,pv,comp = c('l','g')){

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



