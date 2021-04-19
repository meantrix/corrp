#auxiliar functions

#linear regression Calculations
corlm = function(y, x, p.value, comp, verbose, lm.args =list()){

  nx = ny = ""
  if( is.data.frame(x) )  nx = names(x) ; x = x[[1]]
  if( is.data.frame(y) ) ny = names(y); y = y[[1]]

  args = c(list(y ~ as.factor(x)), lm.args)

  sum.res = summary(
    do.call(stats::lm, args )
  )

  pv = stats::pf (sum.res$fstatistic[1],sum.res$fstatistic[2],
                  sum.res$fstatistic[3],lower.tail=F)

  comp = comparepv(pv,p.value,comp)
  msg = NULL


  if(comp$comp) {
    r = sqrt(sum.res[["r.squared"]])

    if(verbose){
      msg = paste(ny,"vs.",nx,". \n",
              "alternative hypothesis: true correlation is not equal to 0","\n",
            "p-value: ",pv,"\n")
    }

  } else {
    r = NA

    if(verbose){
      msg = paste(ny,"vs.",nx,". \n",
              "there is no correlation at the confidence level  p-value. \n",
            "p-value:",p.value, comp$str ,"estimated p-value:",pv)
    }

  }

  return( list(value = r , p.value = pv, msg = msg, variables = c(ny,nx)) )

}

#CramersV Calculations
cramersvp = function(y, x, p.value, comp, verbose, cramersV.args = list()){

  nx = ny = ""
  if( is.data.frame(x) )  nx = names(x) ; x = x[[1]]
  if( is.data.frame(y) ) ny = names(y); y = y[[1]]

  args = c(list(y),list(x),args)

  pv = stats::chisq.test(y,x,simulate.p.value=TRUE)$p.value
  comp = comparepv(pv,p.value,comp)
  msg = NULL


  if(comp$comp) {

    r = do.call(lsr::cramersV,args)

    if(verbose){
      msg = paste(ny,"vs.",nx,". \n",
      "alternative hypothesis: true Cramer's V measure of association is not equal to 0","\n",
      "p-value: ",pv,"\n")
    }

  } else {

    r = NA

    if(verbose){
      msg = paste(ny,"vs.",nx,". \n",
                  "there is no correlation at the confidence level  p-value. \n",
            "p-value:",p.value, comp$str ,"estimated p-value:",pv)
    }

  }

  return( list(value = r , p.value = pv, msg = msg, variables = c(ny,nx)) )

}

# Distance Correlation Calculations
dcorp = function(y, x, p.value, comp, verbose, dcor.args = list()){

  args = c(list(y),list(x),dcor.args)

  dc = do.call(energy::dcorT.test , args)
  pv = dc$p.value
  r = as.numeric(dc$estimate)
  comp = comparepv(pv,p.value,comp)
  msg = NULL


  if(comp$comp) {

    if(verbose){
      msg = paste(names(y),"vs.",names(x),". \n",
        "alternative hypothesis: true distance correlation is not equal to 0","\n",
        "p-value: ",pv,"\n")
    }

  } else {

    r = NA
    if(verbose){
      msg = paste(names(y),"vs.",names(x),". \n",
            "there is no correlation at the confidence level  p-value. \n",
            "p-value:",p.value, comp$str ,"estimated p-value:",pv)
    }

  }

  return( list(value = r , p.value = pv, msg = msg, variables = c(ny,nx)) )

}
# Pearson Calculations
corperp = function(y, x, p.value, comp, verbose,pearson.args = list() ){

  nx = ny = ""
  if( is.data.frame(x) )  nx = names(x) ; x = x[[1]]
  if( is.data.frame(y) ) ny = names(y); y = y[[1]]

  pearson.args$alternative = alternative #from global
  pearson.args$method = "pearson"
  args = c(list(y),list(x),person.args)

  res = do.call(stats::cor.test,args = args)
  pv = res[["p.value"]]
  comp = comparepv(pv,p.value,comp)
  msg = NULL


  if(comp$comp) {
    r = res[["estimate"]]

    if(verbose){
      msg = paste(ny,"vs.",nx,". \n",
            "alternative hypothesis: true Pearson correlation is not equal to 0","\n",
            "p-value: ",pv,"\n")
    }

  } else {

    r = NA

    if(verbose){
      msg = paste(ny,"vs.",nx,". \n",
                  "there is no correlation at the confidence level  p-value. \n",
            "p-value:",p.value, comp$str ,"estimated p-value:",pv)
    }

  }

  return( list(value = r , p.value = pv, msg = msg, variables = c(ny,nx)) )

}


#MIC calculations
micorp = function(y, x, p.value, comp, verbose, mic.args = list()) {

  nx = ny = ""
  if( is.data.frame(x) )  nx = names(x) ; x = x[[1]]
  if( is.data.frame(y) ) ny = names(y); y = y[[1]]

  args = c(list(y),list(x),mic.args)

  pv = ptest(y,x,FUN = function(y,x){
  args = c(list(y),list(x),mic.args)
  do.call(function(...) {z = minerva::mine(...); return(z$MIC) } , args )
  })
  #ptest(y,x,FUN = function(y,x) {minerva::mine(y,x)$MIC} )
  comp = comparepv(pv,p.value,comp)
  msg = NULL


  if(comp$comp) {

    r = do.call(function(...) {z = minerva::mine(...); return(z$MIC) } , args )

    if(verbose){
      msg = paste(ny,"vs.",nx,". \n",
            "alternative hypothesis: true Maximal Information Coefficient is not equal to 0","\n",
            "p-value: ",pv,"\n")
    }

  } else {

    r = NA

    if(verbose){
      msg = paste(ny,"vs.",nx,". \n",
                  "there is no correlation at the confidence level  p-value. \n",
            "p-value:",p.value, comp$str ,"estimated p-value:",pv)
    }

  }

  return( list(value = r , p.value = pv, msg = msg, variables = c(ny,nx)) )



}


#Uncertainty coefficient Calculations
uncorp = function(y, x, p.value, comp, verbose, uncoef.args = list()) {

  nx = ny = ""

  if( is.data.frame(x) )  nx = names(x) ; x = x[[1]]
  if( is.data.frame(y) ) ny = names(y); y = y[[1]]

  args = c(list(y),list(x),uncoef.args)

  pv = ptest(y,x,FUN = function(y,x){
    args = c(list(y),list(x),uncoef.args)
    do.call(DescTools::UncertCoef , args )
  })
  #pv = ptest(y,x,FUN = function(y,x) DescTools::UncertCoef(y,x) )
  comp = comparepv(pv,p.value,comp)
  msg = NULL


  if(comp$comp) {

    r =do.call(DescTools::UncertCoef , args )


    if(verbose){
      msg = paste(ny,"vs.",nx,". \n",
                  "alternative hypothesis: true Uncertainty coefficient is not equal to 0","\n",
                  "p-value: ",pv,"\n")

      cat(msg)

    }

  } else {

    r = NA

    if(verbose){
      msg = paste(ny,"vs.",nx,". \n",
                  "there is no correlation at the confidence level  p-value. \n",
            "p-value:",p.value, comp$str ,"estimated p-value:",pv)

      cat(msg)


    }

  }

  return( list(value = r , p.value = pv, msg = msg, variables = c(ny,nx)) )



}

#TODO
corpps = function(y, x, ...) {


  ppsr::score(y,x)


}



#parallel corr matrix
cor_par = function (...) {
  LoadVars()
  browser()
  dim=NCOL(df)
  corp = foreach::foreach(i=1:dim,.export=c('cor_fun',"comp", "verbose","cor.n",
  "cor.nc" , "cor.cc", "lm.args","dcor.args","mic.args","pps.args",
  "cramersV.args","uncoef.args","verbose","p.test.n.sum","ptest.r") ) %:%
    foreach::foreach (j=1:dim) %dopar% {
      corp = cor_fun(df = df,pos_1 = i,pos_2 = j, p.value=p.value,...)
    }
  matrix(unlist(corp), ncol=ncol(df))
}


cor_fun = function(df, pos_1,pos_2,...){

  # both are numeric
  browser()
  if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
     class(df[[pos_2]]) %in% c("integer", "numeric")){

    switch (cor.n,
            "pearson" = {computeCorN = corperp
            },
            "MIC" = { computeCorN = micorp

            },
            "Dcor" = { computeCorN = dcorp

            },
            "corpps" = {computeCorN = corpps

            }
    )


    r = try(ComputeCorN(df[pos_1]
                     , df[pos_2]
                     , p.value
                     , comp
                     , verbose
                     , ...)
    )

  }

  # one is numeric and other is a factor/character

  if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
     class(df[[pos_2]]) %in% c("factor", "character")){

    switch (cor.n,
            "lm" = {computeCorN = corlm
            },
            "pps" = { computeCorN = corpps

            }
    )


    r = try(ComputeCorN(df[pos_1]
                        , df[pos_2]
                        , p.value
                        , comp
                        , verbose
                        , ...)
    )

  }

  # both are factor/character

  if(class(df[[pos_1]]) %in% c("factor", "character") &&
     class(df[[pos_2]]) %in% c("factor", "character")){

    switch (cor.n,
            "cramersV" = {computeCorN =  cramersvp
            },
            "uncoef" = { computeCorN = uncorp
            },
            "corpps" = { computeCorN = corpps

            }
    )


    r = try(ComputeCorN(df[pos_1]
                        , df[pos_2]
                        , p.value
                        , comp
                        , verbose
                        , ...)
    )

  }

  if((class(r) %in% "try-error")){

    warnings(cat("ERROR: some operations produces Nas values.","\n",
                 pos_1, " FUN " ,pos_2,"\n"))

    r = list(value = NA , p.value = NA, msg = NULL)
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

LoadVars <- function(){
  variables <- ls(envir = parent.frame(2))

  for (var in variables) {
    assign(var, get(var, envir = parent.frame(2)),envir = parent.frame(1))
  }
}

