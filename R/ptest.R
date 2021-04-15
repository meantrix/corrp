#'@title simple pertumation test
#'@description Execute one-sample simple permutation test on two numeric vector.
#'Its keep one vector constant and ‘shuffle’ the other by resampling.
#'This approximates the null hypothesis — that there is no dependency between the variables.
#'@param x A numeric vector.
#'@param y A numeric vector.
#'@param num.s number of samples with replacement created with y numeric vector.
#'@param alternative a character string specifying the alternative hypothesis,
#'must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.

#'@export
#'
ptest = function(x ,y,
                 FUN,
                 sep = 1L,
                 alternative = c("two.sided", "less", "greater"),
                 num.s = 20000, ... ) {


  FUN = match.fun(FUN)
  #check mandatory args
  fargs = formals(FUN)
  fargs = fargs[vapply(fargs, is.symbol, FUN.VALUE = TRUE)]
  fargs = names(fargs)
  largs = length(fargs)
  if( largs > 2 ) stop( 'FUN maximum number of mandatory arguments is 2.' )


  alternative = substr(alternative,1,1)
  checkmate::assert_character(alternative,len = 1, pattern = "t|l|g")
  checkmate::assert_logical(paired,len = 1)
  checkmate::assert_logical(all.p,len = 1)
  checkmate::assert_number(num.s)
  stopifnot(is.numeric(x),is.numeric(y))
  stopifnot(length(x) == length(y))


  obs = ifelse(largs > 2 , FUN(x,y), FUN(x) - FUN(y) )
  N = length(x)
  est = c()
  for(i in 1:num.s){
    y_i = sample(y, length(y), replace = T)
    est = append(estimates, ifelse(largs > 2 , FUN(x,y), FUN(x) - FUN(y) ) )

  }

  switch (alternative,
    'l' = {p.value = mean( est <= obs )
    },
    'g' = {p.value = mean( est >= obs)
    },
    't' = {p.value =  mean( abs(est) >= abs(obs) )}
  )

  return(p.value)
}













}
