test_that("ptest works", {

 x = iris[[1]]
 y = iris[[2]]

 x1 = ptest(x,y,FUN = function(x,y) cor(x,y),num.s = 2000 ,alternative = 't')
 x2 = ptest(x,y,FUN = function(x,y) cor(x,y),num.s = 2000 ,alternative = 'g')
 x3 = ptest(x,y,FUN = function(x,y) cor(x,y),num.s = 2000 ,alternative = 'l')

 y1 = cor.test(x,y,alternative = 't')$p.value
 y2 = cor.test(x,y,alternative = 'g')$p.value
 y3 = cor.test(x,y,alternative = 'l')$p.value

 testthat::expect_equal(x1,y1,tolerance = 0.1)
 testthat::expect_equal(x2,y2,tolerance = 0.1)
 testthat::expect_equal(x2,y2,tolerance = 0.1)


})








