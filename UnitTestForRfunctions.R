context("Homework functions")
########################### HOMEWORK 1 FUNCTIONS########################################
test_that("func1 computes mean, var, sd", {
         x <- 1:10
         var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
         x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
         expect_identical(func1(x), x_list)
         })


test_that("func2 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func2(x), x_list)
  save<-try(func2(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})

test_that("func3 finds the mle of a gamma distribution", {
   genericFun <- function(x, func, interval){
     f7 <- function(theta, x)
     {sum(func(theta, x))}
     oout<- optimize(f7, maximum = TRUE, interval, x=x)
     return(oout$maximum)
   } 
   func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE) ##putting a gamma distribution into generic mle function
   result7_gamma <- genericFun(x,func1,c(0,3))
   result7_gamma
 
   expect_identical(func3(x), result7_gamma)
 })


test_that("func4 computes mean, var, sd", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  mean4 <- function(d){sum(d$x*d$p)}
  var4<-function(d){sum((d$x-mean4)^2 *d$p)}
  x4_list<-list(mean=mean4(d),var=var4(d),sd=sqrt(var4(d)))
  expect_identical(func4(x), x4_list)

})


test_that("func5 computes mean, var, sd with user checks", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)

   alternateFun <- function(d){
    tryCatch(stopifnot(is.numeric(d$x)), error=function(d){print("not numeric")})
    tryCatch(stopifnot(is.finite(d$x)), error=function(d){print("not finite")})
    tryCatch(stopifnot(length(d$x)!=0), error=function(d){print("has 0 length")})
    tryCatch(stopifnot(!is.nan(d$x)), error=function(d){print("NA or NAN")})
    tryCatch(stopifnot(!is.na(d$x), error=function(d){print("NA or NAN")})
    tryCatch(stopifnot(is.numeric(d$p)), error=function(d){print("not numeric")})
    tryCatch(stopifnot(is.finite(d$p)), error=function(d){print("not finite")})
    tryCatch(stopifnot(length(d$p)!=0), error=function(d){print("has 0 length")})
    tryCatch(stopifnot(!is.nan(d$p)), error=function(d){print("NA or NAN")})
    tryCatch(stopifnot(!is.na(d$p), error=function(d){print("NA or NAN")})
    mean5 <- function(d){sum(d$x*d$p)}
    var5<-function(d){sum((d$x-mean5)^2 *d$p)}
    return(list(mean=mean5(d),var=var5(d),sd=sqrt(var5(d))))
  }

  expect_identical(func5(d), alternateFun(d))
})


test_that("func6 computes mean, var, sd with user checks", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)

  altFun <- function(d){

    stopifnot(is.numeric(d$x))
    stopifnot(is.numeric(d$p))

    stopifnot(length(d$x)!=0)
    stopifnot(length(d$p)!=0)

    stopifnot(is.finite(d$x))
    stopifnot(is.finite(d$p))

    stopifnot(!is.na(d$x))
    stopifnot(!is.na(d$p))

    stopifnot(!is.nan(d$x))
    stopifnot(!is.nan(d$p))

    mean5 <- function(d){sum(d$x*d$p)}
    var5<-function(d){sum((d$x-mean5)^2 *d$p)}
    return(list(mean=mean5(d),var=var5(d),sd=sqrt(var5(d))))
  }

  expect_identical(func6(d), altFun(d))



test_that("func7 finds the mle using gamma as example", {
  gammaFUNC <- function(x){
  alpha <- pi
  log <- function(alpha)
  sum(dgamma(x, shape = alpha, log = TRUE))
  interval <- mean(x) + c(-1,1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)

  oout<- optimize(log, maximum = TRUE, interval)
  return (oout$maximum)
    }

    expect_identical(func7(x), gammaFUNC(x, func, interval))
  })


########################### HOMEWORK 2 FUNCTIONS ########################################
test_that("check that Fun1 does matrix multiplication correctly", {
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
  altFun1 <- function(A, x){
    library(MASS)
    load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(A))
    stopifnot(is.finite(x))
    stopifnot(is.finite(A))
    stopifnot(length(x) > 0)
    stopifnot(length(A) > 0)
    stopifnot(length(ncol(A)) > 0)
    stopifnot(length(nrow(A)) > 0)
    stopifnot(length(x)==nrow(A))
    stopifnot(nrow(A) == ncol(A))
    stopifnot(length(x)==ncol(A))

    t(x) %*% ginv(A) %*% x    #using ginv() to find inverse
  }
  expect_identical(Fun1(a, x), altFun1(a,x))
})

test_that("check that Fun2 does matrix multiplication correctly using binary operator %*%", {
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
  "%altFun2%" <- function(A, x){
    library(MASS)
    load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(A))
    stopifnot(is.finite(x))
    stopifnot(is.finite(A))
    stopifnot(length(x) > 0)
    stopifnot(length(A) > 0)
    stopifnot(length(ncol(A)) > 0)
    stopifnot(length(nrow(A)) > 0)
    stopifnot(length(x)==nrow(A))
    stopifnot(nrow(A) == ncol(A))
    stopifnot(length(x)==ncol(A))

    t(x) %*% ginv(A) %*% x    #using ginv() to find inverse
  }
  expect_identical(Fun2(a, x), a %altFun2% x)
})


test_that("check that Fun3 / Fun4 standardizes columns of matrix", {
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
  altFun3 <- function(a){
    stopifnot(is.numeric(a))
    stopifnot(is.finite(a))
    stopifnot(length(a) > 0)
    stopifnot(ncol(a) > 0)
    stopifnot(nrow(a) > 1)

    for(i in 1:ncol(a))
    {
      a[,i] = (a[,i] - mean(a[,i]))/sd(a[,i])

      print(as.matrix(a[,i])) #prints stadardization of each column as a separate matrix
    }

  }
  expect_identical(Fun3(a), altFun3(a))
})
#THIS TEST (AND CORRESPONDING FUNCTION) ARE IDENTICAL TO PROBLEM 4 FROM HOMEWORK


 test_that("check that Fun5 results in same answers as using apply()", {

    fred <- matrix(1:6, ncol = 2)
    a <- myapply(fred, 1, mean)
    b <- apply(fred, 1, mean)
   expect_identical(a, b)
})


############################# DPLYR FUNCTION ###########################################

test_that("filteredData function filters by a probability greater than 0.05", {
  x <- d
  manualFilter<- subset(d, p>0.05)
  expect_identical(manualFilter, filterFun)

})


############################ GGPLOT FUNCTION ##########################################

test_that("ggplot2 wrapper works correctly", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  b <- ggplot2:::qplot(d)
  expect_identical(plotMyData(d), b)

})