##################################### QUIZ / HW 1 FUNCTIONS ##########################################

#' Problem 1
#' Calculate Mean, Variane, SD
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param g vector
#'
#' @return list
#' @export
#' @examples
#' func1(rnorm(10))

func1 <- function(x){
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' Problem 2
#' Calculate Mean, Variane, SD (again)
#'
#' Computes the mean, variance and sd of a vector, but with user checks
#'
#' @param g vector
#'
#' @return list
#' @export
#' @examples
#' func2(rnorm(10))
func2 <- function(x){
  stopifnot(is.numeric(x))
  stopifnot(length(x)!=0)
  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(x))
  
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' Problem 3
#' MLE of gamma distribution
#'
#' Computes the liklihood of a gamma distribution
#'
#' @param b vector
#'
#' @return scalar
#' @export
#' @examples
#' func3(rnorm(10))
func3 <- function(x){
  alpha <- pi
  log <- function(alpha)
    sum(dgamma(x, shape = alpha, log = TRUE))
  interval <- mean(x) + c(-1,1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)
  
  oout<- optimize(log, maximum = TRUE, interval)
  return (oout$maximum)
}

#' Problem 4
#' Weighted mean, var, sd
#'
#' Computes the weighted mean, var, sd
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' data(d)
#' func4(d)
func4 <- function(d){
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Problem 5
#' Weighted mean, var, sd with user checkes
#'
#' Computes the weighted mean, var, sd with user checks
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
#' func5(d)
func5 <- function(d){
  
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
  
  stopifnot(all.equal(sum(d$p),1))
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Problem 6
#' Highlevel check function
#'
#' Checks and throws error if not numeric, finit, zero lenth, NA, NAN
#'
#' @param x object
#'
#' @return object
#' @export
#' @examples
#' func6(NA)

func6 <- function(x){
  
  tryCatch(stopifnot(is.numeric(x)), error=function(e){print("not numeric")})
  tryCatch(stopifnot(is.finite(x)), error=function(e){print("not finite")})
  tryCatch(stopifnot(length(x)!=0), error=function(e){print("has 0 length")})
  tryCatch(stopifnot(!is.nan(x)), error=function(e){print("NA or NAN")})
  tryCatch(stopifnot(!is.na(x)), error=function(e){print("NA or NAN")})
  
}

#'Problem 7
#'  MLE 
#'
#' Computes the liklihood of a given distribution for data x
#'
#' @param x vector
#' @param func function, e.g., `function(theta, x) dgamma(x, shape = theta, log = TRUE)`
#' @param interval vector, i.e., interval for optimize function
#'
#' @return scalar
#' @export
#' @examples
#' func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
#' result7_gamma <- func7(x1,func1,c(0,3))
#' result7_gamma
#' 
func7 <- function(x, func, interval){
  
  f7 <- function(theta, x)
  {sum(func(theta, x))}
  
  oout<- optimize(f7, maximum = TRUE, interval, x=x)
  return(oout$maximum)
} 

##################################### QUIZ / HW 2 FUNCTIONS ##########################################
#' Problem 1
#' matrix multiplication with user checks   
#'
#'
#'multiplies transposed vector by the inverse of a matrix times the vector
#' @param x vector
#' @param a matrix
#' 
#'
#' @return vector (1 dimensional)
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' Fun1(a, x)
#' 

Fun1 <- function(A, x){
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(A))
  stopifnot(is.finite(x))
  stopifnot(is.finite(A))
  stopifnot(length(x) > 0)
  stopifnot(length(A) > 0)
  stopifnot(length(ncol(A)) > 0)
  stopifnot(length(nrow(A)) > 0)
  stopifnot(length(x)==nrow(A))  ##make sure length of x is equal to dimensions of square matrix 
  stopifnot(nrow(A) == ncol(A))  ##square matrix
  stopifnot(length(x)==ncol(A))
  
  
  
  t(x) %*% solve(A) %*% x    #matrix multiplication
}
 #### import inverted data and regular version of that data



#' Problem 2
#' matrix multiplication with user checks using a binary operator
#'
#'
#' @param x vector
#' @param a matrix
#'
#' @return vector (1 dimensional)
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' a %Fun2% x
#' 
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))

"%Fun2%" <- function(A, x){
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(A))
  stopifnot(is.finite(x))
  stopifnot(is.finite(A))
  stopifnot(length(x) > 0)
  stopifnot(length(A) > 0)
  stopifnot(length(ncol(A)) > 0)
  stopifnot(length(nrow(A)) > 0)
  stopifnot(length(x)==nrow(A))  ##make sure length of x is equal to dimensions of square matrix 
  stopifnot(nrow(A) == ncol(A))  ##square matrix
  stopifnot(length(x)==ncol(A))
  
  
  t(x) %*% solve(A) %*% x    #matrix multiplication
}



#' same function for problem 3 and problem 4
#'standardizes columns of numeric matrix
#' 
#' subtracts the mean of a matrix from the matrix and divides by the matrix's standard deviation
#'
#' @param a matrix
#'
#' @return matrix
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#' Fun3(a)
#' 


Fun3 <- function(A){
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
  stopifnot(is.numeric(A))
  stopifnot(is.finite(A))
  stopifnot(length(A) > 0)
  stopifnot(ncol(A) > 0)
  stopifnot(nrow(A) > 1) 
  
  mean <- colMeans(A)
  sd <- apply(A,2,"sd")
  ( A - mean)/sd
  
}




#'Problem 5
#' myapply function 
#'
#' a function like the apply() function
#'
#' @param x matrix
#'
#' @return varies
#' @export
#' @examples
#' fred <- matrix(1:6, ncol = 2)
#' myapply(fred,1,"mean")
#' myapply(fred,2,"mean")
#' 
#' myapply(fred,1,"max")
#' myapply(fred,2,"max")
#' 
#' myapply(fred,1,function(x) quantile(x,0.75))
#' myapply(fred,2,function(x) quantile(x,0.75))
#' 
#' myapply(fred, 1, quantile, probs=.75)
#' myapply(fred, 2, quantile, probs=.75)
#' 
#' myapply(fred, 1, quantile, probs = (1:3)/4)
#' myapply(fred, 2, quantile, probs = (1:3)/4)
#' 
#' moo <- myapply(fred, 1, mean)
#' fred.mean.free.rows <- sweep(fred, 1, moo)
#' rowMeans(fred.mean.free.rows)

myapply <- function(X, MARGIN, FUN, ...)
{
  
  #stopifnot(length(dim(X))==2)
  
  if(length(dim(X))!=2)
  {
    stop("matrix is not 2d")
  } 
  if(!(MARGIN %in% c(1,2)))
  {
    stop("margin is not in 1 or 2")
  }
  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)
  
  if (MARGIN == 1)
  {
    result = list()
    for(i in 1:R)
    {
      result[[i]] = f(X[i,],...)
    }
  }else if(MARGIN == 2)
  {
    result = list()
    for(j in 1:C)
    {
      result[[j]] = f(X[,j],...)
    }
  }
  return(simplify2array(result))
}

#### try against just apply()


