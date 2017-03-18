## Put comments here that give an overall description of what your
## functions do

## This function stores a matrix with 4 method accessories in a list

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(mInv) inv <<- mInv
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## This is the main function that performs the calculation of the inverse
## Please note that I slightly modify the exercise, in fact in case the matrix is singular (det = 0)
## I use the gInv from the MASS function that I load at start so I add a supplementary check 
## In R you cannot find determinant equal to absolute 0 but it is a very small number E-16 (10^-16)
## therefore this is the reason why I added this check

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  if (det(data) < 10E-15) {
    inv <- ginv(data)
  }else{
    inv <- solve(data, ...)
  }
  x$setInv(inv)
  inv
}
