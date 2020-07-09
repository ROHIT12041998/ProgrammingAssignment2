## Put comments here that give an overall description of what your
## functions do

## The following function calculates the mean of the special "vector" created with the above function.
## However, it first checks to see if the mean has already been calculated. If so, it gets the mean from 
## the cache and skips the computation. Otherwise, it calculates the mean of the data 
## and sets the value of the mean in the cache via the setmean function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache. Computing the inverse of a square matrix can be
## done with the solve function in R. For example,
## if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
