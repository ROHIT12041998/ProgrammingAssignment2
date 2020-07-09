

## The  function calculates the mean of the special "vector" created.
## it first checks to see if the mean has already been calculated. If so, it gets the mean from 
## the storage and skips the computation.or else, it calculates the mean of the data 
## and sets the value of the mean in the cache via the setmean function.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inver <<- solveMatrix
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##  computes the inverse of the special "matrix" returned by makeCacheMatrix.
##  If the inverse is already calculated (and the matrix does not change),
## then the cachesolve should retrieve the inverse from the cache. Computes the inverse of a square matrix 

## which can be done with the solve function in R.
cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setInverse(inver)
  inver      
}
