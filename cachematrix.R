## These functions create a list of functions to store/get a matrix and it's inverse. 
## USAGE: make the cache matrix with your matrix and store it as a list as follows:
## CacheList <- makeCacheMatrix(YourMatrix)
##
## Call cacheSolve to calculate the inverse of your matrix if it has never been done
## or pull the inverse from the cache if it has already been calculated:
## cacheSolve(CacheList)

## This function creates a list of functions with your matrix

makeCacheMatrix <- function(mx = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    mx <<- y
    inv <<- NULL
  }
  getmatrix <- function() mx
  setinverse <- function(inve) inv <<- inve
  getinverse <- function() inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates a matrix inverse or pulls it from cache if it's already been calculated

cacheSolve <- function(mx, ...) {
  inv <- mx$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mx$getmatrix()
  inv <- solve(data)
  mx$setinverse(inv)
  inv
}
