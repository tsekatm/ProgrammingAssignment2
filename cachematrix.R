## This function is able to cache time consuming computations. 
## For instance, if the matrix has not changed, it makes sense
## to cache the inverse of the matrix so that when its needed
## again, it can be looked up in the cache rather than to be recomputed

## makeCacheMatrix: this function creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the inverse of the special
## matrix returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve retrieves
## the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
