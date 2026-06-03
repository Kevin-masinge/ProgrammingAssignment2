## This file implements caching for a matrix inverse.
## It avoids recomputing the inverse by storing (caching) it after the first calculation.

## Creates a special matrix object that can store its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## Computes the inverse of the matrix or returns the cached result if available
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setInverse(inv)
  
  inv
}

