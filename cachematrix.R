## These functions are used to create matrix-like objects
## that store their own inverse so that the inverse does
## not need to be calculated repeatedly.

## makeCacheMatrix creates a "matrix" that is really a
## list of four functions: functions to set and get the
## matrix and functions to set and get the inverse. The
## inverse is not calculated automatically but is cached
## once it is computed with the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve takes a cache matrix object (created by the
## createCacheMatrix function) and returns its inverse,
## which it gets from the cache if previously calculated.
## Input matrices are assumed to be invertible.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
