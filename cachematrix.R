## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly, and that is what this two functions do.

##  Creates a "cache.matrix object" with four "methods":
## get() : returns the matrix
## set() : sets a new matrix
## getInverse() : returns the inverse of the matrix if it has been saved
## setInverse() : sets the inverse of the matrix
##
## Arguments:
## x : matrix
##
## Returns:
## "cache.matrix object"
makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Calculates the inverse of the given "cache.matrix" if it has not been
## saved before, and saves it in the "cache.matrix object"
##
## Arguments:
## x : cache.matrix object
## ... : other arguments for the solve function
##
## Returns:
## The inverse of the matrix
cacheSolve <- function(x, ...) {

    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    m <- x$get()
    inverse <- solve(m, ...)
    x$setInverse(inverse)
    inverse
}
