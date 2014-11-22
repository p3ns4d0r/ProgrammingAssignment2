## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # Creates a "cache.matrix object" with four "methods":
    # get() : returns the matrix
    # set() : sets a new matrix
    # getInverse() : returns the inverse of the matrix if it has been saved
    # setInverse() : sets the inverse of the matrix
    #
    # Arguments:
    # x : matrix
    #
    # Returns:
    # cache.matrix object
    
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # Calculates the inverse of the given "cache.matrix" if it has not been
    # saved before, and saves it in the "cache.matrix object"
    #
    # Arguments:
    # x : cache.matrix object
    # ... : other arguments for the solve function
    #
    # Returns:
    # 
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
