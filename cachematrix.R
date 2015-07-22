## makeCacheMatrix creates a matrix object that
## can cache its inverse.
## cacheSolve retrieves and returns an inverse that
## has already been calculated or else it calculates
## the inverse of the matrix returned by 
## makeCacheMatrix.

## makeCacheMatrix creates a matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Initializes inv in local environment
        ## Creates a matrix ("x") and set its values ("y")
        ## in the global environment.
                set <- function(y) {
                x <<- y 
                # Creates matrix x and sets its values from input
                # matrix y
                inv <<- NULL
                # Initializes inverse in global envrironment
                }
        get <- function() x # Reports current values of matrix
        setinverse <- function(solve) inv <<- solve
        # Sets values of inverse in global environment
        getinverse <- function() inv # Reports values of inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        # Establishes lists of functions within main function.
}


## cacheSolve retrieves and returns an inverse that
## has already been calculated or else it calculates
## the inverse of the matrix returned by 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() # Get current value of inv in global environment
        ## Return cached values of inverse if they exist.
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        ## Compute and return inverse values.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
