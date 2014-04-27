## The functions makeCacheMatrix and cacheSolve are used to compute
## the inverse of a matrix and to cache the result so that subsequent
## calls to compute the inverse for the cachedMatrix quickly return a cached result
## instead of computing the inverse from scratch
##
## EXAMPLE USAGE:
## data = c(8,1,6,3,5,7,4,9,2)  # Define the data in a matrix that you want to invert
## m = matrix(data,3,3)         # The matrix is a 3x3 matrix
## b = makeCacheMatrix(m)       # Create a cached version of the matrix m
## inverse = cacheSolve(b)      # Get the inverse of the matrix


## makeCacheMatrix
##
## Given a Matrix, returns a list object that can be passed to the cacheSolve function
## (see cacheSolve below)
makeCacheMatrix <- function(x = matrix()) {
    ## cachedInverted is used to store the inverted matrix. Initialize with NULL
    cachedInverted <- NULL 
    set <- function(matrixToBeInverted) {
        # Set the matrix to be inverted
        x <<- matrixToBeInverted
        # Clear the cache (to force recalculation on subsequent calls)
        cachedInverted <<- NULL  
    }
    # Return the matrix to be inverted
    get <- function() x 
    # Set the cache to the matrix 'solve'
    setSolve <- function(solve) cachedInverted <<- solve 
    # Return the cached inverted matrix
    getSolve <- function() cachedInverted 
    
    # Return list object that allows each of the above functions to be called
    # (set, get, getSolve and getSolve)
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## cacheSolve
##
## Given a list object 'x' created by the makeCacheMatrix function (see above), return
## the inverse of the cached matrix
##
## The result is cached so that subsequent calls to cacheSolve for the same list
## quickly return a cached inverted matrix rather than recomputing the inverse which can
## be an expensive operation
cacheSolve <- function(x, ...) {
    # Try to get the cached inverted matrix
    cachedInverted <- x$getSolve()
    # If cachedInverted is not NULL, then we were successful in reading from cache, so just return cachedInverted
    if(!is.null(cachedInverted)) {
        message("getting cached data")
        return(cachedInverted)
    }
    # If m is NULL, then we were unsuccessful in reading from cache, so compute the inverse
    message("calculating inverse")
    # Get the matrix to be inverted
    matrix <- x$get()
    # Invert the matrix
    inverted <- solve(matrix, ...)
    # Cache the inverted matrix for next time
    x$setSolve(inverted)
    # Return the inverted matrix
    inverted
}
