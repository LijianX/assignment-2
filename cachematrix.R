## The following are functions called makeCacheMatrix and acheSolve
## which can cache and compute the inverse of a matrix

## This function creates a "matrix" object，then cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y;
                m <<- NULL;
        }
        get <- function() return(x);
        setinv <- function(inv) m <<- inv;
        getinv <- function() return(m);
        return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## function'makeCacheMatrix' compotes the inverse of the special
## "Matrix" reutrned by this function. If the inverse has been calculated, 
## and the matrix is still the same, then function'cacheSolve' can retrieve
## the inverse from the cache.

acheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("Getting cached data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        return(m)
}
