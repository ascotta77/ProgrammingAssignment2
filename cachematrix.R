## These functions will input a square matrix and return its inverse
## if no cached version is available. If it is available it will return
## the cached data.

## makeCacheMatrix inputs a square matrix 'x' 
## and returns a list of variables that then input into cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        inverse = NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverter) inverse <<- inverter
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function first checks for the cache data for the inverse of input matrix 'x'.
## If the cache exists, it is returned. If not, the inverse is solved and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse = solve(data,...)
        x$setinverse(inverse)
        inverse
}
