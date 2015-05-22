## The following functions compute the inverse of a given matrix. 

## makeCacheMatrix stores a list of functions to: set and retrieve the matrix value, set and 
## retrieve the cache value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        # set matrix value.
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        # retrieve matrix value.
        x
    }
    setinv <- function(inv) {
        # set inverse matrix value.
        inverse <<- inv
    }
    getinv <- function() {
        # retrieve inverse matrix value.
        inverse
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve performs the matrix inversion. If the inverse has already been calculated 
## and the matrix has not changed, the cached value is returned instead.

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        # already calculated the inverse, get the value from cache.
        message("getting cached data")
        inverse
    }
    else {
        # get the matrix and compute its inverse. store inverse in cache.
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinv(inverse)
        inverse
    }
}
