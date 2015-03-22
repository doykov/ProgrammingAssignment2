## These two functions create a special wrapper for standard matrix
## that stores the inverse matrix if it was computed once.

## makeCacheMatrix creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    x_reverse <- NULL
    set <- function(y) {
        x <<- y
        x_reverse <<- NULL
    }
    get <- function() x
    setreverse <- function(x_rev) x_reverse <<- x_rev
    getreverse <- function() x_reverse
    list(set = set, get = get, setreverse = setreverse,
         getreverse = getreverse)
}


## cacheSolve returns cached inverse of 'x' or computes it for the first time

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_reverse <- x$getreverse()
    if (!is.null(x_reverse)) {
        message("getting cached data")
        return(x_reverse)
    }
    m <- x$get()
    x_reverse <- solve(m, ...)
    x$setreverse(x_reverse)
    x_reverse
}
