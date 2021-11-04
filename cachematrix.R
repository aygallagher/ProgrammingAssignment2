## These functions work together to create and cache the 
## inverse of a matrix.

## makeCacheMatrix creates the matrix whose inverse can be cached. 

makeCacheMatrix <- function(x = matrix()) {
    w <- NULL
    set <- function(y) {
        x <<- y
        w <<- NULL
    }
    get <- function() x
    setinv <- function(solve) w <<- solve
    getinv <- function() w
    list(set = set, get = get,
         setinv = setinv,
         getinv - getinv)
}



## cacheSolve finds the inverse of the matrix created by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
    w <- x$getinv()
    if(!is.null(w)) {
        message("getting cached data")
        return(w)
    }
    data <- x$get()
    w <- solve(data, ...)
    x$setinv(w)
    w
}
