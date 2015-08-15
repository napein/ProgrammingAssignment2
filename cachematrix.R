# The following two functions together help to cache the inverse of a matrix
# rather than compute it repeatedly

# makeCacheMatrix creates a special "matrix" object that can cache its inverse
# by returning a list of functions that are used to set the value of the
# matrix, get the value of the matrix, set the value of the inverse, get the
# value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve computes the inverse of the special "matrix" returned by
# makeCacheMatrix. If the inverse has already been calculated (and the matrix
# has not changed), then the cachesolve retrieves the inverse from the cache.
# Otherwise it calculates the inverse and returns it after caching it.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("Getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
