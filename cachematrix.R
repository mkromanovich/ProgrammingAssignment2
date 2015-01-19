## Below is a pair of functions that creates a special object that stores
## a given square nonsingular invertible matrix and caches its inverse. Thus 
## it saves time on repeated computations. 

## 'makeCacheMatrix' function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    } 
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## 'cacheSolve' function computes the inverse of the special "matrix" 
## returned by 'makeCacheMatrix'. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message ("Getting cached data...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
