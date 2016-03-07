## Programming Assignment 2: Lexical Scoping
## Caching the inverse of a matrix
## The following functions target the potentially time-consuming computations
## of calculating the inverse of a matrix by caching the inverse of a matrix
## once calculated and using it unless matrix value has changed (which is faster
## as the inverse is not unnecessarly recalculated everytime it is called for)


## makeCacheMatrix() stores a matrix and returns the cached value of its inverse
## Returns: A list of the following functions:
## * set: "sets up"/initiates the matrix
## * get: "fetches the matrix
## * setInverse: "sets up"/initiates the cached version of the inverse matrix
## * getInverse: fetches the cached version of the inverse matrix
## Note: Actual inverse calculation is done in the cacheSolve function. This
## function is more like a shell function to manage the matrix and the caches.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve() computes the inverse of the special matrix created by makeCacheMatrix().
## If inverse is already calculated (and matrix is unchanged), it fetches 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Returns inverse of a matrix 'x' (assumes 'x' is invertible)
    ## Uses solve() to invert and makeCacheMatrix() to 'speed up'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}