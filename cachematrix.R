## This code contains functions for calculating the inverse of a matrix
## and caching the results.
## A cache for a matrix and the inverse (inv) of that matrix
##
## arguments: x, the matrix to be cached, default = empty matrix
##
## returns: a list with functions to access the cache
##          - get: gets the cached matrix
##          - set: sets a new matrix to be cached
##          - getinv: gets the cached inverse of the matrix
##          - setinv: sets the inverse of the matrix to be cached
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set = set, get=get, setinv=setinv, getinv=getinv)
}

## code to solve a cached-matrix (x) and cache the result
##
## arguments: x, the cached-matrix (made using makeCacheMatrix)
##            ... extra agruments pased to the solve function
##
## returns: the inverse of the matrix, if possible taken form the cache
##          prints a message if the inverse was taken form cache
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## Testing the R code for caching 
x <- matrix(c(2,5,4,7,8,6,5,4,7),3,3)
det(x)
z1 <- cacheSolve(makeCacheMatrix(x))
z2 <- solve(x)
z1==z2
## z1 == z2 indicates cache solver is working
