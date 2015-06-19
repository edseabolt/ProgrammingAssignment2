## This file contains two functions. One for creating a cached version of the input matrix
## and the other for computing the inverse of the matrix. 

## This function creates a special "matrix" object that can cache its inverse.
##
## 'x' is the input matrix which is always considered to be square and
## invertible.
##
## Returns a list containing functions to operate on the cached matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    getInverse <- function() {
        i
    }
    
    list(
        set=set,
        get=get,
        setInverse=setInverse,
        getInverse=getInverse
    )
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
##
## 'x' is an instance of the cached matrix created by invoking makeCachedMatrix
## prior to calling this function.
##
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
