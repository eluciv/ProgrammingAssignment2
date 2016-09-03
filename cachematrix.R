## These functions are the result of programming assignment 2 solving.
## makeCacheMatrix(x = matrix()) - creates a custom kind-of-matrix object that can cache its inverse matrix.
## cacheSolve <- function(x, ...) - returns a matrix that is the inverse of 'x'. 
##                                  'x' must be an object created with the 'makeCacheMatrix' function.

## Creates a custom kind-of-matrix object that can cache its inverse matrix.
## 'x' - a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    set <- function(m) {
        x <<- m # store the new matrix
        inverseMatrix <<- NULL # clear the inverse matrix cache
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(invMatrix) {
        inverseMatrix <<- invMatrix
    }
    
    getInverse <- function() {
        inverseMatrix
    }
    
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## Returns a matrix that is the inverse of 'x'. 
## 'x' - an object created with the 'makeCacheMatrix' function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    message("Getting cached data...")
    result <- x$getInverse()
    
    if (is.null(result)) {
        message("Cache is empty. Calculating inverse matrix...")
        result <- solve(x$get(), ...)
        x$setInverse(result)
    }
    
    result
}
