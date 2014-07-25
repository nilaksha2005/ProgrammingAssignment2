## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## An object which cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    ##variable to assign inverse
    inv <- NULL

    ##method to set the matrix
    set <- function( matrix ) {
            x <<- matrix
            inv <<- NULL
    }

    ##method to get the matrix
    get <- function() {
    	x
    }

    ##method to set inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ##method to get inverse of the matrix
    getInverse <- function() {
        inv
    }
}


## Write a short comment describing this function

## Calculate the inverse of the matrix returned by the above function "makeCacheMatrix".
## This function gets the inverse, if it is already calculated.
cacheSolve <- function(x, ...) {
    ## get the inverse of matrix x
    m <- x$getInverse()

    ## if already set, return the cached value
    if( !is.null(m) ) {
      return(m)
    }

    ## get the matrix
    mtx <- x$get()

    ## calculate the inverse
    m <- solve(mtx) %*% mtx

    ## set the inverse
    x$setInverse(m)

    ## return matrix
    m
}
