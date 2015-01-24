## This file contains functions that caches the inverse of a 
## matrix and returns the cached inverse of a matrix if the
## matrix has not changed, otherwise ompute the inverse
## Assumption - The inverse exists for any matrix that is input.

## cache object that is used to cache the inverse of a matrix
## The object has a getter and a setter method to get/set the matrix
## It also has the getInverse and setInverse method to get the inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function( m ) {
        if ( !is.null( x ) & !identical( x, m ) ) {
            x <<- m
            inverse <<- NULL
        } else if ( is.null( x ) ) {
            x <<- m
            inverse <<- NULL
        } else {
            message( "No change since the matrix passed is the same as the one cached earlier" )
        }
    }
    
    get <- function() x
    
    getInverse <- function( m ) inverse
    
    setInverse <- function( inv ) inverse <<- inv
    
    list( set = set, get = get,
          setInverse = setInverse, getInverse = getInverse )
}


## This method computes the inverse of a matrix
## but before doing it, this method checks if the inverse already
## exists for the matrix in the cache, if found the method uses the
## cached value

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if ( !is.null( inv ) ) {
        message( "getting the cached inverse" )
        return( inv );
    }
    
    data <- x$get()
    inv <- solve( data )
    x$setInverse( inv )
        
    ## Return a matrix that is the inverse of 'x'
    inv
}