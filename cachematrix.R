## Set of functions to cache and retrieve matrix inverses,
##  thus reducing time-consuming computations for large matrices.


## Creates an object to cache a matrix and its inverse if it's computed
## Methods:
## 1. set: sets the value of the matrix
## 2. get: gets the value of the matrix
## 3. setinverse: sets the value of the matrix inverse
## 4. getinverse: gests the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function( y ) {
        x <<- y
        mInv <<- NULL
    }
    get <- function() x
    setinverse <- function( theMInv ) mInv <<- theMInv
    getinverse <- function() mInv
    list( set = set, get = get, 
          setinverse = setinverse, getinverse = getinverse )
}


## Returns: the inverse of the matrix 
##  of the matrix cache object created with the above function
## Args: the matrix cache object to be used
## Assumptions: the matrix cache object contains a matrix;
##  the matrix is invertible;
## Behavior: Computes the inverse using solve(). 
##  If the inverse has already been calculated
##  (for the current matrix, i.e. and the matrix has not changed) the inverse
##  is retrieved from the cache instead of re-computed.
cacheSolve <- function(x, ...) {
    mInv <- x$getinverse()
    if ( !is.null(mInv) ) {
        message( "getting cached data" )
        return( mInv )
    }
    data <- x$get()
    mInv <- solve( data, ... )
    x$setinverse( mInv )
    mInv
}
