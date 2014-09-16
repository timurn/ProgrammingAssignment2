## This functions implements computation of matrix inverse with the ability
## to use cache when possible and skip reapeated computation.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    ## Functions for setting and getting matrix data.
    set <- function(y) {
        x <<- y
        ## Reset inverse cache when changing matrix data.
        i <<- NULL
    }
    get <- function() x

    ## Functions for setting and getting matrix inverse.
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    ## Returning list, that contains functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix", returned by
## makeCacheMatrix, or taking it from cache, if possible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Trying to get inverse from cache.
    i <- x$getinverse()
    if(!is.null(i)) {
        ## Got matrix inverse from cache, returning.
        message("getting cached data")
        return(i)
    }

    ## No inverse in cache, need to compute.
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)

    i
}
