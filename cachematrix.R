## These two functions, when used together, will store a cached inverse Matrix.
## First store the matrix using 
##    variable <- makeCacheMatrix(matrix(c(...), nrow=<number>, ncol=<number>))
## Then to get the inverse, use
##    cacheSolve(variable)


## This function is a list of functions which stores the matrix and its 
## inverse. It also includes functions to get and set both the matrix and 
## the inverse. 

makeCacheMatrix <- function(x = matrix()) {
     
     ## Clear m.
     m <- NULL
     
     ## Set the matrix and clear the cached inverse.
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     ## Get the stored matrix.
     get <- function() x
     
     ## Set the stored inverse.
     setInverse <- function(matrix) m <<- matrix
     
     ## Get the stored inverse.
     getInverse <- function() m
     
     ## Store the functions.
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## This function returns the inverse of the matrix passed to it.
## X must be of the type makeCacheMatrix.

cacheSolve <- function(x, ...) {
     ## Get the cashed inverse.
     m <- x$getInverse()
     
     ## If the inverse exists, return the cached inverse.
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## If the invers doesn't exist,
     
     ## Get the stored inverse.
     data <- x$get()
     
     ## Get the inverse of the stored matrix.
     m <- solve(data, ...)
     
     ## Cache the inverse.
     x$setInverse(m)
     
     ## Return the inverse.
     m
}
