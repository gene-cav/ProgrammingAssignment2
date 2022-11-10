### Gene Cavagnaro
### R Programming on Coursera
### Programming Assignment 2
### November 9, 2022


## makeCacheMatrix: argument is a matrix. Returns a list of functions to:
      # set: store/modify the matrix
      # get: retrieve the matrix
      # setinverse: store/modify the inverse (called by cacheSolve)
      # getinverse: retrieve the inverse if previously set by setinverse.

## cacheSolve: argument is list of functions (as returned by makeCacheMatrix)
      # returns the inverse of the matrix, storing it if necessary.


## Cache a matrix x and return a list of functions to access matrix and inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
            # if the matrix's value is reset, forget the cached inverse
      }
      get <- function() x
      setinverse <- function(inverted) inv <<- inverted
      getinverse <- function() inv
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}



## Retrieve stored inverse of matrix, or else compute and store inverse.
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}
