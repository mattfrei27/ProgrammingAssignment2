## The functions in this file are for efficiently handling matrix 
## inversion. One function creates an object that stores a matrix
## and its inverse. A second function calculates the inverse and 
## caches it in the object created.
## Written by Matt Frei on 11/16/2015


## makeCacheMatrix creates a matrix object that stores the matrix data
## and, if already calculated, its inverse. The object includes methods
## for getting and setting both the matrix and its inverse.



makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setInv <- function(inv) inverse <<- inv
     getInv <- function() inverse
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}



## cacheSolve takes an object created by makeCacheMatrix and calculates
## its inverse. The inverse is stored in the object and returned.

cacheSolve <- function(x, ...) {
     inverse <- x$getInv()
     if(!is.null(inverse)) {
          return(inverse)
     }
     else {
          matrix <- x$get()
          inverse <- solve(matrix, ...)
          x$setInv(inverse)
          inverse
     }
}
