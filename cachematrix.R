## The functions below cache the inverse of a matrix
## Solution of Programming Assignment 2 for rprog-014 class on Coursera
## Author: Pankaj L. Ahire (pankajahire@gmail.com)

## makeCacheMatrix
## Creates an object wrapping a regular matrix that can cache its inverse.
## Args: x is a matrix object (Vector with a dimension of length 2)
## Outputs: An list object with functions for get, set, getinverse, setinverse
## Notes:
## * Uses <<- for assignment in parent environments for persistence.
## * Assumes x is an invertible matrix. Performs no special checks on x.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(someMatrix) {
    x <<- someMatrix
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(invertedMatrix) inverse <<- invertedMatrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## Computes inverse if it needs to be computed, otherwise return a cached inverse
## for speed and efficiency.
## Args: x is the list object returned by makeCacheMatrix above.
## Outputs: An inverted matrix of the matrix underlying the list object created by makeCacheMatrix
## Notes:
## * Assumes x is the list object returned by makeCacheMatrix above. Performs no checks to validate that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrixToInvert <- x$get()
  inverse <- solve(matrixToInvert, ...)
  x$setinverse(inverse)
  inverse
}
