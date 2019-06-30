## The following functions demonstrate how to use lexical scoping to achieve caching to store the 
## results of computationally intensive operations.

## makeCacheMatrix creates an object containing the following
## 1) matrix 
## 2) place holder for the inverse of the matrix
## 3) function to set the matrix 
## 4) function to get the matrix 
## 5) function to set the inverse of the matrix
## 6) function to get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)   
}


## cacheSolve looks up the inverse of the matrix in object environment. If found returns the cached inverse.
## Otherwise, computes the inverse using solve, caches the inverse and returns the same

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

