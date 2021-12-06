## Put comments here that give an overall description of what your
## functions do
## R Programming Assignment 2
## Need to write an R function that is able to cache potentially time-consuming
## computations. 
## Assignment: Write a pair of functions for caching the Inverse of a Matrix (a costly computation)
## Assumption: The matrix supplied is always invertible 

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## this is a list containing functions to set/ get values of the matrix and
## set/ get value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y) { 
          x <<- y
          i <<- NULL
    
  }
  
  get <- function () x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special matrix created above. If the inverse has already been calculated
## (and the matrix hasn't changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
            message("getting cached data")
            return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
## Testing
M <- matrix(c(3,2,4,4),2,2)
MC <- makeCacheMatrix(M)
cacheSolve(MC)
cacheSolve(MC)
