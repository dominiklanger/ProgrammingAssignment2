## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. The function makeCacheMatrix creates a special matrix 
## object that can cache its inverse when calculated with the function
## cacheSolve, allowing for performance increases.

## makeCacheMatrix creates a special matrix object from a normal 
## matrix object. This special matrix object can cache its inverse
## when calculated with the function cacheSolve below. 
## IMPORTANT: Use the set method to modify the value of a matrix 
## created with makeCacheMatrix and the get method to obtain its value.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) {
            inverse <<- inverse
      }
      getInverse <- function() {
            inverse
      }
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)      
}


## cacheSolve returns the inverse of matrices created with the function
## makeCacheMatrix. When called the first time on such a matrix, the inverse 
## is calculated. Each subsequent time, cacheSolve returns the inverse from 
## cache, thus typically increasing performance. Note that when you modify the 
## value of the matrix, the cache will be cleared and cacheSolve will have to 
## recalculate the inverse the next time you call it.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setInverse(inverse)
      inverse
}
