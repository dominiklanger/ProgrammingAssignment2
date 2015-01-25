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
      inverse <- NULL # Make sure the cache is empty when the special matrix object is created.
      # The set method allows to set the matrix value after creation.
      set <- function(y) {
            x <<- y # Store the matrix value in the scope of the the parent frame.
            inverse <<- NULL # Clear the cache.
      }
      # The get method allows to obtain the matrix value.
      get <- function() {
            x # Return the matrix value stored in the scope of the parent frame.
      }
      # The set method allows to store a value in the cache.
      setInverse <- function(inverse) {
            inverse <<- inverse # Assign the value of the inverse in the scope of the the parent frame.
      }
      getInverse <- function() {
            inverse # Return the cache value stored in the scope of the parent frame.
      }
      # Create and return list with the methods defined above.
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## cacheSolve returns the inverse of matrices created with the function
## makeCacheMatrix. When called the first time on such a matrix, the inverse 
## is calculated. Each subsequent time, cacheSolve returns the inverse from 
## cache, thus typically increasing performance. Note that when you modify the 
## value of the matrix, the cache will be cleared and cacheSolve will have to 
## recalculate the inverse the next time you call it.

cacheSolve <- function(x, ...) {
      inverse <- x$getInverse() # Fetch the cache value.
      # Check if value from cache is valid and return it if ok:
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse) # Exit the function with the value found in the cache.
      }
      data <- x$get() # Obtain the matrix value.
      inverse <- solve(data, ...) # Calculate the inverse of the matrix.
      x$setInverse(inverse) # Cache the value for next time.
      inverse # Return the inverse.
}
