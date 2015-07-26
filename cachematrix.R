##******************************************************
## 
##
## R programming Course
## Program Assignment 2
## Caching the inverse of a Matrix
##
## Course Code: rprog-030
## Author: Nicolas Iguchi
##
##
## These functions manages matrixes that are able to
## store its inverse in cache, making this operation
## faster when it needs to be calculated several times
##
##******************************************************


##------------------------------------------------------
## 
## function: makeCacheMatrix
##
## Creates a Matrix object that can cache its inverse
## 
## Author  : Nicolas Iguchi
## Date    : 2015-07-25
## 
##------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

  # object where the cached inverse value is stored
  cacheInverse <- NULL
  
  #----
  #   Getter and setter to access and write the matrix value
  #----
  get <- function() x
  
  set <- function(y) {
    #-----
    # If a new value is set to the matrix, reset the cache 
    # inversed value beacuse it needs to be recalculated
    #-----
    x <<- y
    cacheInverse <<- NULL
  }
  
  
  #-----
  # Getters and setter to access and write 
  # the cached inverse of the matrix
  #-----
  getInverse <- function() cacheInverse
  
  setInverse <- function(inverse) cacheInverse <<- inverse
  
  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##------------------------------------------------------
## 
## function: cacheSolve
##
## Returns the inverse of a matrix, checking
## if it is already stored in its cache.
##
## If the inverse is already stored in cache, 
## the function returns this value.
## Otherwise, the inverse is calculated on-the flight
##
##
## Author  : Nicolas Iguchi
## Date    : 2015-07-25
## 
##------------------------------------------------------
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    
  
  cachedInverse <- x$getInverse()
  
  #-----
  # Check if the inverse is already stored in cache
  #-----
  if(!is.null(cachedInverse)) {
    
    # Return the cached value
    message("getting cached data")
    return(cachedInverse)
  }
  else {
    #-----
    #  In case the value is not stored in cache,
    #  it is calculated "on the flight"
    #-----
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    
    # Return the calculated value
    inverse
  }
}
