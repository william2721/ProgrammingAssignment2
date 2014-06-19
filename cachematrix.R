## The pair of functions cache the inverse of a matrix so that
## rather than computing inverses repeatedly, the inverse is 
## cached to be used later.

## makeCacheMatrix creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #initialize an empty set for the inverse
  inverse <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the inverse of the matrix
  setinverse <- function(new.inverse) 
    inverse <<- new.inverse
  #get the inverse of the matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## This function computes the inverse of the special "matrix"
## created with makeCacheMatrix. If the inverse has been calculated
## already, then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  #retrieve inverse from the special "matrix"
  inverse <- x$getinverse()
  #if inverse is already calculate, return the existing inversed matrix
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  #solve the inverse of the matrix
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
