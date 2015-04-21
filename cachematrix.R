## These are functions for making a special type of matrix that is able to cache 
## its inverse so that we don't have to compute it repeatedly

## makeCacheMatrix create a special "matrix" object that can cache its iverse.
## the special matrix is basically a list of functions to
## - set & get the matrix
## - set & get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # i is the cached inverse
  i <- NULL
  # Set the value of the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  # Get the value of the matrix
  get <- function() x
  # Set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  # Get the value of the inverse
  getinverse <- function() i
  
  # Return a list with these set of functions
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## this function return the inverse of the special "matrix" created by makeCacheMatrix
## The function will return the cached inverse if it's already there.
## Otherwise, it will compute the inverse, cache it through setinverse function
## before returning us the inverse.

cacheSolve <- function(x, ...) {
  # Get the inverse from the cache to see if it's already computed
    i <- x$getinverse()
    if(!is.null(i))
    {
      # The inverse is cached so we will return that one
      message("getting cached data")
      return(i)
    }
    # If the inverse hasn't been computed & cache, we will do it before returning it.
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    return(i)
}
