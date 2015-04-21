## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
  
  # Return a matrix with these set of functions
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i))
    {
      message("getting cached data")
      return(i)
    }
    else
    {
      data <- x$get()
      i <- solve(data)
      x$setinverse(i)
      return(i)
    }
}
