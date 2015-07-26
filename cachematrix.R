
## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initializing the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse

  ## Method to get the inverse of the matrix
  getinverse <- function() inv
  
  ## Returns the list of methods
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix created by the above function. 
## If the matrix is unchanged, then the cacheSolve function gets the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## get a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## return if the inverse has already been calculated 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## get the matrix from our object
  data <- x$get()
  
  ## calculte the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## storing the inverse to the object to future usage
  x$setInverse(m)
  
  ## returning inverse o/p matrix
  m 
}
