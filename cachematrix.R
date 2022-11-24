## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below is a pair of functions that cache the inverse of a matrix.

## Creating a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## Set the value of "matrix"
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Get the value of "matrix"
  get <- function() x
  
  ## Set the value of inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## Get the value of inverse
  getinverse <- function() i
  
  # Create a special "matrix" (a list) that cache its inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computing the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## Retrieve the inverse from the cache if it has already been calculated
  if(!is.null(i)) {
    message("getting cached inverse matrix...")
    return(i)
  }
  
  ## Otherwise, compute the inverse of the matrix and sets the value of the 
  ## inverse in the cache via the setinverse function
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
