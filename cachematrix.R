## This function will deterine the values assocated w/a Matrix are stored in the cache.
## If the matrix is NOT in cache the a NULL is returned to the calling function
## If the martix IS cached, the solution is looked up in the cached values and returned to the calling function
makeCacheMatrix <- function(x = matrix()) {
  # set result to NULL
  inv <- NULL
  # Set cached values
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x     # Simple get function to return the env var of x
  setinverse <- function(inverse) inv <<- inverse     # Set the inverse in cache
  getinverse <- function() inv                       # Get the cached values for inverse
  # build list of values
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Input Matix for square
## Determine if the Matrix inverse has been cached
## if Yes Print Line stating data from cache & return the cached inverse answer
## if No compute the inverse, call the function to place the values into the cache and return the computed inverse answer
cacheSolve <- function(x, ...) {
  # For Matrix get inverse answer from cache.  NULL is returned if not in cache
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # inverse was in cache.
    message("getting cached data")
    return(inv)
  }
  # Inverse not in cache.  Need to compute inverse
  data <- x$get()
  # Compute Inverse
  inv <- solve(data)
  # Set Inverse in cache
  x$setinverse(inv)
  inv
}

