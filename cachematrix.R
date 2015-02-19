## programming Asssignment 2, Caching the Inverse of a Matrix

## makeCacheMatrix.
# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize vars
  m <- NULL
  # define functions  to pull data from parent environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## cacheSolve.
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
  
  # first check if we have a cached object and matrix hasnt changed.
  m <- x$solve()
  if(!is.null(m)) {
    message("getting cached data")
    # returning cached data
    return(m)
  }
  # otherwize we have to calc inverse via solve and return it
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
