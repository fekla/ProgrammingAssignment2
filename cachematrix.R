## Below are two functions that are used to create a special object that stores a numeric matrix
## and cache's its inverse. These fuctions can be used instead of recomputing the inverse which
## is potentially time-consuming.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function returns the inverse of the special "matrix" returned by 
## makeCacheMatrix above. The cachesolve gets the inverse from the cache if it has already been 
## calculated or computes the inverse otherwise.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}