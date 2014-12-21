## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions that other functions can call. It uses a matrix as the input data type.

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


## This function calls the getSolve function and calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
 m <- x$getSolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m       ## Return a matrix that is the inverse of 'x'
}
