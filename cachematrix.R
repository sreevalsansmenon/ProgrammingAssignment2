## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setminv <- function(solve) minv <<- solve
  getminv <- function() minv
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- mean(data, ...)
  x$setminv(minv)
  minv
        ## Return a matrix that is the inverse of 'x'
}
