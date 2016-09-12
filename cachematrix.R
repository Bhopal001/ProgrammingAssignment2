#Here we have a set of 2 functions to make a matrix and cache its inverse.
# A first function call makeCacheMatrix creates the matrix and save the inverse 
# on cache, and a second function, call cacheSolve, to return the inverse of the
# matrix object we created with the first function. This second function (cacheSolve), 
# will calculate the inverse if it is not calculated before and saved on cache. 
# If the inverse is saved on the cache, cacheSolve will return the inverse matrix from cache.

## makeCacheMatrix, function to create a matrix and set its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve, function to return de inverse of the matrix created using makeCacheMatrix().
# The inverse would be returned from cache in case that exist or created and saved on cache, so 
# we can recover it later without the necessity of re-calculate it.

cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}