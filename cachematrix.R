## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function create a matrix cache (list type)
# with four functions to set and get a matrix (x)
# and setinv and getinv to set and get the inverse of
# the matrix (xinv).
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  # Set the matrix data
  set <- function(y) {
    x <<- y
    xinv <- NULL
  }
  # Get the matrix data
  get <- function() x
  # Set the inverse of the matrix in the cache
  setinv <- function(y) xinv <<- y
  # Get the inverse of the matrix from the cache
  getinv <- function() xinv
  # Return a list with all the methods
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
# Compute the inverse of a matrix from a makeCacheMatrix object.
# If the inverse is already computed, it return that value and
# print a message. If the inverse has not been calculated, 
# it gets the matrix data and compute the inverse using the 
# solve method. This results is cached under the makeCacheMatrix obj.
cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  # Check if the inverse has been already computed
  if(!is.null(xinv)) {
    message("Getting inverse of x from cache")
    return(xinv)
  }
  # Compute the inverse and store it in the cache
  xdata <- x$get()
  xinv <- solve(xdata)
  x$setinv(xinv)
  xinv
}