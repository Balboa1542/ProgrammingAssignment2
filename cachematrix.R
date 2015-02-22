## The following functions cache and calculate the inverse of a matrix.

## This function makes a special matrix object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y;
    matrixInverse <<- NULL;
  }
  get <- function() return(x);
  setinv <- function(inverse) matrixInverse <<- inverse;
  getinv <- function() return(matrixInverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special matrix returned by the "makeCacheMatrix" function. 
## If the inverse has previously been calculated and the matrix has not changed, then "cacheSolve" should 
## retrieve the inverse from the cache created in "makeCacheMatrix".

cacheSolve <- function(x, ...) {
  matrixInverse <- x$getinv()
  if(!is.null(matrixInverse)) {
    message("Getting cached data...")
    return(matrixInverse)
  }
  newInverse <- x$get()
  matrixInverse <- solve(newInverse, ...)
  x$setinv(matrixInverse)
  return(matrixInverse)
}
