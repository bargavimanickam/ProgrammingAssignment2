## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse_val <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse_val <<- NULL
  }
  getMatrix <- function() x
  setInverseMatrix <- function(inverse) inverse_val <<- inverse
  getInverseMatrix <- function() inverse_val
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getInverseMatrix()
  if (!is.null(inverse_val)) {
    message("getting cached data of matrix")
    return(inverse_val)
  }
  mat <- x$getMatrix()
  inverse_val <- solve(mat, ...)
  x$setInverseMatrix(inverse_val)
  inverse_val
}
