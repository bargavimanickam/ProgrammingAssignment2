## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
