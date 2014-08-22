## This file has two methods with the objective of improving the
## performance of repeated inverse matrix calculations.
## On the first time that we calculate the inverse of a matrix,
## we save its inversible matrix. By the second time (and others),
## we use the cached value.

## This function holds other four functions and a variable.
## The variable is initialized as NULL until the first time we
## cache the inverted matrix.
## The other functions allow us to:
## set               - initializes/reinitializes the matrix
## get               - returns the matrix
## setInvertedMatrix - caches the inverted matrix passed as an 
##                     argument into a variable called
##                     cachedInvertedMatrix
## getInvertedMatrix - returns the value of cachedInvertedMatrix
makeCacheMatrix <- function(originalMatrix = matrix()) {
  cachedInvertedMatrix <- NULL
  set <- function(y) {
    originalMatrix <<- y
    cachedInvertedMatrix <<- NULL
  }
  get <- function() originalMatrix
  setInvertedMatrix <- function(invertedMatrix) {
    cachedInvertedMatrix <<- invertedMatrix
  }
  getInvertedMatrix <- function() cachedInvertedMatrix
  list(set = set,
       get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
}

## This function returns the inverted matrix by returning the cached
## one, or if there is no cached matrix, the function will calculate
## it, save it for later and return it.
cacheSolve <- function(x, ...) {
  invertedMatrix <- x$getInvertedMatrix()
  if(!is.null(invertedMatrix)) {
    return(invertedMatrix)
  }
  originalMatrix <- x$get()
  invertedMatrix <- solve(originalMatrix, ...)
  x$setInvertedMatrix(invertedMatrix)
  invertedMatrix
}
