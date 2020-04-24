## THe following functions work together to create a matrix, access and change 
## values for the matrix, and calculate/cache the inverse for a matrix.

## makeCacheMatrix includes getters and setters for the matrix object itself and 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseCalc) inverse <<- inverseCalc
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve interacts with makeCacheMatrix above to retrieve cached inverses of a
## matrix or to calculate an inverse if a value is not already cached.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
