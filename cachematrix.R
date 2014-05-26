## The following two functions use caching to optimize calculating
## the inverse of matrices. When the inverse of a matrix is calculated
## for the first time, it's cached and the cached version is used
## for future computations.

## This function creates a new matrix object that supports
## caching the inverse of a matrix to avoid re-computating
## it each time it is needed in the program. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the new matrix object.
## The function first checks if the inverse of the matrix object
## has been already computed. If it has been, it outputs a message
## saying that it got the cached data and returns the cached
## inverse of the matrix. If not, then it calculates the inverse
## and then caches it in memory, so that in the future it won't
## have to be re-computed again.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    print("Getting cached data")
    return (inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(inv)
  inv
}
