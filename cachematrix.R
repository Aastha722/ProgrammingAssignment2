# Matrix inversion can be computationally expensive. To improve performance,
# it can be useful to cache the inverse of a matrix instead of recomputing it
# every time it is needed.
#
# The following function, makeCacheMatrix, creates a special object that stores:
# - the matrix itself
# - the cached inverse of the matrix (if already computed)
#
# It provides methods to:
# - set and get the matrix
# - set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset cached inverse when matrix changes
  }
  
  get <- function() x
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  getInverse <- function() {
    inv
  }
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


# The cacheSolve function retrieves the inverse of the matrix stored in the
# special object created by makeCacheMatrix.
#
# - If the inverse has already been computed, it returns the cached value.
# - Otherwise, it computes the inverse, caches it, and then returns it.
#
# Note: This function assumes the matrix is invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Returning cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}
