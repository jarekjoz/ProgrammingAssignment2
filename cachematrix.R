## The two functions below are used in conjunction to compute the inverse matrix of a given matrix.
## In order to reduce the processing time, the inverted matrix is cached.
## If the inverse matrix has been computed before (i.e. cached),
## the function will not compute the matrix -- it will retrieve the cached matrix. 
## This reduces the processing cost.

## The function makes a matrix and caches the results of the inversion

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The function inverts a matrix. 
## It first checks whether the same matrix has already been inverted and cached.
## If it has been inverted and cached already, the function does not compute
## the matrix again and returns the cached result.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
