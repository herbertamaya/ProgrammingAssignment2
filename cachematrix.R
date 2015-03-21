## The following two functions calculate and cache the inverse
## of a given matrix. Caching this calculation increases the
## the performance of the program as matrix inversion is
## usually costly computation.

## This function creates a special 'matrix' object that can cache
## its inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setsolve <-  function(inverse) inv <<- inverse
      getsolve <- function() inv
      list(set=set, get=get,setsolve=setsolve,getsolve=getsolve)
}

## This function computes the inverse of the special 'matrix'
## returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getsolve()
      if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data,...)
      x$setsolve(inverse)
      inverse
}
