## This FUN chache the results of a Solve function called over an invertible
## matrix so, if the matix does not change, the operation is not computed again
## and the result is recovered from the cache
## This firs FUN create a special matrix, set and get the values of the matix 
## and of the solve FUN

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This FUN Solve the invertible matrix and if a solution for the matrix already
## exists the FUN get it from the makeCacheMatrix FUN instead of computing the
## results again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
