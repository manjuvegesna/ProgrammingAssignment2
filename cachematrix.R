
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  setorig <- function(y) {
          x <<- y
          invrs <<- NULL
  }
  getorig <- function() x
  setinversevalue <- function(inverse) invrs <<- inverse
  getinversevalue <- function() invrs
  list(set = setorig,
       get = getorig,
       setinverse = setinversevalue,
       getinverse = getinversevalue)

}


cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
  if (!is.null(invrs)) {
          message("Inverse is already caluculated before and is cached")
          return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinverse(invrs)
  invrs
}

