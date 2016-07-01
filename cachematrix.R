## makeCacheMatrix defines getter and setter functions for the matrix 'x' and its inverse.
## cacheSolve first checks if the cacheMatrix already has a stored inverse. If it does not,
## it calculates and stores the inverse of 'x'.


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) { ## If the inverse of 'x' has already been calculated, return cached data
            message("getting cached data")
            return(inv)
      }
      ## Else, calculate and store the inverse of 'x' in cache
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}
