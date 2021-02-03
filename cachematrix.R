# getinv() function from MASS
Library(MASS)
# make matrix 
makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
                    x <<- y
                    inv <<- NULL
          }
          get <- function() x
          setinv <- function(inv) inv <<- inv
          getinv <- function() inv
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}
# get cache data
cacheSolve <- function(x, ...) {
          inv <- x$getinv()
          if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
          }
          data <- x$get()
          inv <- ginv(data, ...)
          x$setinv(inv)
          # return inverse of matrix
          inv 
}
