## Put comments here that give an overall description of what your
## functions do

## Makes a "special" matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y = matrix()) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inv) i <<- inv
     getinverse <- function() i
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Checks a "special" matrix object to see if it has it's inverse cached and returns the cached inverse if so
## If not, the inverse is calculated and cached and then returned

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if (!is.null(i)) {
          message("getting cached data")
          return(i)  ## Return the cached inverse
     }
     data <- x$get()
     i <- solve(data)
     x$setinverse(i)
     i   ## Return a matrix that is the inverse of 'x'
}
