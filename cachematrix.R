## Put comments here that give an overall description of what your
## functions do

## Makes a "special" matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL  ## initialize a NULL value for the cached inverse
     set <- function(y = matrix()) {  ## function to set the matrix value which also sets the cached inverse to NULL
          x <<- y
          i <<- NULL
     }
     get <- function() x ## function to retrieve the matrix
     setinverse <- function(inv) i <<- inv  ## function to set the cached inverse
     getinverse <- function() i  ## function to retrieve the cached inverse
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## return the "special" matrix object
}


## Checks a "special" matrix object to see if it has it's inverse cached and returns the cached inverse if so
## If not, the inverse is calculated and cached and then returned

cacheSolve <- function(x, ...) {
     i <- x$getinverse()  ## retrieve the cached inverse value
     if (!is.null(i)) {  ## if there is a non-NULL cached inverse, then return that value
          message("getting cached data")
          return(i)  ## Return the cached inverse
     }
     data <- x$get()  ## retrieve the matrix
     i <- solve(data) ## solve for the matrix inverse
     x$setinverse(i)  ## set the cached inverse in the "special" matrix object
     i   ## Return a matrix that is the inverse of 'x'
}
