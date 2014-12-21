## Functions are written for R Programming Assignment 2. 
##The below pair of functions cache the inverse of a matrix



## makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
          inverse <- NULL
          
          # set inverse to null
          set <- function(y) {
                    x<<- y
                    inverse <<- NULL
          }
          #get function returns the matrix
          get <- function() x
          
          # function sets the inverse using the variable 'inverse', overriding previous values of 'inverse'
          setinv <-function(solve) inverse <<- solve
          
          # returns the inverse
          getinv <- function() inverse
          
          list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          #retrieves most recent value of inverse
          inverse <- x$getinv()
          
          #returns cached inverse value if not NULL
          if(!is.null(inverse)) {
                    message("getting cached data")
                    return(inverse)
          }
          
          #if inverse value is Null, retrieve matrix x and solve for its inverse
          data <- x$get()
          inverse <- solve(data, ...)
          x$setinv(inverse)
          inverse
}