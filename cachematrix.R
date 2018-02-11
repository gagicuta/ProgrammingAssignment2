## The two functions below are designed to calculate and retrieve 
## cached value for the inverse of a matrix
##This will be done in two steps, defining two functions
##Note: The assumption is that all matrices used are non-singular

## First function, makeCacheMatrix,sets the value of the matrix, 
##gets the value of the matrix, sets the value of the inverse and 
##gets the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function, cacheSolve, calculates the inverse of the 
## matrix created above, checking first if the inverse has already 
## been calculated, skips the calculation and just retrieves it if 
## it's cached, and calculates it otherwise, storing it in the cache 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
  
}
