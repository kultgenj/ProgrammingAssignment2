## Thank you for peer assessing!  You are a rockstar!

## makeCacheMatrix creates a special "matrix", which is really a list
## containing functions to:
## set the value of the matrix "set"
## get the value of the matrix "get"
## set the value of the inverse "setsolve"
## get the value of the inverse "getsolve"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list( set = set, get = get,
              setsolve = setsolve, 
              getsolve = getsolve)
}


## The second function calculates the inverse of the matrix of the above 
## function utilizing the "solve" function.
## If the inverse has already been calculated "cacheSolve" will retrieve
## the inverse from the cache and print "getting cached data".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
