## Set of 2 functions that cache the inverse of a matrix
## Useful if the inverse is used multiple times, don't need to recalculate

## First function - creates an R list that stores a matrix and several functions
## Input is a matrix, output should be assigned to a list variable

makeCacheMatrix <- function(x = matrix()) {    # initialize x and m (x is the matrix)
        m <- NULL                              
        set <- function(y) {                   # four functions that set/get x and m
                x <<- y                        # within the make... environment
                m <<- NULL }                   # instead of the global environment
        get <- function() x                    # using the <<- operator

        setinv <- function(solve) m <<- solve  # defines the solve function to get m
        getinv <- function() m                 # but does not actually calculate m
        
        # returns all the functions as a list with names
        # x and m used by these functions are defined in the same environment
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Second function - takes the list variable from the first function
## Returns matrix m that is the inverse of the original x
## If run multiple times, checks for cached m first before calculating

cacheSolve <- function(x, ...) {
        m <- x$getinv()                        # get the existing m that was defined
                                               # in the make... environment
        if(!is.null(m)) {                      # if it's already a matrix, just return the
                message("getting cached data") # cached m without recalculating
                return(m)
        }
        data <- x$get()                        # otherwise get matrix x and calculate
        m <- solve(data, ...)                  # a new inverse m
        x$setinv(m)
        m                                      # return m
}
