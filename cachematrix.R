## These pair of functions are used for calculating the inverse of matrix and caching 
## the inverse for future use
## Sample Usage/Output:
## def <- makeCacheMatrix(rbind(c(1,3,2), c(5,9,3), c(2,8,4)))
## cacheSolve(def)
## Getting Cached Data
## [,1]       [,2]       [,3]
## [1,]  0.8571429  0.2857143 -0.6428571
## [2,] -1.0000000  0.0000000  0.5000000
## [3,]  1.5714286 -0.1428571 -0.4285714


## This function creates a list containing functions to
## set/get the value of matrix and the inverse of the matrix. 
## This function takes a square/non-singular matrix as an input and
## the output is a list

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list (set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## This function is used for calculating the inverse of a square/non-singular matrix 
## if the inverse is not availble in cache else it returns the value from the cache
## The function takes list as an input (output of makeCacheMatrix) and 
## the output is the inverse of the matrix given as input to makecacheMatrix


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting Cached Data")
        return(inv)
    } else {
        mat <- x$get()
        inv <- solve(mat)
        x$setInverse(inv)
        inv
    }
}
