## Date: 2014-06-20
## Author: Me
## Desc: functions to store a matrix and it's inverse.  Will use cached inverse if it exists
##
## Release: 1.0

## Store a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL       ## initialize inverse to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL    ## on each set action, re-initialize inverse to NULL
        }
        get <- function() x   ## retrieve matrix
        setInverse <- function(inverse_matrix) inverse <<- inverse_matrix  ## Set the inverse matrix.  Does not compute it.
        getInverse <- function() inverse      ## retrieve inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computes the inverse of a cached matrix.  Will use cached value if it exists

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$getInverse()  ## retrieving cached data
        if(!is.null(inverse_matrix)) {    ## checking for existence of cached data
                message("using cached data")  ## cached data was already retrieved.  Not actually getting it here.
                return(inverse_matrix)
        }
        data <- x$get()                ## retrieve the matrix
        inverse_matrix <- solve(data, ...)  ## inverse the matrix
        x$setInverse(inverse_matrix)   ## set the inverse matrix value for later reference
        inverse_matrix
}
