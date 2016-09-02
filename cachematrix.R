## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { ## set the value of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x ## get the value of the matrix
        setInverse <- function(inverse) inv <<- inverse ## set the inverse of the matrix
        getInverse <- function() inv ## get the inverse of the matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## Retrieve the inverse of a matrix from cache if the chache is not null.  i.e.
## if the inverse was previously generaetd and stored in cache by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getInverse() ## Retrieving the inverse
        if (!is.null(inv)) {  ## if the value of inverse is null i.e. not in cache
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()         ## get matrix
        inv <- solve(mat, ...) ## calculates inverse of the matrix
        x$setInverse(inv)      ## set inverse of the matrix
        inv
}
