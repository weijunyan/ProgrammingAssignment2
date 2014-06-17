##  The following pair of function are used to cache the inverse of a matrix.
##Matrix inversion is usually a costly computation and their may be some benefit
##to caching the inverse of a matrix rather than compute it repeatedly.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  A <- NULL
        set <- function(y) {
                x <<- y
                A <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) A <<- inverse
        getinverse <- function() A
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      A <- x$getinverse()
        if(!is.null(A)) {
                message("getting cached data")
                return(A)
        }
        data <- x$get()
        A <- solve(data, ...)
        x$setinverse(A)
        A

}
