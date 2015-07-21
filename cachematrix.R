## These functions cache the inverse of a matrix
## Assumption  - the matrix supplied is always invertible

## This function creates a special "matrix" object that can cache its inverse
## return a list containing functions to
## set/get the matrix
## set/get the inverse

makeCacheMatrix <- function(x = matrix()) {
	  invmat <- NULL
    set <- function(y) {
            x <<- y
            invmat <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invmat <<- inverse
    getinv <- function() invmat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes and return the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
 
    invmat <- x$getinv()
    if(!is.null(invmat)) {
            message("getting cached data")
            return(invmat)
    }
    matdata <- x$get()
    invmat <- solve(matdata, ...)
    x$setinv(invmat)
    invmat
}
