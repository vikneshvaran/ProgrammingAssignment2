## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions will cache the inverse of the matrix 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    
    set <- function (y) {
        x <<- y
        cached_inverse <<- NULL
    }
    
    get <- function()x
    set_inverse <- function(solve) cached_inverse <<- solve
    get_inverse <- function() cached_inverse
    
    list( set=set, get=get,
          set_inverse = set_inverse,
          get_inverse = get_inverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cached_inverse  <- x$get_inverse()
    if(! is.null(cached_inverse)){
        message("getting cached data")
        return(cached_inverse)
        }

data <- x$get()
cached_inverse <- solve(data, ...)
x$se_inverse (cached_inverse)
cached_inverse

}
