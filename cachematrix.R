## These functions cache th einverse of a matrix
## It allows saving computational time for recurring operations

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The  function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## setinv cache the value of the matrix
## getinv reads the cached matrix

makeCacheMatrix <- function(x = matrix()) {
        # declaration of the cache object
        cache <- NULL
        set <- function(y) { ## set the value of the matrix
                x <<- y # pointer of matrix y to x in the parent environment
                cache <<- NULL # Initialization of teh cache in the parent environment
        }
        get <- function() x ## get the value of the matrix
        setinv <- function(solve) { ## setinv cache the value of the matrix
                cache <<- solve 
        }
        getinv <- function() { ## getinv reads the cached matrix
                cache
        }
        list(set = set, # Return a list of operations
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getinv()         # get the cached matrix
        if(!is.null(cache)) {           # return the cached matrix if exists
                message("getting cached data")
                return(cache)
        }
        # as the cache is empty, invert the matrix
        data <- x$get()
        invMatrix <- solve(data)
        x$setinv(invMatrix)
        
        invMatrix # return the inverted matrix
}
