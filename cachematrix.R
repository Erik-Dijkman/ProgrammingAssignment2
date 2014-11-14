## The underneath functions create a matrix, compute the inverse of that matrix
## and cache that inverse.

## The makeCacheMatrix function creates a matrix object. Its 'sub'functions allow 
## you to get or set the matrix's values or those of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve computes, caches and returns a matrix inverse.
## If the inverse has already been calculated then the inverse is being
## retrieved from cache instead.

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
