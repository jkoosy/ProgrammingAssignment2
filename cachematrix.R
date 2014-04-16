## Return a list of functions to
## set the value of matrix
## get the value of matrix
## setinvrs set the value of the matrix inverse
## getinvrs get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvrs <- function(invrs) m <<- invrs
        getinvrs <- function() m
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}


## Return a matrix that is the inverse of 'x'
## if inverse has been computed get it from cache
## otherwise compute inverse

cacheSolve <- function(x, ...) {
        m <- x$getinvrs()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinvrs(m)
        m
}
