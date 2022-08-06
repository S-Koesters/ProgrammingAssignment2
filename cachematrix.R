## Function to cache the Inverse of a Matrix
 
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() {
        	x
    	}
        setInverse <- function(inv) {
        	im <<- inv
    	}
        getInverse <- function() {
        	im
    	}
        list(set = set, get = get,
             setinverse = setInverse,
             getinverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if(!is.null(im)) {
            message("getting cache")
            return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}
