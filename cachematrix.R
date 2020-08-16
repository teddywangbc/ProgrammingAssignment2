## Below are two functions that are used to create a special 
## object that stores a matrix and caches its inverse

##The first function, `makeCacheMatrix` creates a special "Matrix" object,
## which is really a list containing a function to
##1.  set the value of the Matrix
##2.  get the value of the Matrix
##3.  set the inverse of the Matrix
##4.  get the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {inv <- NULL
set <- function(y) {
        x <<- y
        inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
